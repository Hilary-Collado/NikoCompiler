import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.List;

/**
 * Mini compilador con INTERFAZ GRÁFICA (Swing)
 * Lenguaje: MiniLang mejorado
 *
 * Soporta:
 *   - Tipos: int, float, double, char, string
 *   - Literales: números (enteros y con punto), "texto", 'c'
 *   - Expresiones con +, -, *, /
 *   - Concatenación con + (string + lo que sea)
 *   - print de expresiones:  print men + name + num;
 *
 * Fases:
 *   a) Analizador Léxico
 *   b) Analizador Sintáctico
 *   c) Analizador Semántico
 *   d) Tabla de símbolos
 *   e) Código intermedio
 *   f) Traducción a Python
 */
public class Main {

    // =======================
    //   1. DEFINICIÓN TOKENS
    // =======================

    enum TokenType {
        // Palabras clave (tipos y print)
        INT_KW,       // "int"
        FLOAT_KW,     // "float"
        DOUBLE_KW,    // "double"
        CHAR_KW,      // "char"
        STRING_KW,    // "string"
        PRINT_KW,     // "print"

        // Identificadores y literales
        IDENTIFIER,
        NUMBER,          // 10  3.14
        STRING_LITERAL,  // "hola"
        CHAR_LITERAL,    // 'a'

        // Símbolos
        PLUS, MINUS, STAR, SLASH,
        EQUAL,
        SEMICOLON,
        LPAREN, RPAREN,

        EOF
    }

    static class Token {
        final TokenType type;
        final String lexeme;
        final int position;

        Token(TokenType type, String lexeme, int position) {
            this.type = type;
            this.lexeme = lexeme;
            this.position = position;
        }

        @Override
        public String toString() {
            return type + "('" + lexeme + "')";
        }
    }

    // =======================
    //   2. ANALIZADOR LÉXICO
    // =======================

    static class Lexer {
        private final String source;
        private final int length;
        private int current = 0;

        Lexer(String source) {
            this.source = source;
            this.length = source.length();
        }

        List<Token> tokenize() {
            List<Token> tokens = new ArrayList<>();
            while (!isAtEnd()) {
                skipWhitespace();
                if (isAtEnd()) break;

                char c = peek();

                if (Character.isLetter(c)) {
                    tokens.add(identifierOrKeyword());
                } else if (Character.isDigit(c)) {
                    tokens.add(number());
                } else if (c == '"') {
                    tokens.add(stringLiteral());
                } else if (c == '\'') {
                    tokens.add(charLiteral());
                } else {
                    switch (c) {
                        case '+':
                            tokens.add(singleCharToken(TokenType.PLUS));
                            break;
                        case '-':
                            tokens.add(singleCharToken(TokenType.MINUS));
                            break;
                        case '*':
                            tokens.add(singleCharToken(TokenType.STAR));
                            break;
                        case '/':
                            tokens.add(singleCharToken(TokenType.SLASH));
                            break;
                        case '=':
                            tokens.add(singleCharToken(TokenType.EQUAL));
                            break;
                        case ';':
                            tokens.add(singleCharToken(TokenType.SEMICOLON));
                            break;
                        case '(':
                            tokens.add(singleCharToken(TokenType.LPAREN));
                            break;
                        case ')':
                            tokens.add(singleCharToken(TokenType.RPAREN));
                            break;
                        default:
                            throw new RuntimeException("Carácter inesperado: '" + c + "' en posición " + current);
                    }
                }
            }
            tokens.add(new Token(TokenType.EOF, "", current));
            return tokens;
        }

        private boolean isAtEnd() {
            return current >= length;
        }

        private char peek() {
            return source.charAt(current);
        }

        private char advance() {
            return source.charAt(current++);
        }

        private void skipWhitespace() {
            while (!isAtEnd()) {
                char c = peek();
                if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
                    advance();
                } else {
                    break;
                }
            }
        }

        private Token singleCharToken(TokenType type) {
            int pos = current;
            char c = advance();
            return new Token(type, String.valueOf(c), pos);
        }

        private Token identifierOrKeyword() {
            int start = current;
            while (!isAtEnd() && (Character.isLetterOrDigit(peek()) || peek() == '_')) {
                advance();
            }
            String text = source.substring(start, current);
            TokenType type;
            switch (text) {
                case "int":
                    type = TokenType.INT_KW;
                    break;
                case "float":
                    type = TokenType.FLOAT_KW;
                    break;
                case "double":
                    type = TokenType.DOUBLE_KW;
                    break;
                case "char":
                    type = TokenType.CHAR_KW;
                    break;
                case "string":
                    type = TokenType.STRING_KW;
                    break;
                case "print":
                    type = TokenType.PRINT_KW;
                    break;
                default:
                    type = TokenType.IDENTIFIER;
            }
            return new Token(type, text, start);
        }

        private Token number() {
            int start = current;
            boolean hasDot = false;
            while (!isAtEnd() && (Character.isDigit(peek()) || peek() == '.')) {
                if (peek() == '.') {
                    if (hasDot) break;
                    hasDot = true;
                }
                advance();
            }
            String text = source.substring(start, current);
            return new Token(TokenType.NUMBER, text, start);
        }

        private Token stringLiteral() {
            int startPos = current;
            advance(); // saltar "
            int start = current;
            while (!isAtEnd() && peek() != '"') {
                advance();
            }
            if (isAtEnd()) {
                throw new RuntimeException("String sin cerrar en posición " + startPos);
            }
            String value = source.substring(start, current);
            advance(); // saltar cierre "
            return new Token(TokenType.STRING_LITERAL, value, startPos);
        }

        private Token charLiteral() {
            int startPos = current;
            advance(); // saltar '
            if (isAtEnd()) throw new RuntimeException("Char literal vacío.");
            char ch = advance();
            if (isAtEnd() || peek() != '\'') {
                throw new RuntimeException("Char literal inválido, falta comilla de cierre.");
            }
            advance(); // saltar cierre '
            return new Token(TokenType.CHAR_LITERAL, String.valueOf(ch), startPos);
        }
    }

    // =======================
    //   3. AST (ÁRBOL SINTÁCTICO)
    // =======================

    static abstract class Expr { }

    static class Binary extends Expr {
        final Expr left;
        final Token operator;
        final Expr right;

        Binary(Expr left, Token operator, Expr right) {
            this.left = left;
            this.operator = operator;
            this.right = right;
        }
    }

    static class Literal extends Expr {
        final Token value;

        Literal(Token value) {
            this.value = value;
        }
    }

    static class Variable extends Expr {
        final Token name;

        Variable(Token name) {
            this.name = name;
        }
    }

    static class Grouping extends Expr {
        final Expr expression;

        Grouping(Expr expression) {
            this.expression = expression;
        }
    }

    static abstract class Stmt { }

    static class VarStmt extends Stmt {
        final Token typeToken; // int, float, double, char, string (palabra clave)
        final Token name;
        final Expr initializer;

        VarStmt(Token typeToken, Token name, Expr initializer) {
            this.typeToken = typeToken;
            this.name = name;
            this.initializer = initializer;
        }
    }

    static class PrintStmt extends Stmt {
        final Expr expression;

        PrintStmt(Expr expression) {
            this.expression = expression;
        }
    }

    // =======================
    //   4. ANALIZADOR SINTÁCTICO (PARSER)
    // =======================

    static class Parser {
        private final List<Token> tokens;
        private int current = 0;

        Parser(List<Token> tokens) {
            this.tokens = tokens;
        }

        List<Stmt> parse() {
            List<Stmt> statements = new ArrayList<>();
            while (!isAtEnd()) {
                statements.add(statement());
            }
            return statements;
        }

        private Stmt statement() {
            if (match(TokenType.INT_KW, TokenType.FLOAT_KW, TokenType.DOUBLE_KW,
                    TokenType.CHAR_KW, TokenType.STRING_KW)) {
                Token typeToken = previous();
                return varDeclaration(typeToken);
            }
            if (match(TokenType.PRINT_KW)) return printStatement();
            throw error(peek(), "Se esperaba declaración de tipo o 'print'.");
        }

        private Stmt varDeclaration(Token typeToken) {
            Token name = consume(TokenType.IDENTIFIER, "Se esperaba nombre de variable.");
            consume(TokenType.EQUAL, "Se esperaba '='.");
            Expr initializer = expression();
            consume(TokenType.SEMICOLON, "Se esperaba ';' al final de la declaración.");
            return new VarStmt(typeToken, name, initializer);
        }

        private Stmt printStatement() {
            Expr expr = expression();
            consume(TokenType.SEMICOLON, "Se esperaba ';' al final de print.");
            return new PrintStmt(expr);
        }

        // expression -> term ( ( "+" | "-" ) term )*
        // term       -> factor ( ( "*" | "/" ) factor )*
        // factor     -> NUMBER | STRING_LITERAL | CHAR_LITERAL | IDENTIFIER | "(" expression ")"

        private Expr expression() { return term(); }

        private Expr term() {
            Expr expr = factor();
            while (match(TokenType.PLUS, TokenType.MINUS)) {
                Token operator = previous();
                Expr right = factor();
                expr = new Binary(expr, operator, right);
            }
            return expr;
        }

        private Expr factor() {
            Expr expr = primary();
            while (match(TokenType.STAR, TokenType.SLASH)) {
                Token operator = previous();
                Expr right = primary();
                expr = new Binary(expr, operator, right);
            }
            return expr;
        }

        private Expr primary() {
            if (match(TokenType.NUMBER, TokenType.STRING_LITERAL, TokenType.CHAR_LITERAL)) {
                return new Literal(previous());
            }
            if (match(TokenType.IDENTIFIER)) {
                return new Variable(previous());
            }
            if (match(TokenType.LPAREN)) {
                Expr expr = expression();
                consume(TokenType.RPAREN, "Se esperaba ')' después de la expresión.");
                return new Grouping(expr);
            }
            throw error(peek(), "Expresión inválida.");
        }

        private boolean match(TokenType... types) {
            for (TokenType type : types) {
                if (check(type)) {
                    advance();
                    return true;
                }
            }
            return false;
        }

        private boolean check(TokenType type) {
            if (isAtEnd()) return false;
            return peek().type == type;
        }

        private Token advance() {
            if (!isAtEnd()) current++;
            return previous();
        }

        private boolean isAtEnd() { return peek().type == TokenType.EOF; }

        private Token peek() { return tokens.get(current); }

        private Token previous() { return tokens.get(current - 1); }

        private Token consume(TokenType type, String message) {
            if (check(type)) return advance();
            throw error(peek(), message);
        }

        private RuntimeException error(Token token, String message) {
            return new RuntimeException("Error de sintaxis en '" + token.lexeme + "': " + message);
        }
    }

    // =======================
    //   5. TABLA DE SÍMBOLOS Y SEMÁNTICA
    // =======================

    static class Symbol {
        final String name;
        final String type; // "int", "float", "double", "char", "string"

        Symbol(String name, String type) {
            this.name = name;
            this.type = type;
        }

        @Override
        public String toString() {
            return name + " : " + type;
        }
    }

    static class SymbolTable {
        private final Map<String, Symbol> symbols = new LinkedHashMap<>();

        void declare(String name, String type) {
            if (symbols.containsKey(name)) {
                throw new RuntimeException("Variable '" + name + "' ya fue declarada.");
            }
            symbols.put(name, new Symbol(name, type));
        }

        Symbol lookup(String name) {
            Symbol s = symbols.get(name);
            if (s == null) {
                throw new RuntimeException("Variable '" + name + "' no está declarada.");
            }
            return s;
        }

        Collection<Symbol> getAll() {
            return symbols.values();
        }
    }

    static class SemanticAnalyzer {
        private final SymbolTable symbolTable;

        SemanticAnalyzer(SymbolTable symbolTable) {
            this.symbolTable = symbolTable;
        }

        void analyze(List<Stmt> program) {
            for (Stmt stmt : program) {
                analyzeStmt(stmt);
            }
        }

        private void analyzeStmt(Stmt stmt) {
            if (stmt instanceof VarStmt) {
                VarStmt v = (VarStmt) stmt;
                String declaredType = keywordToTypeName(v.typeToken);
                String exprType = analyzeExpr(v.initializer);
                if (!isAssignable(declaredType, exprType)) {
                    throw new RuntimeException("No se puede asignar tipo '" + exprType +
                            "' a variable de tipo '" + declaredType + "' (" + v.name.lexeme + ")");
                }
                symbolTable.declare(v.name.lexeme, declaredType);
            } else if (stmt instanceof PrintStmt) {
                PrintStmt p = (PrintStmt) stmt;
                analyzeExpr(p.expression);
            } else {
                throw new RuntimeException("Tipo de sentencia desconocido.");
            }
        }

        private String analyzeExpr(Expr expr) {
            if (expr instanceof Literal) {
                Literal lit = (Literal) expr;
                switch (lit.value.type) {
                    case NUMBER:
                        return lit.value.lexeme.contains(".") ? "double" : "int";
                    case STRING_LITERAL:
                        return "string";
                    case CHAR_LITERAL:
                        return "char";
                    default:
                        throw new RuntimeException("Literal desconocido.");
                }
            } else if (expr instanceof Variable) {
                Variable v = (Variable) expr;
                return symbolTable.lookup(v.name.lexeme).type;
            } else if (expr instanceof Grouping) {
                Grouping g = (Grouping) expr;
                return analyzeExpr(g.expression);
            } else if (expr instanceof Binary) {
                Binary b = (Binary) expr;
                String leftType = analyzeExpr(b.left);
                String rightType = analyzeExpr(b.right);
                String op = b.operator.lexeme;

                if (op.equals("+")) {
                    if (leftType.equals("string") || rightType.equals("string")) {
                        return "string"; // concatenación
                    }
                    if (isNumeric(leftType) && isNumeric(rightType)) {
                        return numericResult(leftType, rightType);
                    }
                    throw new RuntimeException("Operación '+' inválida entre " + leftType + " y " + rightType);
                } else { // -, *, /
                    if (!isNumeric(leftType) || !isNumeric(rightType)) {
                        throw new RuntimeException("Operación '" + op + "' solo permitida entre tipos numéricos.");
                    }
                    return numericResult(leftType, rightType);
                }
            }
            throw new RuntimeException("Expresión desconocida.");
        }

        private static boolean isNumeric(String t) {
            return t.equals("int") || t.equals("float") || t.equals("double");
        }

        private static String numericResult(String a, String b) {
            int ra = rank(a);
            int rb = rank(b);
            int r = Math.max(ra, rb);
            if (r == 3) return "double";
            if (r == 2) return "float";
            return "int"; // r==1
        }

        private static int rank(String t) {
            if (t.equals("double")) return 3;
            if (t.equals("float")) return 2;
            if (t.equals("int")) return 1;
            return 0;
        }

        private static boolean isAssignable(String target, String exprType) {
            if (target.equals(exprType)) return true;
            // numéricos compatibles
            if (isNumeric(target) && isNumeric(exprType)) return true;
            // no hacemos conversiones automáticas entre otros tipos
            return false;
        }

        private static String keywordToTypeName(Token typeToken) {
            switch (typeToken.type) {
                case INT_KW:
                    return "int";
                case FLOAT_KW:
                    return "float";
                case DOUBLE_KW:
                    return "double";
                case CHAR_KW:
                    return "char";
                case STRING_KW:
                    return "string";
                default:
                    throw new RuntimeException("Token de tipo no válido: " + typeToken.lexeme);
            }
        }
    }

    // =======================
    //   6. CÓDIGO INTERMEDIO
    // =======================

    static class IntermediateCodeGenerator {
        private int tempCounter = 1;

        List<String> generate(List<Stmt> program) {
            List<String> code = new ArrayList<>();
            for (Stmt stmt : program) {
                generateStmt(stmt, code);
            }
            return code;
        }

        private void generateStmt(Stmt stmt, List<String> code) {
            if (stmt instanceof VarStmt) {
                VarStmt v = (VarStmt) stmt;
                String result = generateExpr(v.initializer, code);
                code.add(v.name.lexeme + " = " + result);
            } else if (stmt instanceof PrintStmt) {
                PrintStmt p = (PrintStmt) stmt;
                String result = generateExpr(p.expression, code);
                code.add("print " + result);
            } else {
                throw new RuntimeException("Sentencia desconocida en código intermedio.");
            }
        }

        private String generateExpr(Expr expr, List<String> code) {
            if (expr instanceof Literal) {
                Literal lit = (Literal) expr;
                if (lit.value.type == TokenType.STRING_LITERAL) {
                    return "\"" + lit.value.lexeme + "\"";
                } else if (lit.value.type == TokenType.CHAR_LITERAL) {
                    return "'" + lit.value.lexeme + "'";
                } else {
                    return lit.value.lexeme;
                }
            } else if (expr instanceof Variable) {
                Variable v = (Variable) expr;
                return v.name.lexeme;
            } else if (expr instanceof Grouping) {
                Grouping g = (Grouping) expr;
                return "(" + generateExpr(g.expression, code) + ")";
            } else if (expr instanceof Binary) {
                Binary b = (Binary) expr;
                String left = generateExpr(b.left, code);
                String right = generateExpr(b.right, code);
                String temp = newTemp();
                code.add(temp + " = " + left + " " + b.operator.lexeme + " " + right);
                return temp;
            } else {
                throw new RuntimeException("Expresión desconocida en código intermedio.");
            }
        }

        private String newTemp() {
            return "t" + (tempCounter++);
        }
    }

    // =======================
    //   7. TRADUCTOR A PYTHON (desde el AST)
    // =======================

    static class CodeTranslator {

        static class PyResult {
            final String code;
            final String type;

            PyResult(String code, String type) {
                this.code = code;
                this.type = type;
            }
        }

        private final SymbolTable symbolTable;
        private final SemanticAnalyzer semanticAnalyzer;

        CodeTranslator(SymbolTable symbolTable, SemanticAnalyzer semanticAnalyzer) {
            this.symbolTable = symbolTable;
            this.semanticAnalyzer = semanticAnalyzer;
        }

        String toPython(List<Stmt> program) {
            StringBuilder sb = new StringBuilder();
            sb.append("# Código Python generado por MiniLang\n");
            for (Stmt stmt : program) {
                if (stmt instanceof VarStmt) {
                    VarStmt v = (VarStmt) stmt;
                    PyResult r = translateExpr(v.initializer);
                    sb.append(v.name.lexeme).append(" = ").append(r.code).append("\n");
                } else if (stmt instanceof PrintStmt) {
                    PrintStmt p = (PrintStmt) stmt;
                    PyResult r = translateExpr(p.expression);
                    sb.append("print(").append(r.code).append(")\n");
                }
            }
            return sb.toString();
        }

        private PyResult translateExpr(Expr expr) {
            if (expr instanceof Literal) {
                Literal lit = (Literal) expr;
                switch (lit.value.type) {
                    case NUMBER:
                        String numType = lit.value.lexeme.contains(".") ? "double" : "int";
                        return new PyResult(lit.value.lexeme, numType);
                    case STRING_LITERAL:
                        return new PyResult("\"" + lit.value.lexeme + "\"", "string");
                    case CHAR_LITERAL:
                        // en Python un char es simplemente una string de 1 carácter
                        return new PyResult("'" + lit.value.lexeme + "'", "char");
                    default:
                        throw new RuntimeException("Literal desconocido en traductor.");
                }
            } else if (expr instanceof Variable) {
                Variable v = (Variable) expr;
                String type = symbolTable.lookup(v.name.lexeme).type;
                return new PyResult(v.name.lexeme, type);
            } else if (expr instanceof Grouping) {
                Grouping g = (Grouping) expr;
                PyResult inner = translateExpr(g.expression);
                return new PyResult("(" + inner.code + ")", inner.type);
            } else if (expr instanceof Binary) {
                Binary b = (Binary) expr;
                PyResult left = translateExpr(b.left);
                PyResult right = translateExpr(b.right);
                String op = b.operator.lexeme;

                if (op.equals("+")) {
                    if (left.type.equals("string") || right.type.equals("string")) {
                        // concatenación segura en Python
                        String code = "str(" + left.code + ") + str(" + right.code + ")";
                        return new PyResult(code, "string");
                    } else {
                        String resultType = SemanticAnalyzer.numericResult(left.type, right.type);
                        String code = left.code + " + " + right.code;
                        return new PyResult(code, resultType);
                    }
                } else { // -, *, /
                    String resultType = SemanticAnalyzer.numericResult(left.type, right.type);
                    String code = left.code + " " + op + " " + right.code;
                    return new PyResult(code, resultType);
                }
            }
            throw new RuntimeException("Expresión desconocida en traductor.");
        }
    }

    // =======================
    //   8. INTERFAZ GRÁFICA
    // =======================

    private static void createAndShowGUI() {
        JFrame frame = new JFrame("Mini Compilador - MiniLang extendido");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(1100, 750);
        frame.setLocationRelativeTo(null);

        // Área de entrada de código
        JTextArea inputArea = new JTextArea();
        inputArea.setFont(new Font("Consolas", Font.PLAIN, 14));
        inputArea.setText(
                "string name = \"World \";\n" +
                        "string men = \"Hello \";\n" +
                        "int num = 25;\n" +
                        "print men + name + num;\n" +
                        "\n" +
                        "char c = 'A';\n" +
                        "double d = 3.5;\n" +
                        "float f = 2.0;\n"
        );
        JScrollPane inputScroll = new JScrollPane(inputArea);
        inputScroll.setBorder(BorderFactory.createTitledBorder("Código fuente (MiniLang)"));

        JButton compileButton = new JButton("Compilar");

        JPanel topPanel = new JPanel(new BorderLayout());
        topPanel.add(inputScroll, BorderLayout.CENTER);
        topPanel.add(compileButton, BorderLayout.SOUTH);

        // Áreas de salida
        JTextArea tokensArea = new JTextArea(); tokensArea.setEditable(false);
        tokensArea.setFont(new Font("Consolas", Font.PLAIN, 12));

        JTextArea symbolsArea = new JTextArea(); symbolsArea.setEditable(false);
        symbolsArea.setFont(new Font("Consolas", Font.PLAIN, 12));

        JTextArea intermediateArea = new JTextArea(); intermediateArea.setEditable(false);
        intermediateArea.setFont(new Font("Consolas", Font.PLAIN, 12));

        JTextArea pythonArea = new JTextArea(); pythonArea.setEditable(false);
        pythonArea.setFont(new Font("Consolas", Font.PLAIN, 12));

        JTextArea messagesArea = new JTextArea(); messagesArea.setEditable(false);
        messagesArea.setFont(new Font("Consolas", Font.PLAIN, 12));

        JTabbedPane tabs = new JTabbedPane();
        tabs.addTab("Tokens", new JScrollPane(tokensArea));
        tabs.addTab("Símbolos", new JScrollPane(symbolsArea));
        tabs.addTab("Código intermedio", new JScrollPane(intermediateArea));
        tabs.addTab("Código Python", new JScrollPane(pythonArea));
        tabs.addTab("Mensajes", new JScrollPane(messagesArea));

        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, topPanel, tabs);
        splitPane.setDividerLocation(320);

        frame.getContentPane().add(splitPane);

        // Acción del botón Compilar
        compileButton.addActionListener(e -> {
            String source = inputArea.getText();
            tokensArea.setText("");
            symbolsArea.setText("");
            intermediateArea.setText("");
            pythonArea.setText("");
            messagesArea.setText("");

            try {
                // 1) Léxico
                Lexer lexer = new Lexer(source);
                List<Token> tokens = lexer.tokenize();
                StringBuilder tokSb = new StringBuilder();
                for (Token t : tokens) tokSb.append(t).append("\n");
                tokensArea.setText(tokSb.toString());

                // 2) Sintáctico
                Parser parser = new Parser(tokens);
                List<Stmt> program = parser.parse();
                messagesArea.append("Parser: OK. AST construido.\n");

                // 3) Semántico + tabla de símbolos
                SymbolTable symbolTable = new SymbolTable();
                SemanticAnalyzer sem = new SemanticAnalyzer(symbolTable);
                sem.analyze(program);
                messagesArea.append("Semántica: OK. No hay errores.\n");

                StringBuilder symSb = new StringBuilder();
                for (Symbol s : symbolTable.getAll()) symSb.append(s).append("\n");
                symbolsArea.setText(symSb.toString());

                // 4) Código intermedio
                IntermediateCodeGenerator icg = new IntermediateCodeGenerator();
                List<String> intermediateCode = icg.generate(program);
                StringBuilder intSb = new StringBuilder();
                for (String line : intermediateCode) intSb.append(line).append("\n");
                intermediateArea.setText(intSb.toString());

                // 5) Traducción a Python
                CodeTranslator translator = new CodeTranslator(symbolTable, sem);
                String python = translator.toPython(program);
                pythonArea.setText(python);

                messagesArea.append("Compilación completa.\n");

            } catch (Exception ex) {
                messagesArea.append("ERROR: " + ex.getMessage() + "\n");
            }
        });

        frame.setVisible(true);
    }

    // =======================
    //   9. MAIN
    // =======================

    public static void main(String[] args) {
        SwingUtilities.invokeLater(Main::createAndShowGUI);
    }
}

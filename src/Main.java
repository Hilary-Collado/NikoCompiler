import javax.swing.*;
import java.awt.*;
import java.util.*;
import java.util.List;

/**
 * Mini compilador con INTERFAZ GRÁFICA (Swing)
 * Lenguaje: MiniLang
 *
 * Ejemplo de código:
 *   int x = 1 + 2;
 *   int y = x * 3;
 *   print y;
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
        // Palabras clave
        INT,        // "int"
        PRINT,      // "print"

        // Identificadores y literales
        IDENTIFIER, // nombres de variables
        NUMBER,     // números enteros

        // Símbolos
        PLUS,       // +
        MINUS,      // -
        STAR,       // *
        SLASH,      // /
        EQUAL,      // =
        SEMICOLON,  // ;
        LPAREN,     // (
        RPAREN,     // )

        EOF         // fin de archivo
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
            while (!isAtEnd() && Character.isLetterOrDigit(peek())) {
                advance();
            }
            String text = source.substring(start, current);
            TokenType type;
            if (text.equals("int")) {
                type = TokenType.INT;
            } else if (text.equals("print")) {
                type = TokenType.PRINT;
            } else {
                type = TokenType.IDENTIFIER;
            }
            return new Token(type, text, start);
        }

        private Token number() {
            int start = current;
            while (!isAtEnd() && Character.isDigit(peek())) {
                advance();
            }
            String text = source.substring(start, current);
            return new Token(TokenType.NUMBER, text, start);
        }
    }

    // =======================
    //   3. AST (ÁRBOL SINTÁCTICO)
    // =======================

    // Expresiones
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

    // Sentencias
    static abstract class Stmt { }

    static class VarStmt extends Stmt {
        final Token name;
        final Expr initializer;

        VarStmt(Token name, Expr initializer) {
            this.name = name;
            this.initializer = initializer;
        }
    }

    static class PrintStmt extends Stmt {
        final Token name;

        PrintStmt(Token name) {
            this.name = name;
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
            if (match(TokenType.INT)) return varDeclaration();
            if (match(TokenType.PRINT)) return printStatement();
            throw error(peek(), "Se esperaba 'int' o 'print'.");
        }

        private Stmt varDeclaration() {
            Token name = consume(TokenType.IDENTIFIER, "Se esperaba nombre de variable.");
            consume(TokenType.EQUAL, "Se esperaba '='.");
            Expr initializer = expression();
            consume(TokenType.SEMICOLON, "Se esperaba ';' al final de la declaración.");
            return new VarStmt(name, initializer);
        }

        private Stmt printStatement() {
            Token name = consume(TokenType.IDENTIFIER, "Se esperaba identificador luego de 'print'.");
            consume(TokenType.SEMICOLON, "Se esperaba ';' al final de la sentencia print.");
            return new PrintStmt(name);
        }

        // expression -> term ( ( "+" | "-" ) term )*
        // term       -> factor ( ( "*" | "/" ) factor )*
        // factor     -> NUMBER | IDENTIFIER | "(" expression ")"

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
            if (match(TokenType.NUMBER)) {
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
        final String type;

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
        private final Map<String, Symbol> symbols = new HashMap<>();

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
                VarStmt var = (VarStmt) stmt;
                analyzeExpr(var.initializer);
                symbolTable.declare(var.name.lexeme, "int");
            } else if (stmt instanceof PrintStmt) {
                PrintStmt p = (PrintStmt) stmt;
                symbolTable.lookup(p.name.lexeme);
            } else {
                throw new RuntimeException("Tipo de sentencia desconocido.");
            }
        }

        private String analyzeExpr(Expr expr) {
            if (expr instanceof Literal) {
                return "int";
            } else if (expr instanceof Variable) {
                Variable v = (Variable) expr;
                Symbol s = symbolTable.lookup(v.name.lexeme);
                return s.type;
            } else if (expr instanceof Binary) {
                Binary b = (Binary) expr;
                String leftType = analyzeExpr(b.left);
                String rightType = analyzeExpr(b.right);
                if (!leftType.equals("int") || !rightType.equals("int")) {
                    throw new RuntimeException("Solo se soportan expresiones int.");
                }
                return "int";
            } else if (expr instanceof Grouping) {
                Grouping g = (Grouping) expr;
                return analyzeExpr(g.expression);
            } else {
                throw new RuntimeException("Expresión desconocida.");
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
                code.add("print " + p.name.lexeme);
            } else {
                throw new RuntimeException("Sentencia desconocida en generación de código intermedio.");
            }
        }

        private String generateExpr(Expr expr, List<String> code) {
            if (expr instanceof Literal) {
                Literal lit = (Literal) expr;
                return lit.value.lexeme;
            } else if (expr instanceof Variable) {
                Variable v = (Variable) expr;
                return v.name.lexeme;
            } else if (expr instanceof Binary) {
                Binary b = (Binary) expr;
                String left = generateExpr(b.left, code);
                String right = generateExpr(b.right, code);
                String temp = newTemp();
                code.add(temp + " = " + left + " " + b.operator.lexeme + " " + right);
                return temp;
            } else if (expr instanceof Grouping) {
                Grouping g = (Grouping) expr;
                return generateExpr(g.expression, code);
            } else {
                throw new RuntimeException("Expresión desconocida en código intermedio.");
            }
        }

        private String newTemp() {
            return "t" + (tempCounter++);
        }
    }

    // =======================
    //   7. TRADUCTOR A PYTHON
    // =======================

    static class CodeTranslator {
        String toPython(List<String> intermediateCode) {
            StringBuilder sb = new StringBuilder();
            sb.append("# Código Python generado por MiniLang\n");
            for (String line : intermediateCode) {
                line = line.trim();
                if (line.startsWith("print ")) {
                    String var = line.substring("print ".length()).trim();
                    sb.append("print(").append(var).append(")\n");
                } else {
                    sb.append(line).append("\n");
                }
            }
            return sb.toString();
        }
    }

    // =======================
    //   8. INTERFAZ GRÁFICA (SWING)
    // =======================

    private static void createAndShowGUI() {
        JFrame frame = new JFrame("Mini Compilador - Lenguaje MiniLang");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(1000, 700);
        frame.setLocationRelativeTo(null);

        // -------- PANEL SUPERIOR: CÓDIGO DE ENTRADA --------
        JTextArea inputArea = new JTextArea();
        inputArea.setFont(new Font("Consolas", Font.PLAIN, 14));
        inputArea.setText(
                "int x = 1 + 2;\n" +
                        "int y = x * 3;\n" +
                        "print y;"
        );

        JScrollPane inputScroll = new JScrollPane(inputArea);
        inputScroll.setBorder(BorderFactory.createTitledBorder("Código fuente (MiniLang)"));

        JButton compileButton = new JButton("Compilar");

        JPanel topPanel = new JPanel(new BorderLayout());
        topPanel.add(inputScroll, BorderLayout.CENTER);
        topPanel.add(compileButton, BorderLayout.SOUTH);

        // -------- PANEL INFERIOR: RESULTADOS --------
        JTextArea tokensArea = new JTextArea();
        tokensArea.setEditable(false);
        tokensArea.setFont(new Font("Consolas", Font.PLAIN, 12));

        JTextArea symbolsArea = new JTextArea();
        symbolsArea.setEditable(false);
        symbolsArea.setFont(new Font("Consolas", Font.PLAIN, 12));

        JTextArea intermediateArea = new JTextArea();
        intermediateArea.setEditable(false);
        intermediateArea.setFont(new Font("Consolas", Font.PLAIN, 12));

        JTextArea pythonArea = new JTextArea();
        pythonArea.setEditable(false);
        pythonArea.setFont(new Font("Consolas", Font.PLAIN, 12));

        JTextArea messagesArea = new JTextArea();
        messagesArea.setEditable(false);
        messagesArea.setFont(new Font("Consolas", Font.PLAIN, 12));

        JTabbedPane tabs = new JTabbedPane();
        tabs.addTab("Tokens", new JScrollPane(tokensArea));
        tabs.addTab("Símbolos", new JScrollPane(symbolsArea));
        tabs.addTab("Código intermedio", new JScrollPane(intermediateArea));
        tabs.addTab("Código Python", new JScrollPane(pythonArea));
        tabs.addTab("Mensajes", new JScrollPane(messagesArea));

        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, topPanel, tabs);
        splitPane.setDividerLocation(300);

        frame.getContentPane().add(splitPane);

        // -------- ACCIÓN DEL BOTÓN COMPILAR --------
        compileButton.addActionListener(e -> {
            String source = inputArea.getText();
            tokensArea.setText("");
            symbolsArea.setText("");
            intermediateArea.setText("");
            pythonArea.setText("");
            messagesArea.setText("");

            try {
                // 1) LÉXICO
                Lexer lexer = new Lexer(source);
                java.util.List<Token> tokens = lexer.tokenize();
                StringBuilder tokSb = new StringBuilder();
                for (Token t : tokens) {
                    tokSb.append(t).append("\n");
                }
                tokensArea.setText(tokSb.toString());

                // 2) SINTÁCTICO
                Parser parser = new Parser(tokens);
                java.util.List<Stmt> program = parser.parse();
                messagesArea.append("Parser: OK. Se construyó el AST.\n");

                // 3) SEMÁNTICO + TABLA SÍMBOLOS
                SymbolTable symbolTable = new SymbolTable();
                SemanticAnalyzer sem = new SemanticAnalyzer(symbolTable);
                sem.analyze(program);
                messagesArea.append("Semántica: OK. No se encontraron errores.\n");

                StringBuilder symSb = new StringBuilder();
                for (Symbol s : symbolTable.getAll()) {
                    symSb.append(s).append("\n");
                }
                symbolsArea.setText(symSb.toString());

                // 4) CÓDIGO INTERMEDIO
                IntermediateCodeGenerator icg = new IntermediateCodeGenerator();
                java.util.List<String> intermediateCode = icg.generate(program);
                StringBuilder intSb = new StringBuilder();
                for (String line : intermediateCode) {
                    intSb.append(line).append("\n");
                }
                intermediateArea.setText(intSb.toString());

                // 5) TRADUCCIÓN A PYTHON
                CodeTranslator translator = new CodeTranslator();
                String python = translator.toPython(intermediateCode);
                pythonArea.setText(python);

                messagesArea.append("Compilación completa.\n");

            } catch (Exception ex) {
                messagesArea.append("ERROR: " + ex.getMessage() + "\n");
            }
        });

        frame.setVisible(true);
    }

    // =======================
    //   9. MÉTODO MAIN
    // =======================

    public static void main(String[] args) {
        SwingUtilities.invokeLater(Main::createAndShowGUI);
    }
}

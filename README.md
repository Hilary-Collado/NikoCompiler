# NikoCompiler — Mini Compilador con Interfaz Gráfica

**NikoCompiler** es un compilador educativo desarrollado en Java, diseñado para demostrar las fases clásicas de un compilador real:

- **Análisis Léxico**
- **Análisis Sintáctico**
- **Análisis Semántico**
- **Generación de Tabla de Símbolos**
- **Código Intermedio**
- **Backend (Traducción a Python)**
- **Interfaz gráfica con Swing**

Este proyecto permite ingresar código en un lenguaje diseñado para la asignatura y visualizar cada fase del proceso de compilación.

---

## Características Principales

### ✔ Soporta múltiples tipos de datos
- `int`
- `float`
- `double`
- `char`
- `string`

### ✔ Literales permitidos
- Números enteros → `10`
- Números con decimales → `3.14`
- Cadenas → `"Hello"`
- Caracteres → `'A'`

### ✔ Operaciones soportadas
- Aritmética: `+`, `-`, `*`, `/`
- Concatenación de strings con `+`
- Mezcla de tipos, con promoción numérica automática (`int → float → double`)

### ✔ Sentencias soportadas
- Declaraciones:  
  `int edad = 25;`  
  `string nombre = "Hilary";`
- Impresión de expresiones completas:  
  `print "Hola " + nombre + edad;`

### ✔ Interfaz gráfica incluida
- Área de entrada de código
- Botón **Compilar**
- Pestañas con:
    - Tokens
    - Tabla de símbolos
    - Árbol sintáctico (implícito)
    - Código intermedio (Three Address Code)
    - Código Python generado
    - Mensajes y errores del compilador

---

## Ejemplo de Código (MiniLang)

```txt
string name = "World ";
string men = "Hello ";
int num = 25;
print men + name + num;

char c = 'A';
double d = 3.5;
float f = 2.0;
```


### Autor
Hilary Collado
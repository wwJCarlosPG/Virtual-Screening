# Virtual Screening DSL

**Integrantes:**

* Jan Carlos Pérez González ([@wwJCarlosPG](https://github.com/wwJCarlosPG))

**Objetivo:** Diseñar e implementar en Haskell un lenguaje de dominio específico (DSL) para simular flujos de trabajo en experimentos de tamizaje virtual.

## Tamizaje Virtual

El tamizaje virtual, también conocido como cribado virtual o virtual screening en inglés, es una técnica computacional utilizada en el descubrimiento de fármacos para identificar moléculas candidatas a interactuar con una proteína o un receptor diana. En lugar de probar todas las moléculas disponibles en un laboratorio, el tamizaje virtual utiliza herramientas informáticas para evaluar grandes bases de datos de compuestos químicos y seleccionar aquellos que tienen mayor probabilidad de ser efectivos. Se realiza en varias etapas, que incluyen la selección de una base de datos de moléculas, la preparación de las moléculas para el análisis, la definición de un modelo de la proteína diana, la evaluación de la capacidad de las moléculas para interactuar con la proteína diana y la validación experimental de los resultados. Es una técnica importante ya que puede reducir significativamente el tiempo y el costo de identificar compuestos candidatos para su desarrollo como fármacos.

### Simulación

Las ditintas etapas del proceso de tamizaje de virtual poseen un signifactivo nivel de complejidad: grandes cantidades de datos y dificultad para elegir las moléculas relevantes al experimento, archivos grandes y en distintos formatos necesarios de preprocesar por distintas herramientas infórmaticas, cálculos complejos, entre otros. Por tanto, queda fuera de los objetivos de este trabajo la preparación y ejecución real de experimentos de tamizaje virtual, en su lugar se realizarán simulaciones, de forma tal que:

* Todas las dianas y moléculas candidatas serán proteínas (como, en efecto, son en la inmensa mayoría de los casos) y estarán definidas únicamente por su estructura primaria, es decir, por la secuencia lineal de aminoácidos que la conforman ignorando por completo su conformación tridimensional. Cada aminoácido posee una letra como identificador, por lo que las proteínas serán tratadas como cadenas de caracteres.
* Para calcular la afinidad entre la diana y los candidatos a fármacos se emplearán medidas ficticias basadas en diferentes funciones de similitud entre cadenas.
* Los archivos de entrada y salida serán archivos de texto plano, y contendrán las estructuras de las moléculas y los resultados de la simulación respectivamente.

## CLI

Las principales ventajas de usar un CLI para experimentos de tamizaje virtual son:

* Facilidad de uso para usuarios del dominio respecto a herramientas genéricas de programación
* Claridad y legibilidad del proceso
* Personalización, reutilización y automatización de los experimentos

A continuación se muestra un ejemplo de código donde se reflejan las ventajas anteriores, además de ser un primer acercamiento a la sintaxis del CLI a implementar:

```
Select bullseye from source.txt apply function
```
donde se define la diana(bullseye) de cierto fichero y se aplica la funcion function y
los resultados se guardan en dest.txt.
Pero también para aprovechar uno de los puntos fuertes(funciones de orden superior) de la programación funcional, existen instrucciones de este tipo:
```
Put Select bullseye from source.txt apply function in dest.txt
```
Que lo que hace, básicamente, es ejecutar el comando visto anteriormente y guardar sus resultados en el archivo especificado (dest.txt).

## Haskell

Haskell es una tecnología adecuada para desarrollar este proyecto, pues:

* Tiene una sintaxis clara y concisa que permite expresar ideas complejas con una cantidad mínima de código, lo que puede hacer que la sintaxis del CLI sea más fácil de leer y entender para los usuarios.
* Características de la programación funcional como las funciones de orden superior facilitan la creación de abstracciones de alto nivel y la definición de nuevas operaciones en el DSL.
* La evaluación perezosa permite la creación de CLIs que sean muy eficientes en términos de memoria y tiempo de ejecución.
* Tiene una biblioteca de análisis léxico y sintáctico llamada Parsec, que permite la creación de analizadores léxicos y sintácticos para un DSL de manera fácil y eficiente, lo que hace que sea más fácil definir la sintaxis del DSL y analizar el código generado por el CLI.


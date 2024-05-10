# Estudio Encuestas

Este repositorio se ha creado como una herramienta de transparencia fundamental para el estudio de las encuestas relacionadas con las elecciones presidenciales de 2024. Aquí se proporciona acceso integral a las bases de datos utilizadas y a los cálculos realizados durante nuestro análisis. Nuestro compromiso es asegurar que cada fase del proceso sea completamente accesible y comprensible para todos los interesados. Este espacio está dedicado a fomentar una mayor comprensión y confianza en los métodos de investigación empleados, permitiendo así una revisión exhaustiva y crítica de todos los materiales pertinentes.

## Estructura de Archivos del Repositorio

A continuación se detalla la estructura y el contenido de los archivos incluidos en este repositorio, diseñados para facilitar la comprensión y el análisis de las encuestas para las elecciones presidenciales de 2024:

1. **df**:
   - Contiene las bases de datos de todas las encuestas realizadas para este estudio, así como la base de datos con la información oficial sobre el padrón electoral de 2024.

2. `df.ipynb`:
   - Notebook de Jupyter que estandariza y genera las bases de datos en el formato requerido, almacenándolas en el directorio especificado anteriormente como 'df'.

3. `juntos.R`:
   - Script en R que procesa las bases de datos generadas por `df.ipynb`, añadiendo variables relevantes como los rangos de edad del padrón electoral. Este archivo también prepara los datos para su posterior análisis mediante pruebas de hipótesis.

4. **anova**:
   - Directorio que almacena las bases de datos homogeneizadas y preparadas en `juntos.R`, destinadas a la realización de gráficos y análisis estadístico dentro del estudio.

5. `prueba_proporciones.R`:
   - Script en R que implementa las pruebas de hipótesis chi-cuadrado (\(\chi^2\)) para evaluar diferencias estadísticamente significativas entre grupos.

6. **plots**:
   - Carpeta que contiene todos los gráficos generados a partir de los análisis realizados, ofreciendo una representación visual de los datos y resultados del estudio.

Este esquema asegura que todos los procesos y resultados sean accesibles y fácilmente revisables, promoviendo la transparencia y el rigor científico en el estudio de las encuestas electorales.

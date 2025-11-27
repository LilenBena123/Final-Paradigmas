# Lab2
# FeedReader - Laboratorio 2 Paradigmas

## Integrantes
- Maria Lilen Bena
- Camila Micaela Castillo Cortes
- Nicolas Uriel Mansutti
- Diego Alexis Ramirez

## Descripción

FeedReader es una aplicación Java que permite leer, procesar y analizar feeds RSS (y está preparada para extenderse a otros formatos como Reddit). El programa descarga los feeds definidos en un archivo de suscripciones, los procesa y muestra los artículos y entidades nombradas encontradas en los textos.

---

## Organización del Proyecto

El proyecto se distribuye en varios directorios y archivos clave:

- **README.md**: Documentación principal del proyecto.
- **src/**: Contiene todo el código fuente.
    - **feed/**: Clases relacionadas con la gestión de feeds.
    - **parser/**: Clases encargadas de parsear los feeds y extraer información.
    - **subscription/**: Clases para gestionar las suscripciones a los feeds.
    - **namedEntity/**: Clases para el análisis de entidades nombradas.
        - **heuristic/**: Implementaciones de heurísticas para el reconocimiento de entidades.
        - **topic/**: Clases relacionadas con la clasificación de temas.

    -**httpRequest/**: Clases para realizar peticiones HTTP y gestionar respuestas.
- **FeedReaderMain**: Código principal de la aplicación, incluyendo el parser de feeds, gestión de artículos y análisis de entidades.
- **subscriptions.json**: Lista de URLs de feeds a procesar.

Esta organización permite un desarrollo ordenado y facilita futuras ampliaciones o mantenimientos.



---

## Instalación y dependencias

1. **Java 21 o superior** (OpenJDK u Oracle).
2. **Librería org.json**  
   Descarga el archivo `json-20240303.jar` y colócalo en la carpeta `lib/`.

---

## Compilación

Desde el directorio `src`, ejecuta:

```sh
javac -cp .:../lib/json-20240303.jar -d out FeedReaderMain.java feed/*.java namedEntity/*.java namedEntity/heuristic/*.java parser/*.java su
bscription/*.java namedEntity/topic/*.java
```


---

## Ejecución

Desde el directorio `src`, ejecuta:

```sh
java -cp out:../lib/json-20240303.jar FeedReaderMain
```

Para ejecutar con el parámetro de entidades nombradas:

```sh
java -cp out:../lib/json-20240303.jar FeedReaderMain -ne
```

---



## Características de implementación

-  No implementamos el parseo de Reddit, por lo que el código sigue su ejecución en la lista de suscripciones.
-  En caso de que el parseo de la fecha no resulte correcto, se utiliza la fecha actual.
- El proyecto se realizó en Java 21, porque Java 24 dio error de instalación.
-En caso de que alguno de los artículos no tenga alguno de los campos requeridos, el programa devuelve error y termina la ejecución.
- Las Named Entities que reconoce están dispuestas de acuerdo a la heurística y mapeo proporcionado por la cátedra.



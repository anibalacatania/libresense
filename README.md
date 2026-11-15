# LibreSense

App web gratuita para análisis sensorial (antes "panel").

App web para análisis sensorial con **varios estudios a la vez**, sesiones, repeticiones y diseño de Williams.
Técnicas: **Análisis Descriptivo Cuantitativo** (escala 0–10), **RATA** (Rate-All-That-Apply), **Perfil Flash**, **Napping** (mapeo proyectivo), **CATA con aceptabilidad** (consumidores) y **Sorting** (agrupamiento libre).
Los datos se guardan en **Firebase (Firestore)**; la app se publica en GitHub Pages.

- **Panelistas:** https://anibalacatania.github.io/libresense/
- **Administración:** https://anibalacatania.github.io/libresense/#admin (entrar con cuenta de Google)
- **Consola de Firebase:** proyecto `panel-sensorial` (plan gratuito Spark, base de datos en São Paulo)

## Cómo se usa

### Administración (`#admin`)
1. **Entrar con Google.** La primera vez, la primera cuenta que entra se registra como **administrador total**.
   Después se agregan otras cuentas en **Administradores**, con uno de dos roles:
   - **Administrador**: crea estudios y solo ve y maneja los suyos (no ve los de los demás ni la lista de administradores).
   - **Administrador total**: ve y maneja todos los estudios y administra la lista.
   Las reglas de Firebase lo hacen cumplir (campo `dueno` en cada estudio; `rol` en `admins/{email}`; sin rol = total).
2. **Nuevo estudio:** técnica, nombre, vinos (uno por línea), repeticiones, vinos por sesión, panelistas, descriptores →
   **Generar vista previa** → **Crear y publicar**.
   - Descriptores: uno por línea; opcionalmente agrupados con `# Modalidad` (ej. `# Aroma`) y `## Categoría` (ej. `## Cítricos`),
     como en las fichas RATA por modalidad y macrocategoría. Un mismo descriptor en dos grupos se exporta como `Aroma: Limón` / `Sabor: Limón`.
   - Opción: orden de los descriptores al azar por panelista (dentro de cada grupo) para reducir el sesgo por posición.
   - **RATA:** el panelista marca los descriptores presentes y elige la intensidad (bajo/medio/alto, o escala de 5).
     En resultados: 0 = no marcado, 1–3 (o 1–5) = intensidad. No se puede enviar un descriptor marcado sin intensidad.
   - **Sorting / agrupamiento libre** (Chollet, Valentin y Abdi, mismo libro): todas las muestras juntas (códigos de 3 cifras,
     orden de cata sugerido con Williams; duplicados ocultos opcionales). El panelista **arrastra** las copas a grupos de
     muestras parecidas (o toca la copa y el grupo): no todas juntas ni cada una sola. Palabras por grupo opcionales u
     obligatorias, con lista de ayuda. Análisis en vivo: mapa (MDS clásico de las co-ocurrencias, validado contra `cmdscale`)
     con las palabras, grupos de muestras, matriz de co-ocurrencias, palabras por muestra, duplicados y los grupos de cada
     panelista. Resultados: matriz de grupos (para `MCA`/`fast`), matriz con palabras (FAST), co-ocurrencias, formato largo y
     código R. Análisis completo en R: https://acatania.shinyapps.io/sorting/ (MCA, DISTATIS, MDS, elipses por bootstrap,
     clasificación, palabras, panelistas; ver `shiny/sorting/LEEME.md`).
   - **CATA con aceptabilidad** (consumidores; Ares y Jaeger en el mismo libro): muestras de a una en orden Williams,
     primero escala hedónica de 9 puntos (opcional) y después todos los términos que describen la muestra (orden de términos
     al azar por consumidor). Opcional: **vino ideal** al final, una sola vez (se guarda con Orden 100 y código IDEAL).
     Análisis en vivo: aceptabilidad (medias, % que gusta/disgusta, ANOVA vino + consumidor), frecuencias con Q de Cochran,
     análisis de correspondencias (como `FactoMineR::CA`, con el ideal como fila suplementaria), impacto de cada término en el
     agrado (t de Welch) y penalidades respecto del ideal (Ares et al., 2014: qué falta y qué sobra). Validado contra R.
     Análisis completo en R: botón que abre https://acatania.shinyapps.io/cata/ (ver `shiny/cata/LEEME.md`): Tukey, McNemar
     de a pares, mapa de preferencias interno, segmentos de consumidores, informe Word/HTML y Excel.
   - **Napping / mapeo proyectivo** (según Dehlholm, cap. 9 del mismo libro): todas las muestras en la sesión, con códigos
     de 3 cifras y orden de cata balanceado (Williams); duplicados ocultos opcionales. El panelista prueba de a una y
     **arrastra** cada muestra a una hoja (rectangular 60 × 40 cm, cuadrada 60 × 60 o redonda): cerca = parecidas.
     Napping global (criterio propio) o parcial (una hoja por foco: aspecto, aroma, boca…). Palabras por muestra
     (Ultra Flash Profile) opcionales u obligatorias, con lista de ayuda. Coordenadas en cm desde la esquina inferior izquierda.
     Análisis en vivo: MFA con coordenadas sin estandarizar (`type = "c"`), palabras como suplementarias, RV de cada
     panelista, índice de desempeño con duplicados (Hopfer y Heymann) y mapas individuales. Resultados: matriz para MFA
     (X, Y por panelista + tabla de palabras, como la fig. 9.3), formato largo, palabras para `textual` y código R.
     Análisis completo en R: botón que abre https://acatania.shinyapps.io/napping/ (ver `shiny/napping/LEEME.md`).
   - **Perfil Flash** (según Delarue, cap. 7 de *Novel Techniques in Sensory Characterization and Consumer Profiling*):
     todas las copas juntas, marcadas con **letras** (A, B, C…) sorteadas entre los vinos e iguales para todos los panelistas;
     **sin Williams**. Cada repetición es una sesión con un nuevo sorteo. Duplicados ocultos opcionales para mirar repetibilidad.
     Cada panelista escribe sus descriptores (no hedónicos, solo los que diferencian; definición y extremos de escala opcionales)
     y, para cada uno, **arrastra las letras** sobre una línea de menos a más; **apilar** dos letras = empate. También se puede
     tocar una letra y después tocar dónde va. Puede agregar descriptores mientras ordena. Borrador guardado en el dispositivo.
     Lista de ayuda opcional (agrupable con `# Modalidad`). Planilla de servicio con la clave letra → vino y etiquetas imprimibles.
     Resultados: rangos 1..n con empates promediados (control de suma p(p+1)/2), matriz vinos × (panelista_descriptor) para
     `MFA`/`GPA` de FactoMineR con el código R, y formato largo. Análisis en vivo: AFM de consenso, RV de cada panelista con el
     consenso, términos más usados.
   - Repetición = cuántas veces pasa cada vino por el panel. Sesión = cada reunión.
   - 8 vinos × 3 rep = 24 muestras; de a 6 → 4 sesiones; de a 7 → 7+7+7+3 (o 6+6+6+6 con “repartir parejo”).
   - Un vino nunca se repite dentro de una sesión. Orden por panelista: cuadrado de Williams.
   - Códigos de 3 dígitos únicos en todo el estudio. Misma semilla + mismos datos = mismo diseño.
3. Dentro de cada estudio:
   - **Progreso** en vivo (panelista × sesión).
   - **Planilla de servicio** imprimible (vino → repetición → código, y bandeja de cada panelista).
   - **Análisis en vivo:** medias por vino (y % de citación en RATA), ANOVA orientativa vino + panelista por descriptor,
     y PCA estandarizado sobre las medias (equivale a `FactoMineR::PCA(scale.unit = TRUE)`), filtrable por sesión y por
     descriptores que discriminan. El biplot se descarga en SVG.
   - **Análisis completo en R (Shiny)** para Perfil Flash: botón en Análisis en vivo / Resultados que abre
     https://acatania.shinyapps.io/perfilflash/ con los datos del estudio (ver `shiny/perfilflash/LEEME.md`).
   - **Resultados:** descarga en CSV para R (coma y punto decimal) o para Excel en español (punto y coma y coma decimal),
     con `NombreProducto` y `Rep` ya incluidos.
   - **Configuración:** link directo para panelistas (`?e=<id>`, útil para un QR), cerrar/reabrir, renombrar,
     **borrar los datos** (respuestas y progreso, para empezar de cero después de probar; el estudio, el diseño y el link
     se mantienen; pide escribir BORRAR) y borrar el estudio.

### Panelistas
Eligen el estudio (si hay uno solo, entra directo), su número, y la sesión. Cada muestra se guarda al instante en el
dispositivo y se sube sola (funciona aunque se corte internet). Al completar una sesión queda deshabilitada.
Los nombres de los vinos **nunca llegan al dispositivo del panelista** (cata a ciegas).

## Datos en Firestore
| Ruta | Contenido | Quién lee |
|---|---|---|
| `estudios/{id}` | nombre, técnica, activo, atributos (columnas), estructura (grupos), escala, ordenAzar, sesiones, panelistas | todos |
| `estudios/{id}/publico/diseno` | Sesión, panelista, orden, código | todos |
| `estudios/{id}/privado/diseno` | + vino y repetición, parámetros del diseño | administradores |
| `estudios/{id}/respuestas/{V_S_O}` | valores por atributo | administradores |
| `estudios/{id}/progreso/{V}` | muestras respondidas por sesión | todos |
| `admins/{email}` | administradores | administradores |

Las reglas de seguridad están en `firestore.rules` (se publican en la consola: Firestore → Reglas).

## Modificar la app
El código está en `src/`. `index.html` y `app.js` se generan:
```bash
npm i -D esbuild firebase react@18 react-dom@18 tailwindcss@3
node src/build.mjs            # versión real
node src/build.mjs --prueba   # versión con datos simulados para probar (no publicar)
```
`config.json` tiene la configuración web de Firebase (es pública por diseño; la seguridad la dan las reglas).

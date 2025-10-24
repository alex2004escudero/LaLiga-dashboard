# ⚽ LA LIGA DASHBOARD 13/16

**Análisis visual de La Liga desde la temporada 2013/14 hasta la 2015/16.**  
Proyecto de **análisis y visualización de datos deportivos**: limpieza de datos con **R** y creación de un **dashboard interactivo en Power BI** para explorar el rendimiento de los equipos.

---

## 🧠 Descripción

El dashboard permite analizar estadísticas clave de cada equipo por temporada mediante filtros interactivos (*Season* y *Team*).  
Incluye KPIs como **Points, Wins, Goals Scored, Goals Conceded, League Standing y Best Player**, además de:
- Una tabla de clasificación general con escala de color.  
- Una línea temporal de posiciones (*League Standing across seasons*).  
- Integración de los escudos de cada club.

---

## 🔧 Flujo de trabajo

- **R (`futbol1.R`):** limpieza y unificación de datos (`ratings.csv`, `team_stats_all_seasons.csv`, `escudos_liga.xlsx`), cálculo de métricas por equipo/temporada y exportación a Power BI.  
- **Power BI (`futbol1.pbix`):** modelado, medidas DAX, diseño visual y filtros por temporada/equipo.

---

## 📊 Archivos principales

| Archivo | Descripción |
|----------|-------------|
| `futbol1.R` | Script en R con limpieza y creación de métricas |
| `futbol1.pbix` | Dashboard de Power BI |
| `ratings.csv`, `team_stats_all_seasons.csv` | Datos base |
| `escudos_liga.xlsx` | Escudos de equipos para visualización |
| `dashboard la liga 1.png`, `dashboard la liga 2.png` | Capturas del dashboard |

---

## 🖼️ Vista del dashboard

![Dashboard LaLiga](dashboard%20la%20liga%201.png)

---

## 🧰 Herramientas
R · Power BI · Excel / CSV

---

📌 *Proyecto académico y personal enfocado en la práctica de análisis y visualización de datos deportivos.*  
👉 [Ver resumen y contexto en Notion](https://www.notion.so/Portfolio-de-Alejandro-6e1093c6146a4f648263a4f243777c60?source=copy_link)


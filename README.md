# Kauai: The Garden Isle
### A Statistical Analysis of Hawaii's Oldest Island

Comprehensive socioeconomic analysis of Kauai County comparing it to other Hawaiian counties using U.S. Census data (2018-2022).

## 📦 Installation
```r
install.packages(c("tidyverse", "tidycensus", "tigris", "sf", "leaflet", 
                   "plotly", "kableExtra", "gt", "scales", "viridis", 
                   "patchwork", "shiny", "shinydashboard", "DT", "writexl"))
```

## 🚀 Quick Start

### Render the Report
```r
quarto::quarto_render("Kauai_Final_Report.qmd")
```

### Run the Shiny App
```r
shiny::runApp("app.R")
```

## 📁 Project Structure
```
kauai-analysis/
├── Kauai_Final_Report.qmd    # Main report (Quarto)
├── app.R                       # Interactive dashboard (Shiny)
├── data/                       # Pre-processed census data
│   ├── hawaii_data.rds
│   ├── hawaii_spatial.rds
│   ├── kauai_roads.rds
│   ├── kauai_water.rds
│   └── kauai_census_tracts_data.rds
└── images/                     # Report images
```

## 📊 What's Included

**Quarto Report:**
- 2 interactive Leaflet maps (population & income)
- 6+ statistical plots (demographics, economics, housing)
- 2 comprehensive data tables
- Geographic infrastructure analysis (roads, water features)
- Census tract-level income analysis

**Shiny Dashboard:**
- 6 interactive tabs (Overview, Maps, Economics, Demographics, Data Table, Compare)
- Dynamic filters and metric selection
- Downloadable data (CSV/Excel)
- Multi-county radar comparisons

## 📈 Data Sources

- **U.S. Census Bureau ACS** 2018-2022 5-year estimates
- **TIGER/Line Shapefiles** 2022

Variables: Population, Median Income, Median Age, Education, Home Value, Rent, Unemployment

## 🔧 Customization

### Change Colors
```r
kauai_colors <- c("Kauai" = "#2E86AB", "Other Counties" = "#7A8B99")
```

### Modify Report Theme
```r
format:
  html:
    theme: flatly  # Try: cosmo, journal, lumen, etc.
```

## 📝 Notes

- Data represents **2018-2022 averages** (not a single year)
- All data pre-loaded in `/data` folder (no API key needed)
- Kalawao County may have missing data (population <100)
- Geometries pre-simplified for fast rendering

## 🐛 Troubleshooting

**Shiny app won't run:**
```r
# Check working directory contains app.R and data/ folder
getwd()
setwd("path/to/project")
```

**Maps not displaying:**
- Check internet connection (Leaflet uses online tiles)

**Package errors:**
```r
# Update packages
update.packages(ask = FALSE)
```

---
*Code Reference: claude.ai, and chatgpt.com*
*Data: U.S. Census Bureau ACS 2018-2022 | Tools: R, Quarto, Shiny*
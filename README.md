# 🇵🇹 AENS Portugal Tourism Clusters

<div align="center">

**A statistical analysis of tourism competitivensse and regional clustering across Portugal.**

[View Report](report.pdf)

</div>

## 📖 Overview

This repository hosts the code and data for a comprehensive study aimed at identifying and characterizing distinct tourism competitiveness clusters within Portugal. Utilizing various geographical, socio-economic, and tourism-related variables, the project employs principal component analysis and statistical clustering techniques to segment Portuguese regions based on their tourism competitiveness. The analysis provides valuable insights into the diverse nature of tourism across the country, aiding in targeted policy-making and strategic development.

## ✨ Features

-   **Data-driven Clustering:** Applies robust statistical methods to group Portuguese regions into meaningful tourism clusters.
-   **Geospatial Integration:** Incorporates detailed geographical data of Portugal to visualize clusters and their spatial distribution.
-   **Variable Profiling:** Analyzes key input variables to describe the unique characteristics of each identified tourism cluster.
-   **Comprehensive Reporting:** Generates a detailed PDF report summarizing the methodology, findings, and implications of the clustering analysis.
-   **Reproducible Analysis:** Provides the R script and input data to fully reproduce the analysis and results.

## 🚀 Quick Start

To replicate the analysis or explore the dataset, follow these steps.

### Prerequisites
-   **R Environment:** Ensure you have R installed on your system. You can download it from the [CRAN website](https://cran.r-project.org/).

### Installation

1.  **Clone the repository**
    ```bash
    git clone https://github.com/rafaeldrrmachado/AENS_portugal_tourism_clusters.git
    cd AENS_portugal_tourism_clusters
    ```

2.  **Install R dependencies**
    Open your R console and run the following commands to install the necessary packages. (These are inferred and may need adjustment based on the actual `script_AENSvfinal.R` content.)
    ```R
    install.packages(c("dplyr", "tidyr", "cluster", "factoextra", "sf", "jsonlite", "ggplot2"))
    # You may need additional packages depending on the script's actual requirements.
    ```

### Running the Analysis

1.  **Ensure data is in place**
    The `variaveis input/` and `variaveis profile/` directories should contain the necessary input data files (e.g., `.csv` files). The `gadm41_PRT_2.json` file should be in the root directory.

2.  **Execute the R script**
    Open `script_AENSvfinal.R` in your R IDE (like RStudio) and run the script, or execute it from the command line:
    ```bash
    Rscript script_AENSvfinal.R
    ```
    The script will perform the data loading, clustering, and analysis steps. It might generate additional output files or visualizations in the process.

## 📁 Project Structure

```
AENS_portugal_tourism_clusters/
├── gadm41_PRT_2.json   # Geographical boundaries for Portugal (Admin Level 2)
├── report.pdf          # The final report detailing the tourism clustering analysis
├── script_AENSvfinal.R # The main R script for data analysis and clustering
├── variaveis input/    # Directory containing input data variables for the analysis
└── variaveis profile/  # Directory containing data/outputs related to cluster profiles
```

## 📚 Report

The comprehensive findings of this tourism clustering analysis are documented in `report.pdf`. This report outlines the methodology, presents the identified clusters, discusses their characteristics, and provides visualizations.

## 🙏 Acknowledgments

-   Data sources for Portuguese geographical and tourism-related statistics (primarily INE).

## 📞 Support & Contact

-   🐛 Issues: [GitHub Issues](https://github.com/rafaeldrrmachado/AENS_portugal_tourism_clusters/issues)

---

<div align="center">

**⭐ Star this repo if you find this analysis helpful!**

Made by [Rafael Machado](https://github.com/rafaeldrrmachado)

</div>


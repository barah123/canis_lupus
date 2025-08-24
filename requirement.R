# requirements.R
# Install CRAN packages first
cran_packages <- c(
  'shiny',
  'plotly',
  'DT',
  'vegan',
  'ape',
  'ggplot2',
  'tidyverse',
  'metacoder',
  'networkD3',
  'heatmaply',
  'dendextend',
  'viridis',
  'RColorBrewer',
  'ggthemes',
  'purrr'
)

# Install CRAN packages
install.packages(cran_packages, repos = 'https://cloud.r-project.org/')

# Install BiocManager if not present
if (!require("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager", repos = "https://cloud.r-project.org/")
}

# Install Bioconductor packages
bioc_packages <- c(
  'phyloseq',
  'microbiome'
)

BiocManager::install(bioc_packages)

# Install any packages that might have failed or need specific handling
# Some packages might need additional system dependencies
if (!require("metacoder", quietly = TRUE)) {
    # metacoder might need to be installed from GitHub or have specific dependencies
    install.packages("metacoder", repos = "https://cloud.r-project.org/")
}

# Verify installations
cat("Checking package installations...\n")
for (pkg in c(cran_packages, bioc_packages)) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        cat(paste("Package", pkg, "failed to install\n"))
    } else {
        cat(paste("Package", pkg, "successfully installed\n"))
    }
}

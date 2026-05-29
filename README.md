# BioStatR

Este paquete ha sido desarrollado como soporte del libro [MatematicaEstadisticaMedica](https://github.com/migariane/MatematicaEstadisticaMedica).

This is the BioStatR R package.

## Installation

### From GitHub (Development version)
You can install the development version of BioStatR from [GitHub](https://github.com/migariane/BioStatR) with:

```r
# install.packages("remotes")
remotes::install_github("migariane/BioStatR")
```

*Note: This method is recommended as it automatically builds the package for your specific operating system (Windows, macOS, or Linux).*

### From Local Files
You can also install the package from the provided files in the repository:

```r
# For .tar.gz (Source, cross-platform)
install.packages("path/to/BioStatR_1.0.0.tar.gz", repos = NULL, type = "source")

# For .tgz (macOS binary)
install.packages("path/to/BioStatR_1.0.0.tgz", repos = NULL, type = "binary")

# For .zip (Windows binary)
install.packages("path/to/BioStatR_1.0.0.zip", repos = NULL, type = "binary")
```

*Note: The Windows binary (`.zip`) should be built on a Windows machine using `devtools::build(binary = TRUE)` to ensure compatibility.*

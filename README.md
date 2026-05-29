# BioStatR

Este paquete ha sido desarrollado como soporte del libro [Matemática Estadística Médica con R](https://migariane.github.io/MatematicaEstadisticaMedicinaR/).

Bioestadística con R: BioStatR es un paquete de R para rutinas de básicas de bioestadística.

## Instalación

### Desde GitHub (Versión de desarrollo)
Puede instalar la versión de desarrollo de BioStatR desde [GitHub](https://github.com/migariane/BioStatR) con:

```r
# install.packages("remotes")
remotes::install_github("migariane/BioStatR")
```

*Nota: Se recomienda este metodo ya que construye automáticamente el paquete para su sistema operativo específico (Windows, macOS o Linux).*

### Desde archivos locales
También puede instalar el paquete desde los archivos proporcionados en el repositorio:

```r
# Para .tar.gz (Fuente, multiplataforma)
install.packages("path/to/BioStatR_1.0.0.tar.gz", repos = NULL, type = "source")

# Para .tgz (Binario de macOS)
install.packages("path/to/BioStatR_1.0.0.tgz", repos = NULL, type = "binary")

# Para .zip (Binario de Windows)
install.packages("path/to/BioStatR_1.0.0.zip", repos = NULL, type = "binary")
```

*Nota: El binario de Windows (`.zip`) debe construirse en una máquina con Windows usando `devtools::build(binary = TRUE)` para asegurar la compatibilidad.*

## Cómo citar este paquete

Para obtener la referencia bibliográfica del paquete en R, ejecute:

```r
citation("BioStatR")
```

Si necesita la referencia en formato BibTeX para su artículo académico, puede usar la siguiente estructura:

### Cita del paquete
```bibtex
@Manual{,
  title = {BioStatR: Rutinas de bioestadística},
  author = {Pedro Jesús Femia Marzo & Miguel Angel Luque Fernandez},
  year = {2026},
  note = {R package version 1.0.0},
  url = {https://github.com/migariane/BioStatR},
}
```

### Cita del libro de soporte
Si utiliza este paquete en su investigación, le agradeceríamos que cite también el libro de texto de soporte:

*   **Referencia:** Femia, P., & Luque Fernandez, M. A. *Matemática Estadística Médica con R*.
*   **Enlace:** [Matemática Estadística Médica con R](https://migariane.github.io/MatematicaEstadisticaMedicinaR/)
*   **https://migariane.github.io/MatematicaEstadisticaMedicinaR/**


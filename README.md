<!-- Add a project state badge
See https://github.com/BCDevExchange/Our-Project-Docs/blob/master/discussion/projectstates.md
If you have bcgovr installed and you use RStudio, click the 'Insert BCDevex Badge' Addin. -->

[![img](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

# PEMprepr

One module of the [`PEMr`](https://github.com/bcgov/PEMr)project.
Functions in the package focus on the initial data acquisition,
covariate generation, including the preparation of multi-resolution
covariate stack.

Additional modules in the project include:

-   `PEMsamplr` – functions for preparing a sample plan
-   `PEMmodelr` – model generation, and evaluation
-   `PEMmapr` – map production

### Features

The raster focused functions have the objective of creating a
multi-resolution raster stack of numerous covariates.

1.  Snap and area of interest to ensure that subsequent rasters are
    stackable
2.  Creation of covariates from a digital terrain model via
    [SAGA-GIS](https://saga-gis.sourceforge.io/en/index.html)
3.  Disaggregate coarse resolution rasters to fine scales

The bulk of the functions are *wrapper-functions* utilizing the
[`terra`](https://github.com/rspatial/terra) and
[`sf`](https://github.com/r-spatial/sf) packages

Vector functions in this package faciltate the aquisition of a standard
set of data from the [BC Data
Catalogue](https://catalogue.data.gov.bc.ca) via the [`bcdata`]()
package.

### Installation

``` r
remotes::install_github("bcgov/PEMprepr", build_vignettes = TRUE)
```

### Usage

Raster based:

1.  Create a standardized folder structure for the area of interest
2.  Create an standardized area of interest – that fits well to a
    xxx100m<sup>2</sup> extent
3.  Create a digital terrain model for the desired resolutions (e.g. 5,
    10, and 25m<sup>2</sup>)
4.  Generate covariates for each resolution
5.  disaggregate the course scale covariates to the finest resolution
    level

Vector based – collect a standard set of vectors and place them within
the folder structure.

#### Standardized folder structure

*todo – add a graphic of the folders*

#### Example

The vignette for the package provide an example workflow.

``` r
vignette("PEMprepr")
#> starting httpd help server ... done
```

### Project Status

currently functional … with polish needed.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/PEMprepr/issues/).

### How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### License

    Copyright 2022 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

------------------------------------------------------------------------

*This project was created using the
[bcgovr](https://github.com/bcgov/bcgovr) package.*

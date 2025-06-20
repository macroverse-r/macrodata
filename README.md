
<!-- README.md is generated from README.Rmd. Please edit that file -->

# macrodata

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: AGPL
v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
<!-- badges: end -->

The goal of macrodata is to provide access to international economic and
social panel data from major organizations and academic researchers. It
includes functions for loading, processing, and harmonizing data across
different sources and frequencies.

This package is part of the [macroverse
ecosystem](https://github.com/macroverse-r/macroverse).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("macroverse-r/macrodata")
```

## Basic Usage

### Loading Data

``` r
library(macrodata)

# Load GDP data for G7 countries
data <- md_data(
  ISO = "G7",
  formula = "GDP_C",
  years = c(2010, 2023)
)

# Load multiple indicators
data <- md_data(
  ISO = c("USA", "CHN", "DEU"),
  formula = c("GDP_C", "CU_C/GDP_C*100"),
  variable = c("GDP", "Current Account (% of GDP)"),
  years = c(2000, 2023)
)
```

### Available Data

``` r
# View all available indicators
md_info()

# View only quarterly data
md_info("Q")

# Search for specific indicators
md_info(search = c("GDP", "employment"))

# Filter by data source
md_info(origin = "WB")  # World Bank data
```

## Advanced Features

### Data Processing

``` r
# Seasonal adjustment for quarterly data
data <- md_data(
  ISO = "USA",
  formula = "GDP_C",
  years = c(2010, 2023),
  adjust_seasonal = TRUE,
  window_seasadj = 7
)

# Frequency conversion
data <- md_data(
  ISO = "OECD",
  formula = c("GDP_C", "UNEMP_R"),  # Mixed frequencies
  years = c(2010, 2023),
  matching_yq = "Q2Y",  # Convert quarterly to yearly
  interpolation_method = "Linear"
)
```

### Aggregation

``` r
# Aggregate across countries
data <- md_data(
  ISO = "EU",
  formula = "GDP_C",
  years = c(2010, 2023),
  aggregate_iso = "Sum"  # Sum GDP across EU countries
)

# Aggregate over time
data <- md_data(
  ISO = "USA",
  formula = "GDP_C",
  years = c(2010, 2023),
  aggregate_period = "CAGR"  # Calculate compound annual growth rate
)
```

### Working with Categories

``` r
# Use country groups
data <- md_data(
  ISO = c("BRICS", "G7"),
  formula = "GDP_PC_C",
  years = c(2000, 2023)
)

# Exclude specific countries
data <- md_data(
  ISO = "EU-GBR",  # EU excluding UK
  formula = "GDP_C",
  years = c(2010, 2023)
)
```

## Helper Functions

``` r
# GDP-weighted mean
weighted_inflation <- md_gdp_weighted_mean(
  data = inflation_data,
  weight_year = 2020
)

# Year-over-year growth
growth_data <- data.frame(
  Date = c("2020", "2021", "2022"),
  Value = c(100, 105, 110)
)
growth_rates <- md_calculate_yoy_growth(growth_data)

# Geometric mean (useful for growth rates)
avg_growth <- md_geometric_mean(c(0.05, 0.03, -0.02, 0.08))
```

## Data Sources

The package integrates data from: - World Bank Development Indicators -
International Monetary Fund (IFS, Balance of Payments) - Bank for
International Settlements - OECD Main Economic Indicators - Academic
databases (Jordà-Schularick-Taylor, KOF, etc.)

## macroverse Ecosystem

The macrodata package works seamlessly with other macroverse packages: -
**[mvcommon](https://github.com/macroverse-r/mvcommon)**: Common
utilities and validation -
**[pplot](https://github.com/macroverse-r/pplot)**: Panel data
visualization -
**[isomapper](https://github.com/macroverse-r/isomapper)**: ISO codes
and country mapping - **macrodata**: Data loading and processing (this
package) - **[mvlazy](https://github.com/macroverse-r/mvlazy)**:
Convenience functions -
**[macroverse](https://github.com/macroverse-r/macroverse)**:
Meta-package loading all components

## License

This package is licensed under AGPL-3.0.

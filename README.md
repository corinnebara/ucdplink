
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ucdplink

The goal of **ucdplink** is to link data on violent events from the
Uppsala Conflict Data Program (UCDP) Georeferenced Event Dataset (GED)
to armed conflicts listed in the UCDP Armed Conflict Dataset (ACD).

The rationale for this procedure is described in the article: Bara,
Corinne & Maurice P. Schumann, “Who, what, and where? Linking violence
to civil wars”, *Research & Politics* (2025).

Please cite the article when using this data or package.

**The logic in brief:** Civil wars are more than battles between
governments and rebels; they involve a multitude of actors who
perpetrate different forms of violence linked to the war in some way. In
practice, however, linking various forms of violence to one another and
attributing them to a specific civil war remains a challenge,
conceptually and practically. The ucdplink package solves the practical
aspects of the violence attribution problem and links casualty counts
from different forms of war-related violence to specific civil wars
using information on the actors involved in the event, and on the event
location.

The main function **ucdplink** produces a conflict-month dataset with
information on monthly casualties from various forms of violence. For
more details on the variables included in the final dataset please
consult the
[Codebook](https://github.com/corinnebara/ucdplink/releases/download/v1.0.0/Codebook.pdf).
The procedure has three steps:

1.  It downloads the UCDP/PRIO Armed Conflict Dataset (ACD) and turns it
    from annual data into a conflict-month dataset. This is done in the
    function **acdtomonthly**, which may be useful for users even if
    they are not interested in violence linking.

2.  It then downloads the UCDP Georeferenced Event Dataset (GED) and
    performs some common data wrangling steps, namely splitting
    multi-month events into separate monthly events with casualty counts
    evenly divided between them, and splitting events perpetrated by
    coalition actors among the coalition-constituting actors. This is
    done in the function **gedprep,** which may also be useful even for
    users not interested in violence linking.

3.  Finally, the function links casualties from different forms of
    violence (state-based, one-sided, non-state) to the conflict
    episodes in which this violence (most likely) takes place. The
    linking is done via the actors involved in the violence, and - if
    that is not possible - via the location of the event, using
    information on the location of conflicts from the WZONE Conflict
    Zones Data.

The package currently processes UCDP data versions 24.1. When using this
package, make sure you also correctly cite the source data.

For the function **acdtomonthly**: Cite UCDP/PRIO Armed Conflict Dataset
v.24.1. For citation details, see [UCDP Dataset Download
Center](https://ucdp.uu.se/downloads/).

For the function **gedprep**: Cite UCDP Georeferenced Event Dataset
(GED) Global version 24.1. For citation details, see [UCDP Dataset
Download Center](https://ucdp.uu.se/downloads/).

For the function **ucdplink**: Cite both the above, and in addition the
WZONE conflict zone data: [WZONE: Zones of Armed Conflicts - Harvard
Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PUWJEU).

## Installation

You can install the package from github as follows:

``` r
library(devtools)

devtools::install_github("corinnebara/ucdplink")
library(ucdplink)
```

You also need the `dplyr`, `tidyr`, `stringr`, `readr`,
`lubridate`, `sf`, `cshapes`, `future`, and `furrr` packages. R will try
to install these dependencies if not already installed.

## The Final Data

**Note:** Running the `ucdplink` function may take a considerable amount
of time due to computationally intensive spatial operations. Users who
are happy with our default choices (see below) can also simply download
the final dataset. It includes conflicts from 1989 to 2023 while they
are active, and includes all inactive months until the end of 2023.
Users can simply filter the amount of postwar months they want included
for each conflict episode via the *postwarmonth* variable.

- [Final Dataset
  Excel](https://github.com/corinnebara/ucdplink/releases/download/v1.0.0/Final_dataset.xlsx)

- [Final Dataset R
  Data](https://github.com/corinnebara/ucdplink/releases/download/v1.0.0/Final_dataset.rds)

## ucdplink

The function takes the following arguments:

- postwar_months: How many inactive months to add to each episode.
  Default is 0 (only active conflict). Add 5000 if you want all possible
  postwar months until the end of the dataset.

- include_alqaida: Whether to include conflict 418 (US vs. al-Qaida),
  default is to exclude, as this conflict did not take place on US
  territory.

- start_year: Specify the start year. Default: 1989.

- end_year: Specify the end year. Default: longest possible (currently
  2023).

- buffer_percent: How much buffer to add around each conflict zone (in
  percent, numeric from 0 to 100). Default is no buffer.

- clipcountry: Whether to clip conflict zones at the borders of the
  country/countries primarily affected by the conflict. Default is to
  clip.

- divide_deaths: For events that fall into multiple conflict zones: use
  full casualties in each or divide deaths between the conflicts?
  Default is to use full.

- include_gedtrack: Whether to also return a version of GED in which
  event has information on how it was linked to the conflict-month
  dataset. Default is to not include.

Please refer to Bara & Schumann (2025) for a discussion on the
implications of different user choices.

``` r
linkeddata <- ucdplink(postwar_months = 0, include_alqaida = FALSE, start_year = 1989, end_year = 2023, buffer_percent = 0, clipcountry = TRUE, divide_deaths = FALSE, include_gedtrack = FALSE)
```

## acdtomonthly

The function takes the following arguments:

- postwar_months: How many inactive months to add to each episode.
  Default is 0 (only active conflict). Add 5000 if you want all possible
  postwar months until the end of the dataset.

- include_alqaida: Whether to include conflict 418 (US vs. al-Qaida),
  default is to exclude, as this conflict did not take place on US
  territory.

- start_year: Specify the start year. Default: 1989.

- end_year: Specify the end year. Default: longest possible (currently
  2023).

``` r
monthlydata <- acdtomonthly(postwar_months = 0, include_alqaida = FALSE, start_year = 1989, end_year = 2023)
```

## gedprep

The function takes no arguments:

``` r
preppedged <- gedprep()
```

## Work in Progress

- In the future, we plan to allow users to choose the UCDP version they
  want to work with. As of now, the package only works with the
  (currently) newest version of UCDP, i.e., v24.

- We also plan to implement an additional user choice to only link those
  forms of violence that can be linked via actor identifiers (battle,
  rebel one-sided violence, and inter-rebel fighting), and forego the
  more time-consuming spatial linking.

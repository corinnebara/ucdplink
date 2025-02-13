#' Creates a UCDP conflict-month dataset with information on various forms of violence from UCDP GED, linked via actors or space
#'
#' @param postwar_months How many inactive months to add to each episode. Default is 0 (only active conflict). Add 5000 if you want all possible postwar months until the end of the dataset.
#' @param include_alqaida Whether to include conflict 418 (US-al-Qaida), default is to exclude, as this conflict did not take place on US territory.
#' @param start_year Specify the start year. Default: 1989.
#' @param end_year Specify the end year. Default: longest possible (currently 2023).
#' @param buffer_percent How much buffer to add around each conflict zone (in percent, numeric from 0 to 100). Default is no buffer.
#' @param clipcountry Whether to clip conflict zones at the borders of the country/countries primarily affected by the conflict. Default is to clip.
#' @param divide_deaths For events that fall into multiple conflict zones: use full casualties in each or divide deaths between the conflicts? Default is to use full.
#' @param include_gedtrack Whether to also return a version of GED in which event has information on how it was linked to the conflict-month dataset.
#'
#' @return A conflict-month dataset based on UCDP conflict data, with casualty counts from various forms of violence from UCDP GED
#' @export
#'
#' @importFrom utils download.file unzip
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import readr
#' @import lubridate
#' @import sf
#' @import cshapes
#' @import future
#' @import furrr
#'
#' @examples
#' ucdplink(include_alqaida = FALSE, start_year = 1999, end_year = 2010, include_gedtrack = TRUE)
#' \dontrun{
#'   ucdplink(postwar_months = 24, buffer_percent = 20, clipcountry = FALSE, divide_deaths = TRUE)
#' }
ucdplink <- function(postwar_months = 0,
                     include_alqaida = FALSE,
                     start_year = NULL,
                     end_year = NULL,
                     buffer_percent = 0,
                     clipcountry = TRUE,
                     divide_deaths = FALSE,
                     include_gedtrack = FALSE) {


  # Step 1: Create basedata (conflict-month) from annual conflict data
  message("Step 1: Creating conflict-month data from UCDP Armed Conflict Dataset")
  basedata <- acdtomonthly(postwar_months = postwar_months,
                           include_alqaida = include_alqaida,
                           start_year = start_year,
                           end_year = end_year)

  # Step 2: Create conflict zones for all episodes in this base dataset
  message("Step 2: Creating conflict zones for all episodes in the data")

  steptwo <- makezones(basedata,
                       clipcountry = clipcountry,
                       buffer_percent = buffer_percent)

  basedata <- steptwo$basedata
  episode_zones <- steptwo$episode_zones

  # Step 3: Prepare GED data for further processing
  message("Step 3: Preparing UCDP GED event dataset for further processing")

  gedprepped <- gedprep()

  # Step 4: Link violence by actors identifiers
  message("Step 4: Linking violence from GED to conflict-month data via actor identifiers")

  stepfour <- actorlink(gedprepped, basedata)

  basedata <- stepfour$basedata
  gedtrack <- stepfour$gedtrack

  # Step 5: Intersect GED with conflict zones
  message("Step 5: Intersecting GED with conflict zones. Have a coffee... this takes a while.")

  gedintersection <- intersect(gedprepped,episode_zones)

  # Step 6: Link violence by location
  message("Step 6: Linking violence from GED to conflict-month data via event location")

  stepsix <- spatiallink(gedintersection,
                         basedata,
                         gedtrack,
                         divide_deaths = divide_deaths)

  basedata <- stepsix$basedata
  gedtrack <- stepsix$gedtrack

  # Step 7: Finalize dataset
  message("Step 7: Wrapping up")

  basedata <- finalize(basedata)

  # Optionally, assign gedtrack to the global environment if user chooses to get it
  if (include_gedtrack) {
    assign("gedtrack", gedtrack, envir = .GlobalEnv)
  }

  # Return only final conflict-month dataset as a default
  return(basedata)
}

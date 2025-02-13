#' Function to turn UCDP annual conflict data into a conflict-month dataset.
#'
#' @param postwar_months Maximum postwar months to include, default is only active months. Put 5000 if you want all possible postwar months in the data.
#' @param include_alqaida Optional: Include conflict 418 (US-al-Qaida), default is to exclude.
#' @param start_year Optional: Specify the start year. Default: 1989.
#' @param end_year Specify the end year. Default: longest possible in version (currently 2023).
#'
#' @return A conflict-month dataset based on UCDP data.
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import readr
#' @import lubridate
#'
#' @note When using this function, please ensure that you correctly attribute the source data:
#' @note UCDP/PRIO Armed Conflict Dataset version 24.1 from the Uppsala Conflict Data Program.
#' @note For citation details, refer to the UCDP website: https://ucdp.uu.se/downloads/
#'
#' @examples
#' acdtomonthly(postwar_months = 5000)
#' acdtomonthly(start_year = 2015, end_year = 2023,include_alqaida = TRUE,postwar_months = 24)
#' acdtomonthly()
acdtomonthly <- function(
    postwar_months = 0,
    include_alqaida = FALSE,
    start_year = NULL,
    end_year = NULL
) {

  # Use helper function to download ACD
  acd <- downloaducdp("acd")

  # Dynamically determine the range of years in the dataset
  min_year <- max(1989, min(acd$year, na.rm = TRUE))  # Minimum year (at least 1989)
  max_year <- max(acd$year, na.rm = TRUE)            # Maximum year

  # Validate and adjust `start_year`
  if (!is.null(start_year) && start_year < 1989) {
    stop("Invalid start year: Start year cannot be earlier than 1989.")
  }
  if (!is.null(start_year) && start_year < min_year) {
    warning(paste("Start year", start_year, "is earlier than the dataset's range. Adjusting to", min_year))
    start_year <- min_year
  }

  # Validate and adjust `end_year`
  if (!is.null(end_year) && end_year > max_year) {
    warning(paste("End year", end_year, "is beyond the dataset's range. Adjusting to", max_year))
    end_year <- max_year
  }

  # Ensure `start_year` and `end_year` are consistent
  if (!is.null(start_year) && !is.null(end_year) && start_year > end_year) {
    stop("Invalid year range: Start year cannot be greater than end year.")
  }

  # Default to full range if years are not specified
  if (is.null(start_year)) start_year <- min_year
  if (is.null(end_year)) end_year <- max_year

  # Filter conflicts
  acd <- acd %>%
    filter(type_of_conflict %in% c(3, 4)) %>%
    { if (!include_alqaida) filter(., conflict_id != 418) else . }

  # Standardize dates
  acd <- acd %>%
    mutate(
      end_temp = floor_date(ep_end_date, unit = "month"),
      start_temp = floor_date(start_date2, unit = "month"),
      year = as.character(year),
      date_new = as.Date(paste0(year, "-12-01")),
      year = as.numeric(year)
    )

  # Add end dates if missing
  last_year <- max(acd$year, na.rm = TRUE)
  acd <- acd %>%
    group_by(conflict_id) %>%
    mutate(
      end_temp = case_when(
        year == last_year & ep_end == 1 & !is.na(ep_end_date) ~ floor_date(ep_end_date, unit = "month"),
        year == last_year ~ date_new,
        TRUE ~ end_temp
      )
    )

  # Reduce to one line per episode
  acdsmall <- acd %>%
    filter(!is.na(end_temp)) %>%
    select(conflict_id, start_date, start_date2, start_temp, end_temp)

  # Filter out episodes ending before `start_year` (if specified)
  acdsmall <- acdsmall %>%
    filter(end_temp >= as.Date(paste0(start_year, "-01-01")))

  # Number episodes and expand
  acdbig <- acdsmall %>%
    arrange(conflict_id, start_date2) %>%
    group_by(conflict_id) %>%
    mutate(
      epicount = row_number(),
      conflict_epi_id = str_c(conflict_id, "_", epicount)
    ) %>%
    mutate_at(vars(start_temp, end_temp), ymd) %>%
    group_by(conflict_epi_id) %>%
    mutate(month_year = list(seq(start_temp, end_temp, by = "month"))) %>%
    unnest(cols = month_year) %>%
    ungroup()

  # Add `active`
  acdbig <- acdbig %>% mutate(active = 1)

  # Complete missing months across the global range
  global_min_date <- min(acdbig$month_year, na.rm = TRUE)
  global_max_date <- max(acdbig$month_year, na.rm = TRUE)

  acdfull <- acdbig %>%
    complete(
      conflict_id,
      month_year = seq.Date(global_min_date, global_max_date, by = "month")  # Use global date range
    )
  acdfull <- acdfull %>%
    mutate(year = as.numeric(format(month_year, "%Y")))

  # Fill all variables downward except `active`
  acdfull <- acdfull %>%
    group_by(conflict_id) %>%
    fill(-active, .direction = "down")  # Exclude `active` from being filled downward

  # Code the `active` variable
  acdfull <- acdfull %>%
    mutate(
      active = case_when(
        is.na(active) & is.na(conflict_epi_id) ~ -1,  # Prewar rows
        is.na(active) ~ 0,                           # Postwar/interwar rows
        TRUE ~ active                                # Keep original active rows
      )
    )

  # Drop prewar observations
  acdfull <- acdfull %>% filter(active != -1)

  # Postwar filtering
  acdfull <- acdfull %>%
    group_by(conflict_epi_id, active) %>%
    mutate(
      pwmonth = cumsum(if_else(active == 0, 1, 0))
    ) %>%
    filter(
      active == 1 | (active == 0 & pwmonth <= postwar_months)
    )

  # Apply filtering for start and end years
  acdfull <- acdfull %>%
    filter(!is.na(year) & year >= start_year & year <= end_year)

  # Drop inactive-only episodes
  acdfull <- acdfull %>%
    group_by(conflict_id) %>%
    filter(!all(active == 0)) %>%
    ungroup()

  # Add information from acd (annually changing one)
  acdfull <- acdfull %>% select(conflict_id, conflict_epi_id, month_year, active, pwmonth, year, start_date, start_date2)
  acdbig <- acdfull %>%
    left_join(
      acd %>% select(-start_date, -start_date2),  # Exclude start_date and start_date2 from acd
      by = c("conflict_id", "year")
    ) %>%
    select(-c(intensity_level, cumulative_intensity, start_prec,
              start_prec2, ep_end, ep_end_date, ep_end_prec, end_temp, start_temp, date_new)) %>%
    group_by(conflict_epi_id) %>%
    fill(everything(), .direction = "down") %>%
    ungroup()

  # Save and return (also as an exact copy that we later use to link violence to)
  basedata <- acdbig
  rm(acd)
  return(basedata) # for the polygon processing in next step

}


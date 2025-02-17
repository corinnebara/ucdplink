#' Cleans up final dataset
#'
#' @param basedata
#'
#' @return The final product of our R package: conflict-month dataset with all forms of violence linked
#' @keywords internal
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#'
#' @examples
#' \dontrun{
#'   finalize(basedata)
#' }
finalize <- function(basedata) {

  # remove gwno_b (only relevant for interstate wars)
  basedata <- basedata %>% select(-c(gwno_b))

  # Create summary OSV category (conflict government and official rebels together)
  basedata <- basedata %>% mutate(combatantosv = confgovosv + rebelosv)
  summary(basedata$combatantosv)

  # Unify formatting of all violence variables
  basedata <- basedata %>%
    mutate(across(c(battledeaths:communal), ~ round(as.numeric(.), 1)))

  # Create one additional variable (constant side B, i.e., all rebels that ever, in the same episode (!), fought that government
  rebcons <- basedata %>% filter(active==1) %>% select(conflict_epi_id,month_year,gwno_a,side_b_id)
  rebcons <- rebcons %>% separate_rows(side_b_id, sep = ", ")
  rebcons <- rebcons %>% distinct(conflict_epi_id, side_b_id, .keep_all = T) %>% select(-c(month_year))
  rebcons <- rebcons %>%
    group_by(conflict_epi_id) %>%
    summarise(sideb_constant = paste(side_b_id, collapse = ", "))
  basedata <- left_join(basedata,rebcons, by = c("conflict_epi_id"))

  # Rename some variables
  basedata <- basedata %>% rename(mdate = month_year,
                          epi_id = conflict_epi_id,
                          postwarmonth = pwmonth)

  # Final variable selection and put in right order
  basedata <- basedata %>% select(conflict_id, epi_id, mdate, year, active, postwarmonth, location, type_of_conflict, incompatibility,
                          territory_name, side_a, side_a_id, side_a_2nd, side_b, side_b_id, sideb_constant, side_b_2nd,
                          start_date, start_date2, gwno_a, gwno_a_2nd, gwno_b_2nd, region, version, missingzone,
                          battledeaths, collateralciv, rebelosv, interreb, interreb_collciv, zone_wrv, zone_wrv_civ, zone_osv,
                          zone_nsv, zone_nsv_collciv, govosv, confgovosv, combatantosv, nsagosv, internsag, internsag_collciv, communal)


  # Return final dataset
  return(basedata = basedata)

}


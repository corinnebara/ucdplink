#' Links those forms of violence to the conflict-month dataset that can be linked by actor identifiers
#'
#' @param gedprepped The GED event dataset, prepared in function "gedprep"
#' @param basedata The conflict-month dataset, prepared in function "acdtomonthly"
#'
#' @return The basedata with violence added, and a version of GED that tracks for each event how it was linked
#' @keywords internal
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import lubridate
#'
#' @examples
#' \dontrun{
#'   actorlink(gedprepped,basedata)
#' }
actorlink <- function(
    gedprepped,
    basedata
) {

  # Save a version of GED prepped to track how each event is linked
  gedtrack <- gedprepped

  # Function to process battledeaths
  process_battledeaths <- function() {
    # Keep only Type 1 violence, and only relevant variables
    gedbd <- gedprepped %>% filter(type_of_violence==1) %>%
      select(idsplit,id,date_start,final_date,conflict_new_id,
             best,deaths_civilians)

    # Aggregate best and deaths_civilians to conflict-month
    gedm <- gedbd %>% mutate(month_year = floor_date(date_start, "month")) %>%
      group_by(conflict_new_id, month_year) %>%
      summarize(battledeaths = sum(best),
                collateralciv = sum(deaths_civilians)) %>% ungroup()

    ### Merge into basedata dataset
    basedata$conflict_id <- as.character(basedata$conflict_id)
    basedata <- left_join(basedata,gedm, by = c("conflict_id" = "conflict_new_id", "month_year"))

    # recode missing
    basedata <- basedata %>% mutate(battledeaths = if_else(is.na(battledeaths),0,battledeaths),
                            collateralciv = if_else(is.na(collateralciv),0,collateralciv))

    # Mark in original gedprepped those events that were now linked into the basedata via this procedure
    linked_conflict_months <- basedata %>% select(conflict_id, month_year) %>% mutate(linkedconflictmonths=1)

    gedtrack <- gedtrack %>%
      left_join(linked_conflict_months, by = c("conflict_new_id" = "conflict_id", "final_date" = "month_year")) %>%
      mutate(link_actor_type1 = if_else(linkedconflictmonths==1, 1, 0)) %>% select(-c(linkedconflictmonths))

    list(basedata = basedata, gedtrack = gedtrack)
  }

  # Function to process rebel OSV
  process_rebelosv <- function() {
    # Keep only Type 3 violence by NOT gov actors, and only relevant variables
    gedosv <- gedprepped %>% filter(type_of_violence==3 & osv_is_gov_actor==0) %>%
      select(idsplit,id,date_start,final_date,conflict_new_id,
             side_a_new_id,side_b_new_id,country_id,best)

    # Assign these rebel perpetrators of OSV (side A) a conflict ID (time-constant) from the basedata dataset
    # Logic: Only rebels that also fight the government ever in the period under observation get a conflict ID, the rest can be dropped
    basedatatemp <- basedata %>% select(conflict_id,month_year,gwno_a,side_b_id)
    basedatatemp <- basedatatemp %>% separate_rows(side_b_id, sep = ", ")
    basedatatemp <- basedatatemp %>% distinct(conflict_id, side_b_id, .keep_all = T) %>% select(-c(month_year)) # keep only one obs for each rebel in each conflict (that "drops" annual info)

    # side_a_new_id and country_id from gedprepped has to be matched to side_b_id and gwno_a from basedatatemp, that way each event should only get one conflict assigned
    gedosv$side_a_new_id <- as.character(gedosv$side_a_new_id)
    gedosv$country_id <- as.character(gedosv$country_id)

    gedosv <- left_join(gedosv,basedatatemp, by = c("side_a_new_id" = "side_b_id", "country_id" = "gwno_a"))

    # clean up ged, drop NSA that don't fight the government
    gedosv <- gedosv %>% rename(conf_in_which_osv = conflict_id) %>% filter(!is.na(conf_in_which_osv))

    # This is now OSV by rebels that ever fight the government. An event will only be linked to basedata if both the conflict (given) and the month (not given) exist in basedata
    basedatarelevantconfmonths <- basedata %>% select(conflict_id, month_year) %>% mutate(existingmonths=1) # the conflict-months that existi in basedata
    gedused <- gedosv %>% ungroup() %>% select(idsplit,side_a_new_id,final_date,conf_in_which_osv) # the gedprepped that contains possibly relevant events
    # now assign the relevant months to the gedprepped being used to see whether they exist in basedata
    gedused <- left_join(gedused,basedatarelevantconfmonths, by = c("conf_in_which_osv"="conflict_id","final_date"="month_year"))
    gedused <- gedused %>% filter(existingmonths==1) %>% select(idsplit,side_a_new_id,final_date) %>% mutate(link_actor_rebosv=1)
    gedtrack$side_a_new_id <- as.character(gedtrack$side_a_new_id)
    gedtrack <- left_join(gedtrack,gedused, by = c("idsplit","side_a_new_id","final_date"))

    # now aggregate OSV to conflict-month (using the assigned conflict ID)
    osvcm <- gedosv %>% mutate(month_year = floor_date(final_date, "month")) %>%
      group_by(conf_in_which_osv,month_year) %>%
      summarize(best_sum = sum(best)) %>%
      ungroup()

    ### Merge this into basedata data
    osvcm <- osvcm %>% rename(rebelosv = best_sum)
    basedata <- left_join(basedata,osvcm, by = c("conflict_id" = "conf_in_which_osv", "month_year"))

    # recode missing
    basedata <- basedata %>% mutate(rebelosv = if_else(is.na(rebelosv),0,rebelosv))

    list(basedata = basedata, gedtrack = gedtrack)
  }


  # Subfunction: Process Interrebel Violence
  process_interreb <- function() {
    # Drop irrelevant observations and variables
    gednsv <- gedprepped %>% filter(type_of_violence==2 & nsv_org==1) %>%
      select(id, idsplit, date_start, date_end, final_date, year,
             conflict_new_id, side_a_new_id, side_b_new_id,
             country_id, best,deaths_civilians)

    ### Assign state-basedatad conflict IDs to the actors on side A and side B (unique identifier in this part of the data is idsplit, Side A, Side B)

    # In case needed: Create a temporary unique ID for each row
    gednsv <- gednsv %>% ungroup() %>% mutate(nsvid = row_number()) %>% relocate(nsvid)

    # Duplicate the data, and in the second half former side B becomes A ("bring all actors to one side"), note which are the original in a new column
    geda <- gednsv %>% mutate(original = 1)
    gedb <- gednsv %>% mutate(original = 0)
    # rename actor a into actor b
    gedb <- gedb %>% rename(side_a_new_id = side_b_new_id, side_b_new_id = side_a_new_id)
    # bind them together
    geddouble <- bind_rows(geda,gedb)

    # For each side A: Assign conflict ID by linking from ACD constant: Was this group involved in interrebel violence (side A) in that country (!) ever the side B in a state-basedatad conflict in that country?
    basedatatemp <- basedata %>% select(conflict_id,month_year,gwno_a,side_b_id)
    basedatatemp <- basedatatemp %>% separate_rows(side_b_id, sep = ", ")
    basedatatemp <- basedatatemp %>% distinct(conflict_id, side_b_id, .keep_all = T) %>% select(-c(month_year)) # keep only one obs for each rebel in each conflict (that "drops" annual info)
    geddouble$side_a_new_id <- as.character(geddouble$side_a_new_id)
    geddouble$country_id <- as.character(geddouble$country_id)

    geddouble <- left_join(geddouble,basedatatemp, by = c("side_a_new_id" = "side_b_id", "country_id" = "gwno_a"))
    geddouble <- geddouble %>% rename(fight_in_conf_id = conflict_id)

    # Within same event, the two actors can both be in the same conflict, or only one in a conflict at all, the other a militia
    # Do NOT divide deaths, except if the two side As fight in the same gov conflict (same conflict ID in the two rows of an nsavid)
    # First: drop events in which all conflict_id missing for the same nsvid
    geddouble <- geddouble %>%
      group_by(nsvid) %>%
      filter(!all(is.na(fight_in_conf_id)))
    # Second, divide deaths by side A and side B if conflict ID same for both actors
    geddouble <- geddouble %>% group_by(nsvid) %>%
      mutate(numdiff = n_distinct(fight_in_conf_id, na.rm = F)) %>% # by setting na.rm as false, only conflicts with two same non-missing conflict ID are coded 1, those with two different or one and one missing are 2
      mutate(best = if_else(numdiff==1,best/n(),best),
             deaths_civilians = if_else(numdiff==1,deaths_civilians/n(),deaths_civilians))

    ### Now we can drop those without conflict ID and aggregate to conflict-month using assigned conflict ID
    geddouble <- geddouble %>% filter(!is.na(fight_in_conf_id))

    ### Track them in original GED. These events, if they happen in a conflict-month that exists in basedata
    # I think events are not unique yet. However, the "correct" idsplit/sideA/sideB combo exists in original only once, duplicated one doesn't exist there
    basedatarelevantconfmonths <- basedata %>% select(conflict_id, month_year) %>% mutate(existingmonths=1) # the conflict-months that existi in basedata
    gedused <- geddouble %>% ungroup() %>% select(idsplit,original,side_a_new_id,side_b_new_id,final_date,fight_in_conf_id) # the gedprepped that contains possibly relevant events
    # We need to change the sides again for those events that are non-original, because the idsplit-sidea combo doesn't otherwise exist in gedprepped original
    geduseda <- gedused %>% filter(original==1)
    gedusedb <- gedused %>% filter(original==0)
    gedusedb <- gedusedb %>% rename(side_a_new_id = side_b_new_id, side_b_new_id = side_a_new_id)
    gedusedb$side_a_new_id <- as.character(gedusedb$side_a_new_id)
    geduseda$side_b_new_id <- as.character(geduseda$side_b_new_id)
    gedused <- bind_rows(geduseda,gedusedb)
    # Make gedused distinct basedatad on the unique identifier in gedtrack (isplit,A,B,finaldate)
    gedused <- gedused %>% ungroup() %>% distinct(idsplit,side_a_new_id,side_b_new_id,final_date, .keep_all = T)
    # now assign the relevant months to the gedprepped being used to see whether they exist in basedata
    gedused <- left_join(gedused,basedatarelevantconfmonths, by = c("fight_in_conf_id"="conflict_id","final_date" = "month_year"))
    gedused <- gedused %>% filter(existingmonths==1) %>% select(idsplit,side_a_new_id,side_b_new_id,final_date) %>% mutate(link_actor_interreb=1)
    gedtrack$side_b_new_id <- as.character(gedtrack$side_b_new_id)
    gedtrack <- left_join(gedtrack,gedused, by = c("idsplit","side_a_new_id","side_b_new_id","final_date"))


    # Do the actual aggregation and link into basedata
    nsvmonthly <- geddouble %>% mutate(month_year = floor_date(final_date, "month")) %>%
      group_by(fight_in_conf_id,month_year) %>%
      summarize(interreb = sum(best),
                interreb_collciv = sum(deaths_civilians)) %>%
      ungroup()

    ### Merge this into basedata data
    basedata <- left_join(basedata,nsvmonthly, by = c("conflict_id" = "fight_in_conf_id", "month_year"))

    # recode missing
    basedata <- basedata %>% mutate(interreb = if_else(is.na(interreb),0,interreb),
                            interreb_collciv = if_else(is.na(interreb_collciv),0,interreb_collciv))


    list(basedata = basedata, gedtrack = gedtrack)
  }

  # Run subfunctions sequentially
  # Step 1: Process battledeaths
  result_bd <- process_battledeaths()
  basedata <- result_bd$basedata
  gedtrack <- left_join(gedtrack, result_bd$gedtrack %>% select(idsplit, side_a_new_id, side_b_new_id, final_date, link_actor_type1),
                           by = c("idsplit", "side_a_new_id", "side_b_new_id", "final_date"))

  # Step 2: Process rebel OSV
  result_rebosv <- process_rebelosv()
  basedata <- left_join(basedata, result_rebosv$basedata %>% select(conflict_id, month_year, rebelosv),
                    by = c("conflict_id", "month_year"))
  gedtrack <- left_join(gedtrack, result_rebosv$gedtrack %>% select(idsplit, side_a_new_id, side_b_new_id, final_date, link_actor_rebosv),
                           by = c("idsplit", "side_a_new_id", "side_b_new_id", "final_date"))

  # Step 3: Process interrebel violence
  result_interreb <- process_interreb()
  basedata <- left_join(basedata, result_interreb$basedata %>% select(conflict_id, month_year, interreb, interreb_collciv),
                    by = c("conflict_id", "month_year"))
  gedtrack <- left_join(gedtrack, result_interreb$gedtrack %>% select(idsplit, side_a_new_id, side_b_new_id, final_date, link_actor_interreb),
                           by = c("idsplit", "side_a_new_id", "side_b_new_id", "final_date"))

  # Combine link indicators into a single column in gedtrack
  gedtrack <- gedtrack %>%
    mutate(actorlinked = if_else(
      (link_actor_type1 == 1 | link_actor_rebosv == 1 | link_actor_interreb == 1),
      1,
      0,
      missing = 0
    ))

  # Save updated datasets
  return(list(basedata = basedata, gedtrack = gedtrack))
}

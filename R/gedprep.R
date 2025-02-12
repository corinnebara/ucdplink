#' Takes UCDP GED and processes it to:
#' a) divide multi-month events into separate one-month events (fatalities evenly divided between them)
#' b) divide events by coalition actors to the coalition-constituting actors (fatalities evenly divided between them)
#'
#' @return UCDP GED processed (with more rows than the original)
#' @export
#'
#' @import dplyr tidyr stringr lubridate readr
#'
#' @examples
#' gedprep()
gedprep <- function() {

  # Use helper function to load GED, OSV, and NSV datasets needed in this code
  message("Step 1: Loading UCDP GED, as well as one-sided and non-state violence datasets...")
  ged <- downloaducdp("ged")
  osv <- downloaducdp("osv")
  nsv <- downloaducdp("nsv")
  osv_orig <- osv
  nsv_orig <- nsv

  # Turn GED into correct date format
  ged$date_start <- as.Date(ged$date_start)
  ged$date_end <- as.Date(ged$date_end)

  # Process OSV data
  osv <- osv %>%
    mutate(
      actor_id = as.integer(actor_id)  # Ensure actor_id is integer
    ) %>%
    select(actor_id, coalition_components, is_government_actor) %>%
    rename(
      osv_coalition_components = coalition_components,
      osv_is_gov_actor = is_government_actor
    ) %>%
    distinct(actor_id, .keep_all = TRUE)

  # Process NSV data
  nsv <- nsv %>%
    mutate(
      dyad_id = as.integer(dyad_id)  # Ensure dyad_id is integer
    ) %>%
    select(dyad_id, org, side_a_components, side_b_components) %>%
    rename(
      nsv_org = org,
      nsv_side_a_components = side_a_components,
      nsv_side_b_components = side_b_components
    ) %>%
    distinct(dyad_id, .keep_all = TRUE)

  # Merge OSV and NSV with GED
  ged <- ged %>%
    mutate(side_a_new_id = as.integer(side_a_new_id),
           dyad_new_id = as.integer(dyad_new_id)) %>%
    left_join(osv, by = c("side_a_new_id" = "actor_id")) %>%
    mutate(osv_is_gov_actor = ifelse(type_of_violence != 3, NA, osv_is_gov_actor),
           osv_coalition_components = ifelse(type_of_violence != 3, NA, osv_coalition_components)) %>%
    left_join(nsv, by = c("dyad_new_id" = "dyad_id")) %>%
    mutate(nsv_org = ifelse(type_of_violence != 2, NA, nsv_org),
           nsv_side_a_components = ifelse(type_of_violence != 2, NA, nsv_side_a_components),
           nsv_side_b_components = ifelse(type_of_violence != 2, NA, nsv_side_b_components))

  # Step 2: Split Events by Month
  message("Step 1 complete: Datasets loaded and pre-processed.")
  message("Step 2: Splitting multi-month events into separate rows... this may take a moment!")

  # Standardize start and end dates to the first of each month
  ged$date_start <- format(as.Date(ged$date_start, '%Y/%m/%d'), "%Y-%m-01")
  ged$date_end <- format(as.Date(ged$date_end, '%Y/%m/%d'), "%Y-%m-01")

  # Split events into separate rows for each month
  ged <- ged %>%
    mutate_at(vars(date_start, date_end), ymd) %>%
    group_by(id, date_start) %>%
    mutate(final_date = list(seq(date_start, date_end, by = "month"))) %>%
    unnest(cols = c(final_date))

  # Select only relevant variables
  ged <- ged %>%
    select(
      id, year, active_year, type_of_violence, conflict_dset_id, conflict_new_id,
      conflict_name, dyad_dset_id, dyad_new_id, dyad_name, side_a_new_id, side_a,
      side_b_new_id, side_b, where_prec, where_coordinates, latitude, longitude,
      geom_wkt, priogrid_gid, country, country_id, date_prec, date_start, date_end,
      deaths_civilians, best, gwnoa, gwnob, osv_coalition_components,
      osv_is_gov_actor, nsv_org, nsv_side_a_components, nsv_side_b_components,
      final_date
    )

  # Divide fatality variables by the number of months
  ged <- ged %>%
    group_by(id) %>%
    mutate(
      best = best / n(),
      deaths_civilians = deaths_civilians / n()
    ) %>%
    mutate(
      best = round(best, 2),                   # Round to 2 decimals
      deaths_civilians = round(deaths_civilians, 2)
    ) %>%
    ungroup()

  # Create a new unique ID for split events
  ged <- ged %>%
    mutate(one = 1) %>%
    group_by(id) %>%
    mutate(nummonths = cumsum(one)) %>%       # Count the number of months for each event ID
    unite("idsplit", id, nummonths, sep = "_", remove = FALSE) %>% # Create a new unique ID
    ungroup()

  # Step 3: Process GED for OSV and NSV
  message("Step 2 complete: Events split by month.")
  message("Step 3: Splitting events by coalition actors and dividing fatalities between them...")

  # Remove unnecessary columns
  ged <- ged %>% select(-c(one, nummonths))

  # Create subsets for types of violence
  ged1 <- ged %>% filter(type_of_violence == 1)
  ged2 <- ged %>% filter(type_of_violence == 2)
  ged3 <- ged %>% filter(type_of_violence == 3)

  # Use the original OSV dataset for other processing
  osv <- osv_orig %>%
    mutate(actor_id = as.character(actor_id))

  # Use the original NSV dataset for other processing
  nsv <- nsv_orig %>%
    mutate(dyad_id = as.character(dyad_id))

  ### Process OSV Data
  osv_coal <- osv %>% filter(coalition_components != "")

  # Prepare OSV subset
  osv_sm <- osv_coal %>%
    group_by(actor_id) %>%
    distinct(n() > 1, .keep_all = TRUE) %>%
    select(conflict_id, dyad_id, actor_id, coalition_components)

  # Reshape coalition actors
  osv_sm <- osv_sm %>%
    separate(coalition_components, into = paste0("a", 1:11), fill = "right") %>%
    rename(coalition_id = actor_id) %>%
    pivot_longer(
      cols = starts_with("a"),
      names_to = "actor_id",
      values_to = "side_a_id",
      values_drop_na = TRUE
    ) %>%
    mutate(side_a_id = as.character(side_a_id))

  # Add conflict ID by actor ID
  osv_final <- left_join(osv_sm, osv, by = c("side_a_id" = "actor_id"), relationship = "many-to-many") %>%
    mutate(
      conflict_id.x = as.character(conflict_id.x),
      conflict_id.y = as.character(conflict_id.y)
    ) %>%
    unite("crap_id", conflict_id.x, actor_id, remove = FALSE) %>%
    mutate(conflict_id.y = if_else(is.na(conflict_id.y), crap_id, conflict_id.y)) %>%
    select(conflict_id.y, coalition_id, actor_id, side_a_id) %>%
    group_by(coalition_id, actor_id) %>%
    distinct(n() > 1, .keep_all = TRUE)

  osv_final <- osv_final %>% mutate(coalosv = 1)

  # Merge OSV into ged3
  ged3 <- ged3 %>% mutate(side_a_new_id = as.character(side_a_new_id))
  ged3 <- left_join(ged3, osv_final, by = c("side_a_new_id" = "coalition_id"), relationship = "many-to-many") %>%
    mutate(
      side_a_new_id = if_else(!is.na(side_a_id), side_a_id, side_a_new_id),
      conflict_new_id = if_else(!is.na(side_a_id), as.character(conflict_id.y), as.character(conflict_new_id))
    ) %>%
    select(-c(conflict_id.y, actor_id, side_a_id, "n() > 1"))

  # Divide deaths evenly among coalition actors in OSV
  ged3 <- ged3 %>%
    group_by(idsplit) %>%
    mutate(
      best = best / n(),
      deaths_civilians = deaths_civilians / n()
    ) %>%
    ungroup()

  ### Process NSV Data
  nsv <- nsv %>%
    select(conflict_id, side_a_name, side_a_id, side_a_components, side_b_name, side_b_id, side_b_components, best_fatality_estimate) %>%
    filter(side_a_components != "" | side_b_components != "")

  # Prepare side A
  nsv_a <- nsv %>%
    filter(!side_a_components == "") %>%
    group_by(side_a_id) %>%
    distinct(n() > 1, .keep_all = TRUE) %>%
    select(conflict_id, side_a_id, side_a_components)

  nsv_sm <- nsv_a %>%
    separate(side_a_components, into = paste0("a", 1:12), fill = "right") %>%
    rename(coalition_id = side_a_id) %>%
    pivot_longer(
      cols = starts_with("a"),
      names_to = "actor_id",
      values_to = "side_a_id",
      values_drop_na = TRUE
    ) %>%
    unite("conflict_nsv_id", conflict_id, actor_id, sep = "a", remove = FALSE)

  # Prepare side B
  nsv_b <- nsv %>%
    filter(!side_b_components == "") %>%
    group_by(side_b_id) %>%
    distinct(n() > 1, .keep_all = TRUE) %>%
    select(conflict_id, side_b_id, side_b_components)

  nsv_sm_b <- nsv_b %>%
    separate(side_b_components, into = paste0("a", 1:10), fill = "right") %>%
    rename(coalition_id = side_b_id) %>%
    pivot_longer(
      cols = starts_with("a"),
      names_to = "actor_id",
      values_to = "side_b_id",
      values_drop_na = TRUE
    ) %>%
    unite("conflict_nsv_b_id", conflict_id, actor_id, sep = "b", remove = FALSE)

  # Merge NSV side A into ged2
  ged2 <- ged2 %>% mutate(side_a_new_id = as.character(side_a_new_id))
  nsv_sm <- nsv_sm %>% mutate(coalition_id = as.character(coalition_id))
  ged2 <- left_join(ged2, nsv_sm, by = c("side_a_new_id" = "coalition_id"), relationship = "many-to-many") %>%
    mutate(
      side_a_new_id = if_else(!is.na(side_a_id), side_a_id, side_a_new_id),
      conflict_new_id = if_else(!is.na(side_a_id), as.character(conflict_nsv_id), as.character(conflict_new_id))
    )

  # Merge NSV side B into ged2
  ged2 <- ged2 %>% mutate(side_b_new_id = as.character(side_b_new_id))
  nsv_sm_b <- nsv_sm_b %>% mutate(coalition_id = as.character(coalition_id))
  ged2 <- left_join(ged2, nsv_sm_b, by = c("side_b_new_id" = "coalition_id"), relationship = "many-to-many") %>%
    mutate(
      side_b_new_id = if_else(!is.na(side_b_id), side_b_id, side_b_new_id),
      conflict_new_id = if_else(!is.na(side_b_id), as.character(conflict_nsv_b_id), as.character(conflict_new_id))
    ) %>%
    select(-c(conflict_nsv_id, conflict_nsv_b_id, actor_id.x, actor_id.y, side_a_id, side_b_id))

  # Divide deaths evenly among coalition actors in NSV
  ged2 <- ged2 %>%
    group_by(idsplit) %>%
    mutate(
      best = best / n(),
      deaths_civilians = deaths_civilians / n()
    ) %>%
    ungroup()

  # Ensure same class in all subsets
  message("Step 3 complete: Coalition actors done.")
  message("Finishing up...")

  ged1 <- ged1 %>% mutate(
    side_a_new_id = as.character(side_a_new_id),
    conflict_new_id = as.character(conflict_new_id),
    side_b_new_id = as.character(side_b_new_id)
  )

  ged2 <- ged2 %>%
    mutate(
      side_a_new_id = as.character(side_a_new_id),
      conflict_new_id = as.character(conflict_new_id),
      side_b_new_id = as.character(side_b_new_id)
    ) %>%
    select(-c(conflict_id.x, conflict_id.y))

  ged3 <- ged3 %>%
    mutate(
      side_a_new_id = as.character(side_a_new_id),
      conflict_new_id = as.character(conflict_new_id),
      side_b_new_id = as.character(side_b_new_id)
    ) %>%
    select(-c(coalosv))

  # Combine variables back together
  ged <- bind_rows(ged1, ged2, ged3)

  # Drop some variables not needed in the next steps to make the dataset smaller
  ged <- ged %>%
    select(-active_year, -conflict_dset_id, -conflict_name, -dyad_dset_id,
           -dyad_name, -side_a, -side_b, -where_coordinates, -priogrid_gid, -country)

  # Save the final prepared GED (also as a stripped version that we later use to track how every event was linked)
  return(ged)
}

library(shiny)
library(rangeRs)
library(dplyr)
library(ggplot2)
library(caret)
library(tidyr)
library(scales)
library(gt)
library(stringr)
library(mlbplotR)
library(gridExtra)
library(shinyscreenshot)
library(shinycssloaders)
library(shinythemes)
library(DBI)
library(RPostgreSQL)

pitchers_evals_query <- "select *
from player_dev.r_shiny_pitch_grade_app_player_role_comp_tool"

pitchers_evals_w_mlb <- postgres_query(pitchers_evals_query, db = "aws") %>%
  mutate(across(CHASE_PCT:OS_LVA, ~ factor(.x, c("Awful", "Poor","Below Average","Average","Above Average","Great","Elite"))),
         across(HAS_FASTBALL:HAS_SCREW, ~ as.factor(.x))) %>%
  group_by(TXR_PLAYER_ID) %>%
  arrange(TXR_PLAYER_ID, MAX_GAME_DATE) %>%
  ungroup() %>%
  distinct()

pitchers_evals_w_mlb_scaled <- pitchers_evals_w_mlb %>%
  mutate(across(CHASE_PCT:OS_LVA, ~ as.numeric(.x)),
         START_PCT = ntile(START_PCT, n = 7))

possible_pitchers <- pitchers_evals_w_mlb_scaled %>%
  select(TXR_PLAYER_ID, PLAYER_NAME_DISPLAY_FL, YEAR, MAX_GAME_DATE, THROWS, AGE, COMPETITION_LEVEL_ABBREV, ORGANIZATIONS_ABBREV) %>%
  filter(COMPETITION_LEVEL_ABBREV != 'MLB')

possible_pitchers_names <- setNames(possible_pitchers$TXR_PLAYER_ID, possible_pitchers$PLAYER_NAME_DISPLAY_FL)

max_distance <- 1*9 + sqrt(6^2 + 6^2) + 6 + 6*12 + 6


find_similar_pitchers <- function(scaled_table, eval_table, pitcher_id, game_year, comp_level, max_distance) {
  
  target_pitcher <- scaled_table %>%
    filter(TXR_PLAYER_ID == pitcher_id & YEAR == game_year & COMPETITION_LEVEL_ABBREV == comp_level) %>%
    select(TXR_PLAYER_ID, PLAYER_NAME_DISPLAY_FL, YEAR, THROWS, AGE, COMPETITION_LEVEL_ABBREV) %>%
    filter(row_number() == n())
  
  pitcher <- scaled_table %>%
    filter(THROWS == target_pitcher$THROWS & AGE %in% c(target_pitcher$AGE-1, target_pitcher$AGE, target_pitcher$AGE+1) & COMPETITION_LEVEL_ABBREV == target_pitcher$COMPETITION_LEVEL_ABBREV)
  
  pitcher_test <- pitcher %>%
    select(-c(YEAR, COMPETITION_LEVEL_ABBREV, ORGANIZATIONS_ABBREV, TXR_PLAYER_ID, PLAYER_NAME_DISPLAY_FL, AGE, THROWS, MAX_GAME_DATE, PITCH_COUNT, REL_HEIGHT_TRUE, REL_SIDE_TRUE, EXTENSION_TRUE, major_service_years, major_service_days, G, GS, TBF, made_mlb, seasoned_mlb, MLB_PLAYER_ID:WAR_MLB)) 
  
  rel_distances <- as.matrix(dist(pitcher_test %>% select(REL_SIDE, REL_HEIGHT), method = "euclidean"))
  rel_df <- as.data.frame(rel_distances)
  rel_df$ID <- seq.int(nrow(rel_df))
  rel_point_column <- rel_df %>%
    rename(REL_POINT = which(grepl(pitcher_id, pitcher$TXR_PLAYER_ID) & grepl(target_pitcher$AGE, pitcher$AGE) & grepl(target_pitcher$COMPETITION_LEVEL_ABBREV, pitcher$COMPETITION_LEVEL_ABBREV))) %>%
    arrange(ID) %>%
    select(REL_POINT)
  
  pitcher_test <- cbind(pitcher_test, rel_point_column) %>%
    select(-c(REL_HEIGHT, REL_SIDE))
  
  distances <- dist(pitcher_test, method = "manhattan")
  dist_matrix <- as.matrix(distances)
  dist_df <- as.data.frame(dist_matrix)
  dist_df$ID <- seq.int(nrow(dist_df))
  
  target_column <- dist_df %>%
    rename(target = which(grepl(pitcher_id, pitcher$TXR_PLAYER_ID) & grepl(target_pitcher$AGE, pitcher$AGE) & grepl(target_pitcher$COMPETITION_LEVEL_ABBREV, pitcher$COMPETITION_LEVEL_ABBREV))) %>%
    select(ID, target) %>%
    mutate(target = round(rescale(target, to = c(0, 100), from = c(max_distance, 0)), 1)) %>%
    arrange(ID)
  
  eval_pitcher <- eval_table %>%
    filter(THROWS == target_pitcher$THROWS & AGE %in% c(target_pitcher$AGE-1, target_pitcher$AGE, target_pitcher$AGE+1) & COMPETITION_LEVEL_ABBREV == target_pitcher$COMPETITION_LEVEL_ABBREV)
  
  pitcher_w_similar <- cbind(eval_pitcher, target_column) %>%
    group_by(TXR_PLAYER_ID) %>%
    arrange(desc(target)) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    filter(YEAR < game_year | TXR_PLAYER_ID == pitcher_id)
  
  return(pitcher_w_similar)
}

find_10_closest_pitchers <- function(similar_pitchers, pitcher_id) {
  
  pitcher_10_closest_mlb_pitchers <- similar_pitchers %>%
    select(-c(G, GS, TBF)) %>%
    filter(seasoned_mlb == 1 & TXR_PLAYER_ID != pitcher_id) %>%
    filter(
      if (n() >= 10 & target[10] >= 85) {
        target >= 85
      }
      else {
        row_number() <= 10
      }
    ) %>%
    filter(row_number() <= 20)
  
  top_10_ids <- as.character(pitcher_10_closest_mlb_pitchers$TXR_PLAYER_ID)
  
  top_10_w_stats <- pitcher_10_closest_mlb_pitchers %>%
    filter(TXR_PLAYER_ID %in% top_10_ids) %>%
    mutate(ROLE = ifelse(GS_MLB / G_MLB >= 0.6, 'Starter', ifelse(GS_MLB / G_MLB < 0.6 & GS_MLB / G_MLB >= 0.3, 'Swingman', 'Reliever'))) %>%
    arrange(desc(target))
  
  return(top_10_w_stats)
}

find_top_25_big_league_pct <- function(similar_pitchers, pitcher_id) {
  
  pitcher_top_25_big_league_pct <- similar_pitchers %>%
    filter(TXR_PLAYER_ID != pitcher_id) %>%
    arrange(desc(target)) %>%
    filter(row_number() <= 25) %>%
    summarise(made_mlb_pct = sum(made_mlb) / n())
  
  return(pitcher_top_25_big_league_pct)
}

find_similar_big_league_pct <- function(similar_pitchers, pitcher_id) {
  
  pitcher_similar_big_league_pct <- similar_pitchers %>%
    filter(TXR_PLAYER_ID != pitcher_id) %>%
    summarise(made_mlb_pct = sum(made_mlb) / n())
  
  return(pitcher_similar_big_league_pct)
}

ten_closest_prcs <- function(similar_pitchers, pitchers_evals_w_mlb, ten_closest, pitcher_id) {
  
  similar_pitchers_traits <- similar_pitchers %>%
    filter(seasoned_mlb == 1 | TXR_PLAYER_ID == pitcher_id) %>%
    filter(row_number() <= nrow(ten_closest) + 1) %>%
    mutate(across(c(HAS_FASTBALL:HAS_SCREW, CHASE_PCT:OS_LVA), ~ as.factor(.x)),
           across(CHASE_PCT:OS_LVA, ~ as.numeric(.x))) %>%
    mutate(CHASE_DIST = CHASE_PCT - first(CHASE_PCT),
           K_DIST = K_RATE - first(K_RATE),
           BB_DIST = BB_RATE - first(BB_RATE),
           FPS_DIST = FPS_RATE - first(FPS_RATE),
           GB_DIST = GB_PCT - first(GB_PCT),
           XWOBA_DIST = XWOBA - first(XWOBA),
           FB_STUFF_DIST = FB_STUFF - first(FB_STUFF),
           BB_STUFF_DIST = BB_STUFF - first(BB_STUFF),
           OS_STUFF_DIST = OS_STUFF - first(OS_STUFF),
           FB_LVA_DIST = FB_LVA - first(FB_LVA),
           BB_LVA_DIST = BB_LVA - first(BB_LVA),
           OS_LVA_DIST = OS_LVA - first(OS_LVA)
    ) %>%
    filter(TXR_PLAYER_ID != pitcher_id) %>%
    mutate(SIMILARITY = ifelse(CHASE_DIST == 0, "Chase", "")) %>%
    mutate(SIMILARITY = ifelse(K_DIST == 0, paste(SIMILARITY, "K", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse(BB_DIST == 0, paste(SIMILARITY, "BB", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse(FPS_DIST == 0, paste(SIMILARITY, "FPS", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse(GB_DIST == 0, paste(SIMILARITY, "GB", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse(XWOBA_DIST == 0, paste(SIMILARITY, "xwOBA", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse(!is.na(FB_STUFF_DIST) & FB_STUFF_DIST == 0 & !is.na(BB_STUFF_DIST) & BB_STUFF_DIST == 0 & !is.na(OS_STUFF_DIST) & OS_STUFF_DIST == 0, paste(SIMILARITY, "STUFF (FB,BB,OS)", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse(!is.na(FB_STUFF_DIST) & FB_STUFF_DIST == 0 & !is.na(BB_STUFF_DIST) & BB_STUFF_DIST == 0 & (is.na(OS_STUFF_DIST) | OS_STUFF_DIST != 0), paste(SIMILARITY, "STUFF (FB,BB)", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse(!is.na(FB_STUFF_DIST) & FB_STUFF_DIST == 0 & (is.na(BB_STUFF_DIST) | BB_STUFF_DIST != 0) & !is.na(OS_STUFF_DIST) & OS_STUFF_DIST == 0, paste(SIMILARITY, "STUFF (FB,OS)", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse((is.na(FB_STUFF_DIST) | FB_STUFF_DIST != 0) & !is.na(BB_STUFF_DIST) & BB_STUFF_DIST == 0 & !is.na(OS_STUFF_DIST) & OS_STUFF_DIST == 0, paste(SIMILARITY, "STUFF (BB,OS)", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse(!is.na(FB_STUFF_DIST) & FB_STUFF_DIST == 0 & (is.na(BB_STUFF_DIST) | BB_STUFF_DIST != 0) & (is.na(OS_STUFF_DIST) | OS_STUFF_DIST != 0), paste(SIMILARITY, "FB STUFF", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse((is.na(FB_STUFF_DIST) | FB_STUFF_DIST != 0) & !is.na(BB_STUFF_DIST) & BB_STUFF_DIST == 0 & (is.na(OS_STUFF_DIST) | OS_STUFF_DIST != 0), paste(SIMILARITY, "BB STUFF", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse((is.na(FB_STUFF_DIST) | FB_STUFF_DIST != 0) & (is.na(BB_STUFF_DIST) | BB_STUFF_DIST != 0) & !is.na(OS_STUFF_DIST) & OS_STUFF_DIST == 0, paste(SIMILARITY, "OS STUFF", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse(!is.na(FB_LVA_DIST) & FB_LVA_DIST == 0 & !is.na(BB_LVA_DIST) &  BB_LVA_DIST == 0 & !is.na(OS_LVA_DIST) & OS_LVA_DIST == 0, paste(SIMILARITY, "LVA (FB,BB,OS)", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse(!is.na(FB_LVA_DIST) & FB_LVA_DIST == 0 & !is.na(BB_LVA_DIST) & BB_LVA_DIST == 0 & (is.na(OS_LVA_DIST) | OS_LVA_DIST != 0), paste(SIMILARITY, "LVA (FB,BB)", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse(!is.na(FB_LVA_DIST) & FB_LVA_DIST == 0 & (is.na(BB_LVA_DIST) | BB_LVA_DIST != 0) & !is.na(OS_LVA_DIST) & OS_LVA_DIST == 0, paste(SIMILARITY, "LVA (FB,OS)", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse((is.na(FB_LVA_DIST) | FB_LVA_DIST != 0) & !is.na(BB_LVA_DIST) & BB_LVA_DIST == 0 & !is.na(OS_LVA_DIST) & OS_LVA_DIST == 0, paste(SIMILARITY, "LVA (BB,OS)", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse(!is.na(FB_LVA_DIST) & FB_LVA_DIST == 0 & (is.na(BB_LVA_DIST) | BB_LVA_DIST != 0) & (is.na(OS_LVA_DIST) | OS_LVA_DIST != 0), paste(SIMILARITY, "FB LVA", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse((is.na(FB_LVA_DIST) | FB_LVA_DIST != 0) & !is.na(BB_LVA_DIST) & BB_LVA_DIST == 0 & (is.na(OS_LVA_DIST) | OS_LVA_DIST != 0), paste(SIMILARITY, "BB LVA", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse((is.na(FB_LVA_DIST) | FB_LVA_DIST != 0) & (is.na(BB_LVA_DIST) | BB_LVA_DIST != 0) & !is.na(OS_LVA_DIST) & OS_LVA_DIST == 0, paste(SIMILARITY, "OS LVA", sep = ", "), SIMILARITY)) %>%
    mutate(SIMILARITY = ifelse(substring(SIMILARITY, 1, 1) == ",", substring(SIMILARITY, 3), SIMILARITY)) %>%
    mutate(BETTER = ifelse(CHASE_DIST > 0, "Chase", "")) %>%
    mutate(BETTER = ifelse(K_DIST > 0, paste(BETTER, "K", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse(BB_DIST > 0, paste(BETTER, "BB", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse(FPS_DIST > 0, paste(BETTER, "FPS", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse(GB_DIST > 0, paste(BETTER, "GB", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse(XWOBA_DIST > 0, paste(BETTER, "xwOBA", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse(!is.na(FB_STUFF_DIST) & FB_STUFF_DIST > 0 & !is.na(BB_STUFF_DIST) & BB_STUFF_DIST > 0 & !is.na(OS_STUFF_DIST) & OS_STUFF_DIST > 0, paste(BETTER, "STUFF (FB,BB,OS)", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse(!is.na(FB_STUFF_DIST) & FB_STUFF_DIST > 0 & !is.na(BB_STUFF_DIST) & BB_STUFF_DIST > 0 & (is.na(OS_STUFF_DIST) | OS_STUFF_DIST <= 0), paste(BETTER, "STUFF (FB,BB)", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse(!is.na(FB_STUFF_DIST) & FB_STUFF_DIST > 0 & (is.na(BB_STUFF_DIST) | BB_STUFF_DIST <= 0) & !is.na(OS_STUFF_DIST) & OS_STUFF_DIST > 0, paste(BETTER, "STUFF (FB,OS)", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse((is.na(FB_STUFF_DIST) | FB_STUFF_DIST <= 0) & !is.na(BB_STUFF_DIST) & BB_STUFF_DIST > 0 & !is.na(OS_STUFF_DIST) & OS_STUFF_DIST > 0, paste(BETTER, "STUFF (BB,OS)", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse(!is.na(FB_STUFF_DIST) & FB_STUFF_DIST > 0 & (is.na(BB_STUFF_DIST) | BB_STUFF_DIST <= 0) & (is.na(OS_STUFF_DIST) | OS_STUFF_DIST <= 0), paste(BETTER, "FB STUFF", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse((is.na(FB_STUFF_DIST) | FB_STUFF_DIST <= 0) & !is.na(BB_STUFF_DIST) & BB_STUFF_DIST > 0 & (is.na(OS_STUFF_DIST) | OS_STUFF_DIST <= 0), paste(BETTER, "BB STUFF", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse((is.na(FB_STUFF_DIST) | FB_STUFF_DIST <= 0) & (is.na(BB_STUFF_DIST) | BB_STUFF_DIST <= 0) & !is.na(OS_STUFF_DIST) & OS_STUFF_DIST > 0, paste(BETTER, "OS STUFF", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse(!is.na(FB_LVA_DIST) & FB_LVA_DIST > 0 & !is.na(BB_LVA_DIST) & BB_LVA_DIST > 0 & !is.na(OS_LVA_DIST) & OS_LVA_DIST > 0, paste(BETTER, "LVA (FB,BB,OS)", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse(!is.na(FB_LVA_DIST) & FB_LVA_DIST > 0 & !is.na(BB_LVA_DIST) & BB_LVA_DIST > 0 & (is.na(OS_LVA_DIST) | OS_LVA_DIST <= 0), paste(BETTER, "LVA (FB,BB)", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse(!is.na(FB_LVA_DIST) & FB_LVA_DIST > 0 & (is.na(BB_LVA_DIST) | BB_LVA_DIST <= 0) & !is.na(OS_LVA_DIST) & OS_LVA_DIST > 0, paste(BETTER, "LVA (FB,OS)", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse((is.na(FB_LVA_DIST) | FB_LVA_DIST <= 0) & !is.na(BB_LVA_DIST) & BB_LVA_DIST > 0 & !is.na(OS_LVA_DIST) & OS_LVA_DIST > 0, paste(BETTER, "LVA (BB,OS)", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse(!is.na(FB_LVA_DIST) & FB_LVA_DIST > 0 & (is.na(BB_LVA_DIST) | BB_LVA_DIST <= 0) & (is.na(OS_LVA_DIST) | OS_LVA_DIST <= 0), paste(BETTER, "FB LVA", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse((is.na(FB_LVA_DIST) | FB_LVA_DIST <= 0) & !is.na(BB_LVA_DIST) & BB_LVA_DIST > 0 & (is.na(OS_LVA_DIST) | OS_LVA_DIST <= 0), paste(BETTER, "BB LVA", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse((is.na(FB_LVA_DIST) | FB_LVA_DIST <= 0) & (is.na(BB_LVA_DIST) | BB_LVA_DIST <= 0) & !is.na(OS_LVA_DIST) & OS_LVA_DIST > 0, paste(BETTER, "OS LVA", sep = ", "), BETTER)) %>%
    mutate(BETTER = ifelse(substring(BETTER, 1, 1) == ",", substring(BETTER, 3), BETTER)) %>%
    mutate(WORSE = ifelse(CHASE_DIST < 0, "Chase", "")) %>%
    mutate(WORSE = ifelse(K_DIST < 0, paste(WORSE, "K", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse(BB_DIST < 0, paste(WORSE, "BB", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse(FPS_DIST < 0, paste(WORSE, "FPS", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse(GB_DIST < 0, paste(WORSE, "GB", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse(XWOBA_DIST < 0, paste(WORSE, "xwOBA", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse(!is.na(FB_STUFF_DIST) & FB_STUFF_DIST < 0 & !is.na(BB_STUFF_DIST) & BB_STUFF_DIST < 0 & !is.na(OS_STUFF_DIST) & OS_STUFF_DIST < 0, paste(WORSE, "STUFF (FB,BB,OS)", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse(!is.na(FB_STUFF_DIST) & FB_STUFF_DIST < 0 & !is.na(BB_STUFF_DIST) & BB_STUFF_DIST < 0 & (is.na(OS_STUFF_DIST) | OS_STUFF_DIST >= 0), paste(WORSE, "STUFF (FB,BB)", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse(!is.na(FB_STUFF_DIST) & FB_STUFF_DIST < 0 & (is.na(BB_STUFF_DIST) | BB_STUFF_DIST >= 0) & !is.na(OS_STUFF_DIST) & OS_STUFF_DIST < 0, paste(WORSE, "STUFF (FB,OS)", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse((is.na(FB_STUFF_DIST) | FB_STUFF_DIST >= 0) & !is.na(BB_STUFF_DIST) & BB_STUFF_DIST < 0 & !is.na(OS_STUFF_DIST) & OS_STUFF_DIST < 0, paste(WORSE, "STUFF (BB,OS)", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse(!is.na(FB_STUFF_DIST) & FB_STUFF_DIST < 0 & (is.na(BB_STUFF_DIST) | BB_STUFF_DIST >= 0) & (is.na(OS_STUFF_DIST) | OS_STUFF_DIST >= 0), paste(WORSE, "FB STUFF", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse((is.na(FB_STUFF_DIST) | FB_STUFF_DIST >= 0) & !is.na(BB_STUFF_DIST) & BB_STUFF_DIST < 0 & (is.na(OS_STUFF_DIST) | OS_STUFF_DIST >= 0), paste(WORSE, "BB STUFF", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse((is.na(FB_STUFF_DIST) | FB_STUFF_DIST >= 0) & (is.na(BB_STUFF_DIST) | BB_STUFF_DIST >= 0) & !is.na(OS_STUFF_DIST) & OS_STUFF_DIST < 0, paste(WORSE, "OS STUFF", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse(!is.na(FB_LVA_DIST) & FB_LVA_DIST < 0 & !is.na(BB_LVA_DIST) & BB_LVA_DIST < 0 & !is.na(OS_LVA_DIST) & OS_LVA_DIST < 0, paste(WORSE, "LVA (FB,BB,OS)", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse(!is.na(FB_LVA_DIST) & FB_LVA_DIST < 0 & !is.na(BB_LVA_DIST) & BB_LVA_DIST < 0 & (is.na(OS_LVA_DIST) | OS_LVA_DIST >= 0), paste(WORSE, "LVA (FB,BB)", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse(!is.na(FB_LVA_DIST) & FB_LVA_DIST < 0 & (is.na(BB_LVA_DIST) | BB_LVA_DIST >= 0) & !is.na(OS_LVA_DIST) & OS_LVA_DIST < 0, paste(WORSE, "LVA (FB,OS)", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse((is.na(FB_LVA_DIST) | FB_LVA_DIST >= 0) & !is.na(BB_LVA_DIST) & BB_LVA_DIST < 0 & !is.na(OS_LVA_DIST) & OS_LVA_DIST < 0, paste(WORSE, "LVA (BB,OS)", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse(!is.na(FB_LVA_DIST) & FB_LVA_DIST < 0 & (is.na(BB_LVA_DIST) | BB_LVA_DIST >= 0) & (is.na(OS_LVA_DIST) | OS_LVA_DIST >= 0), paste(WORSE, "FB LVA", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse((is.na(FB_LVA_DIST) | FB_LVA_DIST >= 0) & !is.na(BB_LVA_DIST) & BB_LVA_DIST < 0 & (is.na(OS_LVA_DIST) | OS_LVA_DIST >= 0), paste(WORSE, "BB LVA", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse((is.na(FB_LVA_DIST) | FB_LVA_DIST >= 0) & (is.na(BB_LVA_DIST) | BB_LVA_DIST >= 0) & !is.na(OS_LVA_DIST) & OS_LVA_DIST < 0, paste(WORSE, "OS LVA", sep = ", "), WORSE)) %>%
    mutate(WORSE = ifelse(substring(WORSE, 1, 1) == ",", substring(WORSE, 3), WORSE)) %>%
    select(TXR_PLAYER_ID, PLAYER_NAME_DISPLAY_FL, YEAR, COMPETITION_LEVEL_ABBREV, ORGANIZATIONS_ABBREV, AGE, THROWS, SIMILARITY, BETTER, WORSE, target)
  
  similar_pitchers_traits <- merge(similar_pitchers_traits, pitchers_evals_w_mlb, by = c("TXR_PLAYER_ID", "PLAYER_NAME_DISPLAY_FL", "YEAR", "COMPETITION_LEVEL_ABBREV", "ORGANIZATIONS_ABBREV", "AGE", "THROWS")) %>%
    mutate(PITCH_TYPES = ifelse(HAS_FASTBALL == 1, "FB", "")) %>%
    mutate(PITCH_TYPES = ifelse(HAS_SINKER == 1, paste(PITCH_TYPES, "SI", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_SLIDER == 1, paste(PITCH_TYPES, "SL", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_CURVE == 1, paste(PITCH_TYPES, "CU", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_CHANGE == 1, paste(PITCH_TYPES, "CH", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_SPLITTER == 1, paste(PITCH_TYPES, "SP", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_CUTTER == 1, paste(PITCH_TYPES, "CT", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_KNUCK == 1, paste(PITCH_TYPES, "KN", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_SCREW == 1, paste(PITCH_TYPES, "SC", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(substring(PITCH_TYPES, 1, 1) == ",", substring(PITCH_TYPES, 3), PITCH_TYPES)) %>%
    arrange(desc(target)) %>%
    select(TXR_PLAYER_ID, PLAYER_NAME_DISPLAY_FL, YEAR, COMPETITION_LEVEL_ABBREV, ORGANIZATIONS_ABBREV, AGE, THROWS, REL_HEIGHT_TRUE, REL_SIDE_TRUE, EXTENSION_TRUE, PITCH_TYPES, SIMILARITY, BETTER, WORSE, target)
  
  ten_closest_careers <- pitchers_evals_w_mlb %>%
    filter(TXR_PLAYER_ID %in% ten_closest$TXR_PLAYER_ID) %>%
    filter(COMPETITION_LEVEL_ABBREV == 'MLB') %>%
    group_by(TXR_PLAYER_ID) %>%
    arrange(TXR_PLAYER_ID, YEAR) %>%
    filter(row_number() == n()) %>%
    ungroup()
  
  ten_closest_together <- rbind(ten_closest_careers %>% select(TXR_PLAYER_ID:OS_LVA, START_PCT), ten_closest %>% select(TXR_PLAYER_ID:OS_LVA, START_PCT)) %>%
    arrange(TXR_PLAYER_ID, YEAR, COMPETITION_LEVEL_ABBREV) %>%
    mutate(across(HAS_FASTBALL:OS_LVA, ~ as.numeric(.x))) %>%
    group_by(TXR_PLAYER_ID) %>%
    summarise(across(HAS_FASTBALL:START_PCT, ~ .x - lag(.x)), .groups = "drop") %>%
    filter(!is.na(START_PCT))
  
  ten_closest_changes <- ten_closest_together %>%
    mutate(PITCHES_ADDED = ifelse(HAS_FASTBALL == 1, "FB", "")) %>%
    mutate(PITCHES_ADDED = ifelse(HAS_SINKER == 1, paste(PITCHES_ADDED, "SI", sep = ", "), PITCHES_ADDED)) %>%
    mutate(PITCHES_ADDED = ifelse(HAS_SLIDER == 1, paste(PITCHES_ADDED, "SL", sep = ", "), PITCHES_ADDED)) %>%
    mutate(PITCHES_ADDED = ifelse(HAS_CURVE == 1, paste(PITCHES_ADDED, "CU", sep = ", "), PITCHES_ADDED)) %>%
    mutate(PITCHES_ADDED = ifelse(HAS_CHANGE == 1, paste(PITCHES_ADDED, "CH", sep = ", "), PITCHES_ADDED)) %>%
    mutate(PITCHES_ADDED = ifelse(HAS_SPLITTER == 1, paste(PITCHES_ADDED, "SP", sep = ", "), PITCHES_ADDED)) %>%
    mutate(PITCHES_ADDED = ifelse(HAS_CUTTER == 1, paste(PITCHES_ADDED, "CT", sep = ", "), PITCHES_ADDED)) %>%
    mutate(PITCHES_ADDED = ifelse(HAS_KNUCK == 1, paste(PITCHES_ADDED, "KN", sep = ", "), PITCHES_ADDED)) %>%
    mutate(PITCHES_ADDED = ifelse(HAS_SCREW == 1, paste(PITCHES_ADDED, "SC", sep = ", "), PITCHES_ADDED)) %>%
    mutate(PITCHES_ADDED = ifelse(substring(PITCHES_ADDED, 1, 1) == ",", substring(PITCHES_ADDED, 3), PITCHES_ADDED)) %>%
    mutate(PITCHES_DROPPED = ifelse(HAS_FASTBALL == -1, "FB", "")) %>%
    mutate(PITCHES_DROPPED = ifelse(HAS_SINKER == -1, paste(PITCHES_DROPPED, "SI", sep = ", "), PITCHES_DROPPED)) %>%
    mutate(PITCHES_DROPPED = ifelse(HAS_SLIDER == -1, paste(PITCHES_DROPPED, "SL", sep = ", "), PITCHES_DROPPED)) %>%
    mutate(PITCHES_DROPPED = ifelse(HAS_CURVE == -1, paste(PITCHES_DROPPED, "CU", sep = ", "), PITCHES_DROPPED)) %>%
    mutate(PITCHES_DROPPED = ifelse(HAS_CHANGE == -1, paste(PITCHES_DROPPED, "CH", sep = ", "), PITCHES_DROPPED)) %>%
    mutate(PITCHES_DROPPED = ifelse(HAS_SPLITTER == -1, paste(PITCHES_DROPPED, "SP", sep = ", "), PITCHES_DROPPED)) %>%
    mutate(PITCHES_DROPPED = ifelse(HAS_CUTTER == -1, paste(PITCHES_DROPPED, "CT", sep = ", "), PITCHES_DROPPED)) %>%
    mutate(PITCHES_DROPPED = ifelse(HAS_KNUCK == -1, paste(PITCHES_DROPPED, "KN", sep = ", "), PITCHES_DROPPED)) %>%
    mutate(PITCHES_DROPPED = ifelse(HAS_SCREW == -1, paste(PITCHES_DROPPED, "SC", sep = ", "), PITCHES_DROPPED)) %>%
    mutate(PITCHES_DROPPED = ifelse(substring(PITCHES_DROPPED, 1, 1) == ",", substring(PITCHES_DROPPED, 3), PITCHES_DROPPED)) %>%
    summarise(TXR_PLAYER_ID, PITCHES_ADDED, PITCHES_DROPPED)
  
  ten_closest_stats <- merge(ten_closest, ten_closest_changes, by = "TXR_PLAYER_ID")
  
  ten_closest_stats <- ten_closest_stats %>%
    arrange(desc(target)) %>%
    select(TXR_PLAYER_ID, MLB_PLAYER_ID:WAR_MLB, PITCHES_ADDED, PITCHES_DROPPED)
  
  top_ten_combined <- merge(similar_pitchers_traits, ten_closest_stats, by = "TXR_PLAYER_ID") %>%
    relocate(MLB_PLAYER_ID) %>%
    select(-c(TXR_PLAYER_ID, ORGANIZATIONS_ABBREV))
  
  return(top_ten_combined)

}

ten_closest_summary_funct <- function(top_ten_combined) {
  
  ten_closest_summary_row <- top_ten_combined %>%
    summarise(PLAYER_NAME_DISPLAY_FL = "TOTALS",
              across(YEAR:target, ~ "-"),
              G_MLB = sum(G_MLB),
              GS_MLB = sum(GS_MLB),
              FIP_MLB = round(weighted.mean(FIP_MLB, convert_innings(IP_MLB, "to_actual_num")), 2),
              SO_RATE_MLB = round(weighted.mean(SO_RATE_MLB, as.numeric(TBF_MLB)), 1),
              WALK_RATE_MLB = round(weighted.mean(WALK_RATE_MLB, as.numeric(TBF_MLB)), 1),
              GB_RATE_MLB = round(weighted.mean(GB_RATE_MLB, as.numeric(TBF_MLB)), 1),
              WAR_MLB = "-",
              PITCHES_ADDED = "-",
              PITCHES_DROPPED = "-",
              IP_MLB = convert_innings(sum(convert_innings(IP_MLB, "to_actual_num")), "to_display_format")) %>%
    relocate(c(IP_MLB), .after = GS_MLB)
  
  ten_closest_summary_table <- ten_closest_summary_row %>%
    gt() %>%
    tab_options(table.font.size = 11) %>%
    cols_label(
      PLAYER_NAME_DISPLAY_FL = "",
      YEAR = "",
      COMPETITION_LEVEL_ABBREV = "",
      AGE = "",
      THROWS = "",
      REL_HEIGHT_TRUE = "",
      REL_SIDE_TRUE = "",
      EXTENSION_TRUE = "",
      PITCH_TYPES = "",
      SIMILARITY = "",
      BETTER = "",
      WORSE = "",
      target = "",
      G_MLB = "",
      GS_MLB = "",
      IP_MLB = "",
      FIP_MLB = "",
      SO_RATE_MLB = "",
      WALK_RATE_MLB = "",
      GB_RATE_MLB = "",
      WAR_MLB = "",
      PITCHES_ADDED = "",
      PITCHES_DROPPED = "") %>%
    gtExtras::gt_highlight_cols(
      columns = c(2,4,6,8,10,12,14,16,18,20,22),
      fill = "lightgray",
      alpha = 0.5) %>%
    gtExtras::gt_highlight_cols(
      columns = 1,
      fill = "#d4f7f7",
      alpha = 0.5
    ) %>%
    gt_add_vertical_line(
      columns = target) %>%
    cols_align("center") %>%
    cols_width(PLAYER_NAME_DISPLAY_FL ~ px(90),
               YEAR ~ px(40),
               COMPETITION_LEVEL_ABBREV ~ px(43),
               AGE ~ px(37),
               THROWS ~ px(54),
               REL_HEIGHT_TRUE ~ px(52),
               REL_SIDE_TRUE ~ px(39),
               EXTENSION_TRUE ~ px(67),
               PITCH_TYPES ~ px(71),
               SIMILARITY ~ px(80),
               BETTER ~ px(75),
               WORSE ~ px(75),
               target ~ px(70),
               G_MLB ~ px(40),
               GS_MLB ~ px(40),
               IP_MLB ~ px(50),
               FIP_MLB ~ px(38),
               SO_RATE_MLB ~ px(38),
               WALK_RATE_MLB ~ px(40),
               GB_RATE_MLB ~ px(42),
               WAR_MLB ~ px(42),
               PITCHES_ADDED ~ px(54),
               PITCHES_DROPPED ~ px(62)
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = PLAYER_NAME_DISPLAY_FL
      )
    ) %>%
    opt_interactive(
      use_sorting = FALSE,
      use_pagination = FALSE
    )
  
  return(ten_closest_summary_table)
    
}

ten_closest_table_funct <- function(top_ten_combined) {
  top_ten_stats_summary <- top_ten_combined %>%
    select(-c(TBF_MLB, PITCH_MLB)) %>%
    mutate(target = round(target)) %>%
    arrange(desc(target))
  
  # top_ten_stats_summary <- rbind(top_ten_combined, ten_closest_summary_row)
  
  ten_closest_table <- top_ten_stats_summary %>%
    gt() %>%
    tab_header(title = md("**Closest MLB Pitchers**"),
               subtitle = md("*min. 100 BF at MLB*")) %>%
    cols_label(
               PLAYER_NAME_DISPLAY_FL = md("**Pitcher**"),
               YEAR = md("**Year**"),
               COMPETITION_LEVEL_ABBREV = md("**Level**"),
               AGE = md("**Age**"),
               THROWS = md("**Throws**"),
               REL_HEIGHT_TRUE = md("**Rel Height**"),
               REL_SIDE_TRUE = md("**Rel Side**"),
               EXTENSION_TRUE = md("**Extension**"),
               PITCH_TYPES = md("**Repertoire**"),
               SIMILARITY = md("**Similar KPIs**"),
               BETTER = md("**Better KPIs**"),
               WORSE = md("**Worse KPIs**"),
               target = md("**Similarity %**"),
               G_MLB = md("**G**"),
               GS_MLB = md("**GS**"),
               IP_MLB = md("**IP**"),
               FIP_MLB = md("**FIP**"),
               SO_RATE_MLB = md("**K%**"),
               WALK_RATE_MLB = md("**BB%**"),
               GB_RATE_MLB = md("**GB%**"),
               WAR_MLB = md("**WAR**"),
               PITCHES_ADDED = md("**Pitches Added**"),
               PITCHES_DROPPED = md("**Pitches Dropped**")) %>%
    tab_options(table.font.size = 11) %>%
    tab_spanner(
      label = "At Same Point In Career",
      columns = c(YEAR:target)
    ) %>%
    tab_spanner(
      label = "MLB Career Statistics",
      columns = c(G_MLB:PITCHES_DROPPED)
    ) %>%
    gt_add_vertical_line(
      columns = target) %>%
    #gt_fmt_mlb_dot_headshot(columns = MLB_PLAYER_ID) %>%
    gtExtras::gt_highlight_cols(
      columns = c(3,5,7,9,11,13,15,17,19,21,23),
      fill = "lightgray",
      alpha = 0.5) %>%
    gtExtras::gt_highlight_cols(
      columns = 2,
      fill = "#d4f7f7",
      alpha = 0.5
    ) %>%
    tab_style(style = cell_borders(sides = c("top", "bottom"),
                                   color = "lightgrey",
                                   weight = px(1)),
              locations = cells_body()
    ) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(
                columns = PLAYER_NAME_DISPLAY_FL)) %>%
    tab_style(style = cell_text(size = "medium"),
              locations = cells_body(
                columns = target)) %>%
    cols_align("center") %>%
    cols_width(PLAYER_NAME_DISPLAY_FL ~ px(90),
               YEAR ~ px(40),
               COMPETITION_LEVEL_ABBREV ~ px(43),
               AGE ~ px(37),
               THROWS ~ px(54),
               REL_HEIGHT_TRUE ~ px(52),
               REL_SIDE_TRUE ~ px(39),
               EXTENSION_TRUE ~ px(67),
               PITCH_TYPES ~ px(71),
               SIMILARITY ~ px(80),
               BETTER ~ px(75),
               WORSE ~ px(75),
               target ~ px(70),
               G_MLB ~ px(40),
               GS_MLB ~ px(40),
               IP_MLB ~ px(50),
               FIP_MLB ~ px(38),
               SO_RATE_MLB ~ px(38),
               WALK_RATE_MLB ~ px(40),
               GB_RATE_MLB ~ px(42),
               WAR_MLB ~ px(42),
               PITCHES_ADDED ~ px(54),
               PITCHES_DROPPED ~ px(62)
    ) %>%
    cols_hide(MLB_PLAYER_ID) %>%
    # gt_color_pill(
    #   columns = target,
    #   domain = c(50,100),
    #   midpoint = 75,
    #   palette = c("#D22D49", "white", "#1DBE3A")
    # ) %>%
    data_color(
      columns = target,
      domain = c(60,100),
      palette = c("#D22D49", "white", "#1DBE3A")
    ) %>%
    opt_interactive(
      use_compact_mode = TRUE
    )
  
  return(ten_closest_table)
}


focus_pitcher_table <- function(similar_pitchers, pitcher_id) {
  focus_pitcher <- similar_pitchers %>%
    filter(TXR_PLAYER_ID == pitcher_id) %>%
    filter(target == 100) %>%
    select(-c(REL_HEIGHT, REL_SIDE, EXTENSION))
  
  focus_pitcher <- focus_pitcher %>%
    mutate(PITCH_TYPES = ifelse(HAS_FASTBALL == 1, "FB", "")) %>%
    mutate(PITCH_TYPES = ifelse(HAS_SINKER == 1, paste(PITCH_TYPES, "SI", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_SLIDER == 1, paste(PITCH_TYPES, "SL", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_CURVE == 1, paste(PITCH_TYPES, "CU", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_CHANGE == 1, paste(PITCH_TYPES, "CH", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_SPLITTER == 1, paste(PITCH_TYPES, "SP", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_CUTTER == 1, paste(PITCH_TYPES, "CT", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_KNUCK == 1, paste(PITCH_TYPES, "KN", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(HAS_SCREW == 1, paste(PITCH_TYPES, "SC", sep = ", "), PITCH_TYPES)) %>%
    mutate(PITCH_TYPES = ifelse(substring(PITCH_TYPES, 1, 1) == ",", substring(PITCH_TYPES, 3), PITCH_TYPES))
  
  focus_pitcher_wider <- focus_pitcher %>%
    select(YEAR:AGE, CHASE_PCT:OS_LVA) %>%
    pivot_longer(cols = c(CHASE_PCT:OS_LVA),
                 names_to = "KPI",
                 values_to = "Rank") %>%
    select(KPI, Rank)
  
  focus_pitcher_1 <- focus_pitcher_wider %>%
    slice(1:6) %>%
    rename(KPI_1 = KPI,
           Rank_1 = Rank)
  focus_pitcher_2 <- focus_pitcher_wider %>%
    slice(7:12) %>%
    rename(KPI_2 = KPI,
           Rank_2 = Rank)
  
  focus_pitcher_combo <- cbind(focus_pitcher_1, focus_pitcher_2)
  
  focus_pitcher_tbl <- focus_pitcher_combo %>%
    gt() %>%
    tab_header(title = paste0(focus_pitcher$PLAYER_NAME_DISPLAY_FL, "'s ", focus_pitcher$YEAR, " Performance Rankings"),
               subtitle = paste0(focus_pitcher$ORGANIZATIONS_ABBREV," / ", focus_pitcher$THROWS, "HP / ",
                                 focus_pitcher$COMPETITION_LEVEL_ABBREV, " / ", focus_pitcher$AGE, " Y.O. / ",
                                 focus_pitcher$PITCH_TYPES, " / ",
                                 focus_pitcher$REL_HEIGHT_TRUE, " Ht / ", focus_pitcher$REL_SIDE_TRUE, " Sd / ",
                                 focus_pitcher$EXTENSION_TRUE, " Ext")) %>%
    cols_label(KPI_1 = md("KPI"),
               Rank_1 = md("Rank"),
               KPI_2 = md("Pitch Grades"),
               Rank_2 = md("Rank")) %>%
    text_case_match("CHASE_PCT" ~ "Chase %",
                    "K_RATE" ~ "K %",
                    "BB_RATE" ~ "BB %",
                    "FPS_RATE" ~ "FPS %",
                    "GB_PCT" ~ "GB %",
                    "XWOBA" ~ "xwOBA",
                    "FB_STUFF" ~ "FB Stuff",
                    "BB_STUFF" ~ "BB Stuff",
                    "OS_STUFF" ~ "OS Stuff",
                    "FB_LVA" ~ "FB LVA",
                    "BB_LVA" ~ "BB LVA",
                    "OS_LVA" ~ "OS LVA") %>%
    data_color(
      columns = c(Rank_1, Rank_2),
      method = "factor",
      palette = c("#D22D49","white","#1DBE3A"),
      domain = focus_pitcher$CHASE_PCT
    ) %>%
    cols_width(KPI_1 ~ px(120),
               Rank_1 ~ px(140),
               KPI_2 ~ px(120),
               Rank_2 ~ px(140)) %>%
    cols_align(align = "center",
               columns = everything()) %>%
    gt_theme_rangers() 
  
  return(focus_pitcher_tbl)
}

pie_chart <- function(ten_close) {
  
  pcts <- ten_close %>%
    group_by(ROLE) %>%
    summarise(pcts = n() / nrow(ten_close)) %>%
    mutate(labels = scales::percent(pcts),
           ROLE = factor(ROLE, levels = c("Starter", "Reliever", "Swingman"))) %>%
    arrange(ROLE)
  
  pcts_pie <- ggplot(pcts, aes(x = "", y = pcts, fill = ROLE)) +
    ggtitle("Role Distribution", subtitle = "Closest MLB Pitchers") +
    geom_col(color = "black") +
    geom_label(aes(label = labels),
               position = position_stack(vjust = 0.5),
               show.legend = FALSE) +
    coord_polar(theta = "y", direction = -1) +
    scale_fill_manual(values = c('Starter' = '#D22D49', 'Reliever' = '#00D1ED', 'Swingman' = '#1DBE3A')) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"))
  

  
  return(pcts_pie)
}

prob_table <- function(similar_pitchers, top_25_big_league_pct, similar_big_league_pct) {
  
  mlb_prob_diff <- data.frame(closest = round(top_25_big_league_pct$made_mlb_pct * 100, 1),
                              all_similar = round(similar_big_league_pct$made_mlb_pct * 100, 1)) %>%
    mutate(diff = closest - all_similar)
  
  mlb_prob_table <- mlb_prob_diff %>%
    gt() %>%
    tab_header(title = md("**Likelihood of Making MLB %**"),
               subtitle = md(paste0("Comparing 25 closest pitchers to all ", ifelse(similar_pitchers[1,]$THROWS == 'R', 'Right', 'Left'),
                                    "-Handed Pitchers at ", similar_pitchers[1,]$COMPETITION_LEVEL_ABBREV, " between ", similar_pitchers[1,]$AGE - 1,
                                    "-", similar_pitchers[1,]$AGE + 1, " Y.O."))) %>%
    cols_label(closest = "25 Closest %",
               all_similar = "All Similar %",
               diff = "Difference") %>%
    tab_style(style = cell_text(color = "#D22D49", style = "italic"),
              locations = cells_body(rows = closest < all_similar, columns = diff)) %>%
    tab_style(style = cell_text(color = "#1DBE3A", style = "italic"),
              locations = cells_body(rows = closest > all_similar, columns = diff)) %>%
    tab_style(style = cell_text(style = "italic"),
              locations = cells_body(rows = closest == all_similar, columns = diff)) %>%
    gt_theme_rangers() %>%
    cols_align("center")
  
  return(mlb_prob_table)
}



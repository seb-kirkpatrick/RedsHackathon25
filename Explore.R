library(tidyverse)
library(randomForest)

samp <- read_csv("sample_submission.csv")

pbp <- read_csv("savant_data_2021_2023.csv")
people <- read_csv("lahman_people.csv")

# Figure out who is in our test set

all_pitchers <- unique(pbp$pitcher)
all_batters <- unique(pbp$batter)
players <- unique(c(all_pitchers, all_batters))
test <- samp$PLAYER_ID
setdiff(test, players) # All players in the test set are in 2021-2023, aka no rookies

player_2023 <- pbp |> 
  filter(game_year == 2023) |> 
  select(batter, pitcher) |> 
  distinct() |> 
  unlist() |> 
  unique()

player_2022 <- pbp |> 
  filter(game_year == 2022) |> 
  select(batter, pitcher) |> 
  distinct() |> 
  unlist() |> 
  unique()

player_2021 <- pbp |> 
  filter(game_year == 2021) |> 
  select(batter, pitcher) |> 
  distinct() |> 
  unlist() |> 
  unique()

setdiff(test, player_2023) # 74 players in 2024 not in 2023, but in 2021 or 2022
setdiff(test, player_2022) # 255
setdiff(test, player_2021) # 408

# So the test set is all players in 2024 who played in at least one season between 2021 and 2023

# Data

b <- pbp |>
  select(batter, game_year, game_pk, inning, at_bat_number) |>
  group_by(batter, game_year, game_pk) |>
  summarize(pa = n_distinct(at_bat_number),
            start = any(inning <= 3)) |>
  ungroup() |>
  group_by(batter, game_year) |>
  summarize(b_games = n_distinct(game_pk),
            b_starts = sum(start),
            b_pa = sum(pa)) |>
  ungroup()

p <- pbp |>
  select(pitcher, game_year, game_pk, at_bat_number, role_key, pitch_number_appearance) |>
  group_by(pitcher, game_year, game_pk, role_key) |>
  summarize(pa = n_distinct(at_bat_number),
            pitches = max(pitch_number_appearance)) |>
  ungroup() |>
  group_by(pitcher, game_year) |>
  summarize(p_games = n_distinct(game_pk),
            p_starts = sum(role_key == "SP"),
            p_bf = sum(pa),
            pitches = sum(pitches)) |>
  ungroup()

both <- inner_join(p, b, by = c("pitcher" = "batter", "game_year")) |> 
  mutate(ohtani = p_bf * b_pa,
         p_diff = p_bf - b_pa) |>
  arrange(-ohtani)

#view(both)

Ohtani <- "18396fcf5f98aac97ec6127f7924868d3ef7bd9e" # Though he did NOT pitch in 2024
batters <- both |> filter(pitcher == Ohtani | b_pa > p_bf) |> select(pitcher, game_year)
pitchers <- both |> filter(pitcher == Ohtani | p_bf > b_pa) |> select(pitcher, game_year) 

# For everyone else, going to only include them for batter if pa > bf, and vice versa

# One issue to note is that intentional walks do not show up on the pitch by pitch data, so may want to add that into the total plate appearances


# Batter positions

dat <- pbp |>
  filter(pitch_number == 1) |>  
  select(game_year, game_pk, fielder_2_1:fielder_9) |>
  distinct() |>
  select(!game_pk)

pos <- dat |> pivot_longer(cols = -game_year, names_to = "position", values_to = "player_id") |>
  group_by(player_id, game_year) |>
  count(position) |>
  slice_max(order_by = n) |>
  select(-n)


# Pitcher Positions

b_updated <- b |> 
  filter(!(batter %in% pitchers$pitcher)) |>
  left_join(pos, by = c("batter" = "player_id", "game_year")) |>
  mutate(
    position = ifelse(is.na(position), "DH", position)
  )

p_updated <- p |>
  filter(!(pitcher %in% batters$pitcher))

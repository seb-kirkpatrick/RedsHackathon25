library(tidyverse)
library(randomForest)
library(zoo)

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
no_2023 <- setdiff(test, player_2023)

setdiff(test, player_2022) # 255
setdiff(no_2023, player_2022) # 17
no_2223 <- setdiff(no_2023, player_2022)

setdiff(test, player_2021) # 408
setdiff(no_2223, player_2021)

# So the test set is all players in 2024 who played in at least one season between 2021 and 2023

# Data

b <- pbp |>
  select(batter, game_year, game_pk, inning, events, at_bat_number) |>
  group_by(batter, game_year, game_pk) |>
  summarize(pa = n_distinct(at_bat_number),
            start = any(inning <= 3),
            HR = sum(events == "home_run", na.rm=T),
            H = sum(events %in% c("double", "home_run", "single", "triple"), na.rm=T),
            BB = sum(events == "walk", na.rm=T),
            SO = sum(events == "strikeout", na.rm=T)) |>
  ungroup() |>
  group_by(batter, game_year) |>
  summarize(b_games = n_distinct(game_pk),
            b_starts = sum(start),
            b_pa = sum(pa),
            pa_per_g = b_pa / b_games,
            b_HR = sum(HR),
            b_H = sum(H),
            b_BB = sum(BB),
            b_SO = sum(SO))|>
  ungroup()

IBB <- pbp |>
  select(game_pk, game_year, game_date, batter, at_bat_number, on_1b) |>
  arrange(game_pk, game_date, at_bat_number) |>
  filter(at_bat_number == lag(at_bat_number) + 2) |>
  group_by(game_year, on_1b) |>
  summarize(IBB = n()) |>
  ungroup()

b_ibb <- b |>
  left_join(IBB, by = c("batter" = "on_1b", "game_year")) |>
  mutate(IBB = ifelse(is.na(IBB), 0, IBB),
         b_pa = b_pa + IBB) # Updating for IBB

p <- pbp |>
  select(pitcher, game_year, game_pk, at_bat_number, role_key, pitch_number_appearance) |>
  group_by(pitcher, game_year, game_pk, role_key) |>
  summarize(pa = n_distinct(at_bat_number),
            pitches = max(pitch_number_appearance)) |>
  ungroup() |>
  group_by(pitcher, game_year) |>
  summarize(p_games = n_distinct(game_pk),
            p_starts = sum(role_key == "SP"),
            p_relief = p_games - p_starts,
            p_bf = sum(pa),
            pitches = sum(pitches),
            p_per_app = pitches / p_games) |>
  ungroup()

both <- inner_join(p, b, by = c("pitcher" = "batter", "game_year")) |> 
  mutate(ohtani = p_bf * b_pa,
         p_diff = p_bf - b_pa) |>
  arrange(-ohtani)

#view(both)

Ohtani <- "18396fcf5f98aac97ec6127f7924868d3ef7bd9e" # Though he did NOT pitch in 2024
batters <- both |> filter(pitcher == Ohtani | b_pa > p_bf) |> select(pitcher, game_year) #Just to use as a name list
pitchers <- both |> filter(pitcher == Ohtani | p_bf > b_pa) |> select(pitcher, game_year) #Same

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
  slice_max(order_by = n, with_ties = F) |>
  select(-n)

# Batter Ages and Szn

bio <- people |>
  select(player_mlb_id, birthDate, debut, weight, height, bats, throws)

# Batter Data

b_updated <- b_ibb |> 
  filter(!(batter %in% pitchers$pitcher) |
           batter == Ohtani) |>
  left_join(pos, by = c("batter" = "player_id", "game_year")) |>
  mutate(
    position = as.factor(ifelse(is.na(position), "DH", position))
  ) |>
  left_join(bio, by = c("batter" = "player_mlb_id")) |>
  mutate(
    Age = game_year - year(birthDate),
    Szn = game_year - year(debut) + 1
    )

# Prep input for RF

batter_train <- b_updated |>
  arrange(batter, game_year) |>
  group_by(batter) |>
  mutate(
    nxt_szn_pt = lead(b_pa)
  ) |> ungroup() |>
  mutate(nxt_szn_pt = ifelse(is.na(nxt_szn_pt), 0, nxt_szn_pt)) |>
  filter(game_year < 2023,
         !is.na(birthDate)) # This is only going to be the batters who hit in 2023, so there may be a few missing

bat_model <- randomForest(
  nxt_szn_pt ~ b_pa + b_games + b_starts + pa_per_g + Age + Szn + b_HR + b_BB + b_SO + position + weight + height,
  data = batter_train,
  ntree = 1000,
  mtry = sqrt(12)
)

batter_test <- b_updated |>
  filter(game_year == 2023,
         batter %in% samp$PLAYER_ID)

predictions <- batter_test |> 
  mutate(pt = round(predict(bat_model, batter_test))) |>
  select(id = batter, pt)




## Pitcher Now


p_updated <- p |>
  filter(!(pitcher %in% batters$pitcher) |
           pitcher == Ohtani) |>
  left_join(bio, by = c("pitcher" = "player_mlb_id")) |>
  mutate(
    Age = game_year - year(birthDate),
    Szn = game_year - year(debut) + 1
  )

p_train <- p_updated |>
  arrange(pitcher, game_year) |>
  group_by(pitcher) |>
  mutate(
    nxt_szn_pt = lead(p_bf)
  ) |> ungroup() |>
  mutate(nxt_szn_pt = ifelse(is.na(nxt_szn_pt), 0, nxt_szn_pt)) |>
  filter(game_year < 2023,
         !is.na(birthDate))
  
p_model <- randomForest(
  nxt_szn_pt ~ p_bf + p_starts + p_games + p_per_app + Age + Szn + weight + height,
  data = p_train,
  ntree = 1000,
  mtry = sqrt(8)
)

p_test <- p_updated |>
  filter(game_year == 2023,
         pitcher %in% samp$PLAYER_ID,
         pitcher != Ohtani) # He did not pitch in 2024

p_predictions <- p_test |> 
  mutate(pt = round(predict(p_model, p_test))) |>
  select(id = pitcher, pt)


pred_1 <- rbind(predictions, p_predictions)

missing23 <- setdiff(samp$PLAYER_ID, pred_1$id) # Missing 74 players, so that should be everyone who did not play in 2023


# Players with missing seasons

b_missing <- b_updated |>
  filter(batter %in% missing23) |>
  group_by(batter) |>
  slice_max(order_by = game_year) |>
  ungroup() |>
  mutate(Age = Age + (2023 - game_year),
         Szn = Szn)

b_missing_pred <- b_missing |> 
  mutate(pt = round(predict(bat_model, b_missing))) |>
  select(id = batter, pt)

p_missing <- p_updated |>
  filter(pitcher %in% missing23) |>
  group_by(pitcher) |>
  slice_max(order_by = game_year) |>
  ungroup() |>
  mutate(Age = Age + (2023 - game_year),
         Szn = Szn)

p_missing_pred <- p_missing |> 
  mutate(pt = round(predict(p_model, p_missing))) |>
  select(id = pitcher, pt)

pred <- rbind(pred_1, b_missing_pred, p_missing_pred)

rf_initial <- left_join(samp, pred, by = c("PLAYER_ID" = "id")) |>
  mutate(PLAYING_TIME = pt) |>
  select(PLAYER_ID, PLAYING_TIME)

write.csv(rf_initial, "predcition_rf2.csv", row.names=F)


## Time Series ##

monthly <- pbp |> 
  mutate(month = month(game_date),
         month = case_match(
           month,
           3 ~ 4,
           10 ~ 9,
           .default = month
         )) |>
  select(batter, game_year, game_pk, inning, events, at_bat_number, month) |>
  group_by(batter, game_year, game_pk, month) |>
  summarize(pa = n_distinct(at_bat_number),
            start = any(inning <= 3),
            HR = sum(events == "home_run", na.rm=T),
            H = sum(events %in% c("double", "home_run", "single", "triple"), na.rm=T),
            BB = sum(events == "walk", na.rm=T),
            SO = sum(events == "strikeout", na.rm=T)) |>
  ungroup() |>
  group_by(batter, game_year, month) |>
  summarize(b_games = n_distinct(game_pk),
            b_starts = sum(start),
            b_pa = sum(pa),
            pa_per_g = b_pa / b_games,
            b_HR = sum(HR),
            b_H = sum(H),
            b_BB = sum(BB),
            b_SO = sum(SO))|>
  ungroup()

IBB <- pbp |>
  select(game_pk, game_year, game_date, batter, at_bat_number, on_1b) |>
  arrange(game_pk, game_date, at_bat_number) |>
  filter(at_bat_number == lag(at_bat_number) + 2 & on_1b != lag(batter)) |>
  mutate(month = month(game_date)) |>
  group_by(game_year, month, on_1b) |>
  summarize(IBB = n()) |>
  ungroup()

monthly_ibb <- monthly |>
  filter(!(batter %in% pitchers$pitcher) |
           batter == Ohtani) |>
  left_join(IBB, by = c("batter" = "on_1b", "month", "game_year")) |>
  mutate(IBB = ifelse(is.na(IBB), 0, IBB),
         b_pa = b_pa + IBB)

all_months <- expand.grid(
  batter = unique(monthly_ibb$batter),
  game_year = unique(monthly_ibb$game_year),
  month = 4:9
)

monthly_filled <- all_months |> 
  left_join(monthly_ibb, by = c("batter", "game_year", "month")) |> 
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) |> 
  arrange(batter, game_year, month) |>
  group_by(batter) |>
  mutate(
    lag_pa = lag(b_pa, 1, default = 0),
    lag2_pa = lag(b_pa, 2, default = 0),
    lag3_pa = lag(b_pa, 3, default = 0),
    lag_HR = lag(b_HR, 1, default = 0),
    lag_BB = lag(b_BB, 1, default = 0),
    lag_SO = lag(b_SO, 1, default = 0),
    roll_3_pa = rollapplyr(lag(b_pa,1, default = 0), width = 3, FUN = sum, fill = NA, align = "right"),
    roll_3_HR = rollapplyr(lag(b_HR,1, default = 0), width = 3, FUN = sum, fill = NA, align = "right"),
    roll_3_BB = rollapplyr(lag(b_BB,1, default = 0), width = 3, FUN = sum, fill = NA, align = "right"),
    roll_3_SO = rollapplyr(lag(b_SO,1, default = 0), width = 3, FUN = sum, fill = NA, align = "right")
  ) |>
  mutate(
    roll_3_pa = ifelse(is.na(roll_3_pa), cumsum(lag_pa), roll_3_pa),
    roll_3_HR = ifelse(is.na(roll_3_HR), cumsum(lag_HR), roll_3_HR),
    roll_3_BB = ifelse(is.na(roll_3_BB), cumsum(lag_BB), roll_3_BB),
    roll_3_SO = ifelse(is.na(roll_3_SO), cumsum(lag_SO), roll_3_SO)
  ) |>
  ungroup()

career_avg <- b_updated |>
  group_by(batter) |>
  summarize(cr_game = mean(b_games, na.rm=T),
            cr_pa = mean(b_pa, na.rm=T),
            cr_starts = mean(b_starts, na.rm=T),
            cr_paperg = mean(pa_per_g, na.rm=T),
            cr_HR = mean(b_HR, na.rm=T),
            cr_H = mean(b_H, na.rm=T),
            cr_BB = mean(b_BB, na.rm=T),
            cr_SO = mean(b_SO, na.rm=T)) |>
  ungroup()

ts_batter <- monthly_filled |>
  left_join(pos, by = c("batter" = "player_id", "game_year")) |>
  mutate(
    position = as.factor(ifelse(is.na(position), "DH", position))
  ) |>
  left_join(career_avg, by = c("batter")) |>
  left_join(bio, by = c("batter" = "player_mlb_id")) |>
  mutate(
    Age = game_year - year(birthDate),
    Szn = game_year - year(debut) + 1
  ) |>
  group_by(batter) |>
  mutate(
    next_b_pa = lead(b_pa, 1)) |>
  ungroup() |>
  filter(Szn > 0)


play_szn <- ts_batter |>
  group_by(batter, game_year) |>
  summarize(
    games = sum(b_games),
    play = games > 0
  ) |>
  select(batter, game_year, play)

train_data <- ts_batter |>
  left_join(play_szn, by = c("batter", "game_year")) |>
  select(batter, game_year, month, b_pa, lag_pa, lag2_pa, lag3_pa, roll_3_pa, position, cr_game:cr_SO, Age, Szn, next_b_pa, play, bats)

  
train_no_na <- train_data |>
  filter(play,
         !is.na(cr_game),
         !is.na(next_b_pa),
         !(game_year == 2021 & month < 7))


library(ranger)

rf1 <- ranger(
  next_b_pa ~ b_pa + lag_pa + roll_3_pa + position + cr_game + cr_pa + cr_HR + cr_starts + cr_paperg + cr_H + cr_BB + cr_SO + Age + Szn + bats,
  num.trees = 1000,
  importance = "impurity",
  data = train_no_na,
  seed = 740
)


test_data <- train_data |>
  filter(game_year == 2023,
         batter %in% samp$PLAYER_ID,
         month == 9) |> 
  select(!next_b_pa)

rf1_preds <- predict(rf1, data=test_data)
rf1_preds$predictions

apr <- test_data |>
  mutate(game_year = 2024,
         month = 4,
         roll_3_pa = lag2_pa + lag_pa + b_pa,
         lag3_pa = lag2_pa,
         lag2_pa = lag_pa,
         lag_pa = b_pa,
         b_pa = rf1_preds$predictions,
         Age = Age + 1,
         Szn = Szn + 1,
         pa = b_pa
         )

rf1_pred_apr <- predict(rf1, apr)

may <- apr |>
  mutate(
    month = 5, 
    roll_3_pa = lag2_pa + lag_pa + b_pa,
    lag3_pa = lag2_pa,
    lag2_pa = lag_pa,
    lag_pa = b_pa,
    b_pa = rf1_pred_apr$predictions,
    pa = pa + b_pa
  )

rf1_pred_may <- predict(rf1, may)

jun <- may |>
  mutate(
    month = 6, 
    roll_3_pa = lag2_pa + lag_pa + b_pa,
    lag3_pa = lag2_pa,
    lag2_pa = lag_pa,
    lag_pa = b_pa,
    b_pa = rf1_pred_may$predictions,
    pa = pa + b_pa
  )

rf1_pred_jun <- predict(rf1, jun)

jul <- jun |>
  mutate(
    month = 7, 
    roll_3_pa = lag2_pa + lag_pa + b_pa,
    lag3_pa = lag2_pa,
    lag2_pa = lag_pa,
    lag_pa = b_pa,
    b_pa = rf1_pred_jun$predictions,
    pa = pa + b_pa
  )

rf1_pred_jul <- predict(rf1, jul)

aug <- jul |>
  mutate(
    month = 8, 
    roll_3_pa = lag2_pa + lag_pa + b_pa,
    lag3_pa = lag2_pa,
    lag2_pa = lag_pa,
    lag_pa = b_pa,
    b_pa = rf1_pred_jul$predictions,
    pa = pa + b_pa
  )

rf1_pred_aug <- predict(rf1, aug)

sep <- aug |>
  mutate(
    month = 9, 
    roll_3_pa = lag2_pa + lag_pa + b_pa,
    lag3_pa = lag2_pa,
    lag2_pa = lag_pa,
    lag_pa = b_pa,
    b_pa = rf1_pred_aug$predictions,
    pa = pa + b_pa
  )

ts_b_pred <- sep |>
  mutate(
    PLAYER_ID = batter,
    pt = round(pa)
  ) |>
  select(PLAYER_ID, pt)

# Now Pitchers

monthly_p <- pbp |> 
  mutate(month = month(game_date),
         month = case_match(
           month,
           3 ~ 4,
           10 ~ 9,
           .default = month
         )) |>
  select(pitcher, game_year, month, game_pk, at_bat_number, role_key, pitch_number_appearance) |>
  group_by(pitcher, game_year, month, game_pk, role_key) |>
  summarize(pa = n_distinct(at_bat_number),
            pitches = max(pitch_number_appearance)) |>
  ungroup() |>
  group_by(pitcher, month, game_year) |>
  summarize(p_games = n_distinct(game_pk),
            p_starts = sum(role_key == "SP"),
            p_relief = p_games - p_starts,
            p_bf = sum(pa),
            pitches = sum(pitches),
            p_per_app = pitches / p_games) |>
  ungroup() |>
  filter(!(pitcher %in% batters$pitcher) |
           pitcher == Ohtani) |>
  arrange(pitcher, game_year, month)


all_months_p <- expand.grid(
  pitcher = unique(monthly_p$pitcher),
  game_year = unique(monthly_p$game_year),
  month = 4:9
)

p_month_data <- all_months_p |>
  left_join(monthly_p, by = c("pitcher", "game_year", "month")) |>
  select(pitcher, game_year, month, p_bf, p_games) |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) |> 
  arrange(pitcher, game_year, month) |>
  group_by(pitcher) |>
  mutate(
    lag_bf = lag(p_bf, 1, default = 0),
    lag2_bf = lag(p_bf, 2, default = 0),
    lag3_bf = lag(p_bf, 3, default = 0),
    roll_3_bf = rollapplyr(lag(p_bf,1, default = 0), width = 3, FUN = sum, fill = NA, align = "right"),
  ) |>
  mutate(
    roll_3_bf = ifelse(is.na(roll_3_bf), cumsum(lag_bf), roll_3_bf),
  ) |>
  ungroup()

career_avg_p <- p_updated |>
  group_by(pitcher) |>
  summarize(
    cr_bf = mean(p_bf, na.rm=T),
    cr_game = mean(p_games, na.rm=T),
    cr_starts = mean(p_starts, na.rm=T),
    cr_pitches = mean(pitches, na.rm=T),
    cr_pperapp = mean(p_per_app, na.rm=T)
  )

year_p <- p_updated |>
  group_by(pitcher, game_year) |>
  summarize(
    Position = case_when(
      p_starts == p_games ~ "SP",
      p_starts == 0 ~ "RP",
      TRUE ~ "Both"
    )
  )

pmonth <- p_month_data |>
  left_join(career_avg_p, by = "pitcher") |>
  left_join(year_p, by = c("pitcher", "game_year")) |>
  left_join(bio, by = c("pitcher" = "player_mlb_id")) |>
  mutate(
    Age = game_year - year(birthDate),
    Szn = game_year - year(debut) + 1
  ) |>
  group_by(pitcher) |>
  mutate(
    next_p_bf = lead(p_bf, 1)) |>
  ungroup() |>
  filter(Szn > 0)


play_szn_p <- pmonth |>
  group_by(pitcher, game_year) |>
  summarize(
    games = sum(p_games),
    play = games > 0
  ) |>
  select(pitcher, game_year, play)

pm_train <- pmonth |>
  left_join(play_szn_p, by = c("pitcher", "game_year")) |>
  select(-p_games, -bats)


pm_no_na <- pm_train|>
  filter(play,
         !is.na(cr_game),
         !is.na(next_p_bf),
         !(game_year == 2021 & month < 7))
  
rf2 <- ranger(
  next_p_bf ~ p_bf + lag_bf + roll_3_bf + Position + cr_game + cr_bf + cr_starts + cr_pitches + cr_pperapp + Age + Szn + throws,
  num.trees = 1000,
  importance = "impurity",
  data = pm_no_na,
  seed = 740
)

p_test_data <- pm_train |>
  filter(game_year == 2023,
         pitcher %in% samp$PLAYER_ID,
         month == 9,
         pitcher != Ohtani) |> 
  select(!next_p_bf)

rf2_preds <- predict(rf2, data=p_test_data)
rf2_preds$predictions

p_apr <- p_test_data |>
  mutate(game_year = 2024,
         month = 4,
         roll_3_bf = lag2_bf + lag_bf + p_bf,
         lag3_bf = lag2_bf,
         lag2_bf = lag_bf,
         lag_bf = p_bf,
         p_bf = rf2_preds$predictions,
         Age = Age + 1,
         Szn = Szn + 1,
         bf = p_bf
         )

rf2_pred_apr <- predict(rf2, p_apr)

p_may <- p_apr |>
  mutate(
    month = 5, 
    roll_3_bf = lag2_bf + lag_bf + p_bf,
    lag3_bf = lag2_bf,
    lag2_bf = lag_bf,
    lag_bf = p_bf,
    p_bf = rf2_pred_apr$predictions,
    bf = bf + p_bf
  )

rf2_pred_may <- predict(rf2, p_may)

p_jun <- p_may |>
  mutate(
    month = 6, 
    roll_3_bf = lag2_bf + lag_bf + p_bf,
    lag3_bf = lag2_bf,
    lag2_bf = lag_bf,
    lag_bf = p_bf,
    p_bf = rf2_pred_may$predictions,
    bf = bf + p_bf
  )

rf2_pred_jun <- predict(rf2, p_jun)

p_jul <- p_jun |>
  mutate(
    month = 7, 
    roll_3_bf = lag2_bf + lag_bf + p_bf,
    lag3_bf = lag2_bf,
    lag2_bf = lag_bf,
    lag_bf = p_bf,
    p_bf = rf2_pred_jun$predictions,
    bf = bf + p_bf
  )

rf2_pred_jul <- predict(rf2, p_jul)

p_aug <- p_jul |>
  mutate(
    month = 8, 
    roll_3_bf = lag2_bf + lag_bf + p_bf,
    lag3_bf = lag2_bf,
    lag2_bf = lag_bf,
    lag_bf = p_bf,
    p_bf = rf2_pred_jul$predictions,
    bf = bf + p_bf
  )

rf2_pred_aug <- predict(rf2, p_aug)

p_sep <- p_aug |>
  mutate(
    month = 9, 
    roll_3_bf = lag2_bf + lag_bf + p_bf,
    lag3_bf = lag2_bf,
    lag2_bf = lag_bf,
    lag_bf = p_bf,
    p_bf = rf2_pred_aug$predictions,
    bf = bf + p_bf
  )

ts_p_pred <- p_sep |>
  mutate(
    PLAYER_ID = pitcher,
    pt = round(bf)
  ) |>
  select(PLAYER_ID, pt)


ts_predictions <-
  rbind(ts_b_pred, ts_p_pred) |>
  left_join(samp, by = "PLAYER_ID") |>
  mutate(PLAYING_TIME = round(pt)) |>
  select(PLAYER_ID, PLAYING_TIME)

write.csv(ts_predictions, "final_pred_time_series.csv", row.names=F)

batter_gap <- b_ibb |>
  group_by(batter) |>
  summarize(
    played_2021 = any(game_year == 2021),
    played_2022 = any(game_year == 2022),
    played_2023 = any(game_year == 2023)
  ) |>
  filter(played_2021 & !played_2022 & played_2023) |>
  pull(batter)

pitcher_gap <- p |>
  group_by(pitcher) |>
  summarize(
    played_2021 = any(game_year == 2021),
    played_2022 = any(game_year == 2022),
    played_2023 = any(game_year == 2023)
  ) |>
  filter(played_2021 & !played_2022 & played_2023) |>
  pull(pitcher)
  
bg_plot <- b_ibb |>
  filter(batter %in% batter_gap) |>
  ggplot(
    aes(x = as.character(game_year), y = b_pa, group = batter)) +
  geom_line(alpha=0.8) + 
  geom_smooth(aes(group = 1), se = FALSE, color = "red", size = 1) + # Overall trend line
  theme_minimal() +
  scale_x_discrete(expand = c(.05, .05)) +
  labs(y = "Plate Appearances", x = "Season", title = "Batters with Season Gap") +
  theme(
    plot.title = element_text(size = 30, hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
    legend.position = "none"
  )
ggsave("BatterGaps.png", bg_plot, width = 8, height = 6, dpi = 300, bg = "white")

pg_plot <- p |>
  filter(pitcher %in% pitcher_gap) |>
  ggplot(
    aes(x = as.character(game_year), y = p_bf, group = pitcher)) +
  geom_line(alpha=0.8) + 
  geom_smooth(aes(group = 1), se = FALSE, color = "red", size = 1) + # Overall trend line
  theme_minimal() +
  scale_x_discrete(expand = c(.05, .05)) +
  labs(y = "Plate Appearances", x = "Season", title = "Pitchers with Season Gap") +
  theme(
    plot.title = element_text(size = 30, hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(margin = ggplot2::margin(r = 15)),
    legend.position = "none"
  )
ggsave("PitcherGaps.png", pg_plot, width = 8, height = 6, dpi = 300, bg = "white")

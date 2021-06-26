# qb_data, rb_data, wr_data, te_data

#colnames(rb_data)

temp <- rb_data %>%
  group_by(posteam, week, drive, qtr) %>%
  summarise(
    catches = sum(complete_pass, na.rm = TRUE),
    rushes = sum(rush_attempt, na.rm = TRUE),
    touches = sum(catches + rushes),
  )

ggplot(data = rb_data, mapping = aes(x = game_seconds_remaining, y = rush_attempt)) +
  geom_boxplot(mapping = aes(group = cut_width(game_seconds_remaining, 500)))

ggplot(data = temp, mapping = aes(x = qtr, y = touches)) +
  geom_point(aes(size = touches), alpha = 1/3)

ggplot(data = temp, mapping = aes(x = week, y = qtr)) +
  geom_tile(mapping = aes(fill = touches), color = "grey50") +
  scale_fill_gradient(low = "white", high = "#000F70")

rb_data %>% 
  count(rusher, wt = rushing_yards, sort = TRUE)

rb_data %>%
  count(rusher, run_location, wt = rushing_yards)
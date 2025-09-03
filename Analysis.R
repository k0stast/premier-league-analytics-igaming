
df <- read.csv(file.choose())

head(df)
str(df)
summary(df)

#packages
install.packages(c("dplyr", "readr", "tidyverse", "lubridate"))
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(lubridate)

#data
df <- readr::read_delim("C:/Users/Kostas/OneDrive/Υπολογιστής/archive/epl_matches - Αντιγραφή.csv", delim = ";", show_col_types = FALSE) %>%
  mutate(
    date = dmy(Date),
    Season = if_else(month(date) >= 8, paste0(year(date), "/", year(date)+1),
                     paste0(year(date)-1, "/", year(date))),
    goal_diff = as.numeric(FTHG) - as.numeric(FTAG)
  )

head(df)

                           #Descriptive                   

#Question 1
# Overall
overall <- df %>%
  summarise(
    n_matches = n(),
    mean_home_goal_diff = mean(goal_diff, na.rm = TRUE),
    pct_HomeWin = mean(FTR == "H", na.rm = TRUE),
    pct_Draw    = mean(FTR == "D", na.rm = TRUE),
    pct_AwayWin = mean(FTR == "A", na.rm = TRUE)
  )

print(overall)

by_home_team <- df %>%
  group_by(HomeTeam) %>%
  summarise(
    n_matches = n(),
    mean_home_goal_diff = mean(goal_diff, na.rm = TRUE),
    pct_HomeWin = mean(FTR == "H", na.rm = TRUE),
    pct_Draw    = mean(FTR == "D", na.rm = TRUE),
    pct_AwayWin = mean(FTR == "A", na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(desc(mean_home_goal_diff))
print(by_home_team)

#Question 2
df <- df %>%
  mutate(total_goals = as.numeric(FTHG) + as.numeric(FTAG))

goal_stats <- df %>%
  summarise(
    mean_goals = mean(total_goals, na.rm = TRUE),
    var_goals  = var(total_goals, na.rm = TRUE),
    p_over25   = mean(total_goals > 2.5, na.rm = TRUE),
    p_zero     = mean(total_goals == 0, na.rm = TRUE)
  )

print(goal_stats)

# Προαιρετικά: Histogram
ggplot(df, aes(x = total_goals)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Κατανομή Συνολικών Γκολ (Premier League 2021/22)",
       x = "Συνολικά Γκολ στον αγώνα", y = "Συχνότητα")

#Question 3
# Υπολογισμός συνολικών γκολ
df <- df %>%
  mutate(total_goals = as.numeric(FTHG) + as.numeric(FTAG))

# 1) Συσχετίσεις (Pearson & Spearman) με τα βασικά match stats
cor_stats <- df %>%
  summarise(
    cor_HS   = cor(total_goals, HS,  use = "complete.obs", method = "pearson"),
    cor_HST  = cor(total_goals, HST, use = "complete.obs", method = "pearson"),
    cor_HC   = cor(total_goals, HC,  use = "complete.obs", method = "pearson"),
    cor_HF   = cor(total_goals, HF,  use = "complete.obs", method = "pearson"),
    spear_HS  = cor(total_goals, HS,  use = "complete.obs", method = "spearman"),
    spear_HST = cor(total_goals, HST, use = "complete.obs", method = "spearman"),
    spear_HC  = cor(total_goals, HC,  use = "complete.obs", method = "spearman"),
    spear_HF  = cor(total_goals, HF,  use = "complete.obs", method = "spearman")
  )

print(cor_stats)

# 2) Boxplots: πώς αλλάζουν τα συνολικά γκολ ανάλογα με το αποτέλεσμα (FTR)
library(ggplot2)

ggplot(df, aes(x = FTR, y = total_goals, fill = FTR)) +
  geom_boxplot() +
  labs(title = "Συνολικά Γκολ ανά Τελικό Αποτέλεσμα (Premier League 2021/22)",
       x = "Τελικό Αποτέλεσμα (H/D/A)", y = "Συνολικά Γκολ") +
  theme_minimal()

#Question 4
# 1) Μέση συμπεριφορά διαιτητών
by_ref <- df %>%
  group_by(Referee) %>%
  summarise(
    n_matches = n(),
    avg_HY = mean(HY, na.rm = TRUE),  # HY = Home Yellow Cards
    avg_AY = mean(AY, na.rm = TRUE),  # AY = Away Yellow Cards
    avg_HR = mean(HR, na.rm = TRUE),  # HR = Home Red Cards
    avg_AR = mean(AR, na.rm = TRUE),  # AR = Away Red Cards
    avg_total_cards = mean(HY + AY + HR + AR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_total_cards))

print(by_ref)

# 2) Μέση πειθαρχία ομάδων (ως γηπεδούχοι)
by_team <- df %>%
  group_by(HomeTeam) %>%
  summarise(
    n_matches = n(),
    avg_Y = mean(HY, na.rm = TRUE),   # Home Yellow Cards
    avg_R = mean(HR, na.rm = TRUE),   # Home Red Cards
    avg_total_cards = mean(HY + HR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_total_cards))

print(by_team)

# 3) Οπτικοποίηση: Boxplot για συνολικές κάρτες ανά referee

df <- df %>%
  mutate(total_cards = HY + AY + HR + AR)

ggplot(df, aes(x = Referee, y = total_cards)) +
  geom_boxplot(fill = "tomato", alpha = 0.6) +
  labs(title = "Συνολικές Κάρτες ανά Διαιτητή (Premier League 2021/22)",
       x = "Διαιτητής", y = "Συνολικές Κάρτες στον αγώνα") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


                          #Diagnostic

# Βασικά πεδία & βοηθητικές μεταβλητές
df <- df %>%
  mutate(
    FTHG = as.numeric(FTHG),
    FTAG = as.numeric(FTAG),
    HTHG = as.numeric(HTHG),
    HTAG = as.numeric(HTAG),
    HY   = as.numeric(HY),
    AY   = as.numeric(AY),
    HR   = as.numeric(HR),
    AR   = as.numeric(AR),
    total_goals = FTHG + FTAG,
    ht_goal_diff = HTHG - HTAG,          # διαφορά σκορ στο ημίχρονο (home - away)
    ft_goal_diff = FTHG - FTAG           # διαφορά στο τέλος
  )

if ("Attendance" %in% names(df)) {
  df <- df %>%
    mutate(Attendance = suppressWarnings(as.numeric(gsub(",", "", Attendance))))
}

#Question 1 
# Πίνακας HTR (ημίχρονο) x FTR (τελικό) με ποσοστά ανά γραμμή
ht_ft_tbl <- df %>%
  count(HTR, FTR) %>%
  group_by(HTR) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

print(ht_ft_tbl)

# Γυρίσματα (comebacks)
home_comeback_n   <- df %>% filter(HTR == "A", FTR == "H") %>% nrow()     # home έχανε στο HT, κέρδισε στο FT
home_comeback_den <- df %>% filter(HTR == "A") %>% nrow()
home_comeback_rate <- ifelse(home_comeback_den > 0, home_comeback_n / home_comeback_den, NA_real_)

away_comeback_n   <- df %>% filter(HTR == "H", FTR == "A") %>% nrow()     # away έχανε στο HT, κέρδισε στο FT
away_comeback_den <- df %>% filter(HTR == "H") %>% nrow()
away_comeback_rate <- ifelse(away_comeback_den > 0, away_comeback_n / away_comeback_den, NA_real_)

draw_to_homewin <- df %>% filter(HTR == "D", FTR == "H") %>% nrow()
draw_to_awaywin <- df %>% filter(HTR == "D", FTR == "A") %>% nrow()

cat("Home comebacks (A at HT -> H at FT):", home_comeback_n, 
    " / ", home_comeback_den, " = ", round(100*home_comeback_rate,1), "%\n", sep = "")
cat("Away comebacks (H at HT -> A at FT):", away_comeback_n, 
    " / ", away_comeback_den, " = ", round(100*away_comeback_rate,1), "%\n", sep = "")
cat("From Draw at HT -> Home win FT:", draw_to_homewin, "\n")
cat("From Draw at HT -> Away win FT:", draw_to_awaywin, "\n")

# Buckets με βάση τη διαφορά στο HT και κατανομή FT αποτελέσματος
df_ht_buckets <- df %>%
  mutate(
    ht_bucket = case_when(
      ht_goal_diff <= -2 ~ "Away +2+ at HT",
      ht_goal_diff == -1 ~ "Away +1 at HT",
      ht_goal_diff == 0  ~ "Draw at HT",
      ht_goal_diff == 1  ~ "Home +1 at HT",
      ht_goal_diff >= 2  ~ "Home +2+ at HT",
      TRUE ~ NA_character_
    )
  )

ht_bucket_outcomes <- df_ht_buckets %>%
  filter(!is.na(ht_bucket)) %>%
  group_by(ht_bucket) %>%
  summarise(
    n = n(),
    home_win = mean(FTR == "H", na.rm = TRUE),
    draw     = mean(FTR == "D", na.rm = TRUE),
    away_win = mean(FTR == "A", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(factor(ht_bucket, levels = c("Away +2+ at HT","Away +1 at HT","Draw at HT","Home +1 at HT","Home +2+ at HT")))

print(ht_bucket_outcomes)

# Heatmap HTR x FTR (ποσοστά)
ggplot(ht_ft_tbl, aes(x = HTR, y = FTR, fill = pct)) +
  geom_tile() +
  geom_text(aes(label = paste0(round(100*pct,1), "%")), color = "white", fontface = "bold") +
  scale_fill_gradient(low = "grey70", high = "steelblue") +
  labs(title = "HT Result vs FT Result — Premier League 2021/22",
       x = "HTR (Ημίχρονο: H/D/A)", y = "FTR (Τελικό: H/D/A)", fill = "Ποσοστό") +
  theme_minimal()

#Question 2
ref_goals <- df %>%
  group_by(Referee) %>%
  summarise(
    n_matches = n(),
    mean_goals = mean(total_goals, na.rm = TRUE),
    median_goals = median(total_goals, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_goals))

# φίλτρο για refs με αρκετά ματς (π.χ. ≥10) για να μην επηρεάζουν μικρά δείγματα
ref_goals_min10 <- ref_goals %>% filter(n_matches >= 10)

print(ref_goals_min10)

# Γράφημα bar chart
library(ggplot2)
ggplot(ref_goals_min10, aes(x = reorder(Referee, mean_goals), y = mean_goals)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Μέσος Όρος Γκολ ανά Διαιτητή (Premier League 2021/22)",
       x = "Διαιτητής", y = "Μ.Ο. συνολικών γκολ") +
  theme_minimal()

#Question 3
# Ενοποίηση home & away δεδομένων σε ένα frame "από την οπτική της ομάδας"
home_side <- df %>%
  transmute(
    Team = HomeTeam,
    own_y = HY, own_r = HR,
    outcome = case_when(FTR == "H" ~ "Win",
                        FTR == "D" ~ "Draw",
                        TRUE       ~ "Loss")
  )

away_side <- df %>%
  transmute(
    Team = AwayTeam,
    own_y = AY, own_r = AR,
    outcome = case_when(FTR == "A" ~ "Win",
                        FTR == "D" ~ "Draw",
                        TRUE       ~ "Loss")
  )

team_long <- bind_rows(home_side, away_side)

team_discipline_results <- team_long %>%
  mutate(own_cards = own_y + 2*own_r) %>%   # βάρος: yellow=1, red=2
  group_by(Team) %>%
  summarise(
    n_matches = n(),
    avg_cards = mean(own_cards, na.rm = TRUE),
    win_rate  = mean(outcome == "Win",  na.rm = TRUE),
    draw_rate = mean(outcome == "Draw", na.rm = TRUE),
    loss_rate = mean(outcome == "Loss", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_cards))

print(team_discipline_results)

# Συσχέτιση (Pearson) μέσες κάρτες vs loss rate
cor_cards_loss <- cor(team_discipline_results$avg_cards,
                      team_discipline_results$loss_rate,
                      use = "complete.obs",
                      method = "pearson")
cat("Correlation(avg_cards, loss_rate) =", round(cor_cards_loss, 3), "\n")

#Question 4
# Υπολογισμός διαφοράς καρτών ανά ματς
df_cd <- df %>%
  mutate(
    card_diff = (HY + 2*HR) - (AY + 2*AR),  # θετικό = περισσότερες κάρτες ο Home
    card_bucket = case_when(
      card_diff <= -2 ~ "Away +2+ cards",
      card_diff == -1 ~ "Away +1 card",
      card_diff == 0  ~ "Even cards",
      card_diff == 1  ~ "Home +1 card",
      card_diff >= 2  ~ "Home +2+ cards",
      TRUE ~ NA_character_
    )
  )

# Υπολογισμός αποτελεσμάτων ανά bucket
card_outcomes <- df_cd %>%
  filter(!is.na(card_bucket)) %>%
  group_by(card_bucket) %>%
  summarise(
    n = n(),
    home_win = mean(FTR == "H", na.rm = TRUE),
    draw     = mean(FTR == "D", na.rm = TRUE),
    away_win = mean(FTR == "A", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(factor(card_bucket,
                 levels = c("Away +2+ cards",
                            "Away +1 card",
                            "Even cards",
                            "Home +1 card",
                            "Home +2+ cards")))

print(card_outcomes)

# Προαιρετικό stacked bar chart
library(ggplot2)
card_plot <- df_cd %>%
  filter(!is.na(card_bucket)) %>%
  count(card_bucket, FTR) %>%
  group_by(card_bucket) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

ggplot(card_plot, aes(x = card_bucket, y = pct, fill = FTR)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Αποτέλεσμα ανά διαφορά καρτών (Home - Away)",
       x = "Διαφορά καρτών", y = "Ποσοστό αγώνων") +
  theme_minimal()


                                       #Odds

#1
library(tidyverse)

# ---- helper: ορισμός όλων των bookies που μπορεί να υπάρχουν στο dataset ----
triples <- list(
  B365 = c("B365H","B365D","B365A"),
  BS   = c("BSH","BSD","BSA"),
  BW   = c("BWH","BWD","BWA"),
  GB   = c("GBH","GBD","GBA"),
  IW   = c("IWH","IWD","IWA"),
  LB   = c("LBH","LBD","LBA"),
  PS   = c("PSH","PSD","PSA"),
  SO   = c("SOH","SOD","SOA"),
  SB   = c("SBH","SBD","SBA"),
  SJ   = c("SJH","SJD","SJA"),
  SY   = c("SYH","SYD","SYA"),
  VC   = c("VCH","VCD","VCA"),
  WH   = c("WHH","WHD","WHA"),
  # Προσθέτουμε και τα BetBrain aggregates σαν “pseudo-bookmakers”
  BbAv = c("BbAvH","BbAvD","BbAvA"),
  BbMx = c("BbMxH","BbMxD","BbMxA")
)

present <- names(triples)[sapply(triples, function(v) all(v %in% names(df)))]

# ---- υπολογισμός overround ανά αγώνα & ανά bookmaker ----
overround_long <- map_dfr(present, function(name) {
  cols <- triples[[name]]
  H <- suppressWarnings(as.numeric(df[[cols[1]]]))
  D <- suppressWarnings(as.numeric(df[[cols[2]]]))
  A <- suppressWarnings(as.numeric(df[[cols[3]]]))
  valid <- !is.na(H) & !is.na(D) & !is.na(A) & H > 0 & D > 0 & A > 0
  ov <- rep(NA_real_, length(H))
  ov[valid] <- (1/H[valid]) + (1/D[valid]) + (1/A[valid])  # overround (π.χ. 1.06 = 6% margin)
  tibble(bookmaker = name, overround = ov)
})

# ---- σύνοψη: μέσο/median overround, margin% κ.λπ. ----
summary_overround <- overround_long %>%
  group_by(bookmaker) %>%
  summarise(
    n = sum(!is.na(overround)),
    mean_overround      = mean(overround, na.rm = TRUE),
    median_overround    = median(overround, na.rm = TRUE),
    sd_overround        = sd(overround, na.rm = TRUE),
    mean_margin_pct   = (mean_overround - 1) * 100,   # margin %
    median_margin_pct = (median_overround - 1) * 100,
    .groups = "drop"
  ) %>%
  arrange(mean_overround)

print(summary_overround)

# ---- γραφήματα (προαιρετικά) ----
# 1) Bar chart: μέσο margin% ανά bookmaker
ggplot(summary_overround,
       aes(x = reorder(bookmaker, mean_margin_pct), y = mean_margin_pct)) +
  geom_col() +
  coord_flip() +
  labs(title = "Μέσο Margin (Overround-1) ανά Bookmaker — 1X2",
       x = "Bookmaker", y = "Μέσο Margin (%)") +
  theme_minimal()

# 2) Boxplot: κατανομή overround ανά bookmaker (για να δεις τη διασπορά)
ggplot(overround_long, aes(x = bookmaker, y = overround - 1)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Κατανομή Overround ανά Bookmaker — 1X2",
       x = "Bookmaker", y = "Overround - 1 (Margin)") +
  theme_minimal()


#2
library(tidyverse)

triples <- list(
  B365 = c("B365H","B365D","B365A"),
  BW   = c("BWH","BWD","BWA"),
  IW   = c("IWH","IWD","IWA"),
  PS   = c("PSH","PSD","PSA"),
  WH   = c("WHH","WHD","WHA"),
  VC   = c("VCH","VCD","VCA")
)

present <- names(triples)[sapply(triples, function(v) all(v %in% names(df)))]

# calibration
calibration_long <- map_dfr(present, function(name) {
  cols <- triples[[name]]
  H <- as.numeric(df[[cols[1]]])
  D <- as.numeric(df[[cols[2]]])
  A <- as.numeric(df[[cols[3]]])
  
  valid <- !is.na(H) & !is.na(D) & !is.na(A) & H > 0 & D > 0 & A > 0
  
  imp_H <- 1/H; imp_D <- 1/D; imp_A <- 1/A
  overround <- imp_H + imp_D + imp_A
  norm_H <- imp_H / overround
  norm_D <- imp_D / overround
  norm_A <- imp_A / overround
  
  tibble(
    bookmaker = name,
    FTR = df$FTR,
    p_H = ifelse(valid, norm_H, NA),
    p_D = ifelse(valid, norm_D, NA),
    p_A = ifelse(valid, norm_A, NA)
  )
})

#bookmaker
calibration_summary <- calibration_long %>%
  group_by(bookmaker) %>%
  summarise(
    mean_p_H = mean(p_H, na.rm = TRUE),
    mean_p_D = mean(p_D, na.rm = TRUE),
    mean_p_A = mean(p_A, na.rm = TRUE),
    actual_H = mean(FTR == "H", na.rm = TRUE),
    actual_D = mean(FTR == "D", na.rm = TRUE),
    actual_A = mean(FTR == "A", na.rm = TRUE),
    .groups = "drop"
  )

print(calibration_summary)

# implied vs actual
calibration_plot <- calibration_summary %>%
  pivot_longer(cols = starts_with("mean_p"), names_to = "type", values_to = "mean_p") %>%
  mutate(
    outcome = case_when(
      type == "mean_p_H" ~ "Home",
      type == "mean_p_D" ~ "Draw",
      type == "mean_p_A" ~ "Away"
    ),
    actual = case_when(
      type == "mean_p_H" ~ calibration_summary$actual_H[match(bookmaker, calibration_summary$bookmaker)],
      type == "mean_p_D" ~ calibration_summary$actual_D[match(bookmaker, calibration_summary$bookmaker)],
      type == "mean_p_A" ~ calibration_summary$actual_A[match(bookmaker, calibration_summary$bookmaker)]
    )
  )

ggplot(calibration_plot, aes(x = outcome)) +
  geom_col(aes(y = mean_p, fill = "Implied"), position = "dodge") +
  geom_point(aes(y = actual, color = "Actual"), size = 3) +
  facet_wrap(~bookmaker) +
  labs(title = "Calibration: Implied vs Actual Probabilities (1X2)",
       x = "Outcome", y = "Probability") +
  theme_minimal()


#3
df_ou <- df %>%
  mutate(
    over_odds  = as.numeric(`B365>2.5`),
    under_odds = as.numeric(`B365<2.5`),
    imp_over   = 1 / over_odds,
    imp_under  = 1 / under_odds,
    overround  = imp_over + imp_under,
    norm_over  = imp_over / overround,
    norm_under = imp_under / overround,
    actual_over = (FTHG + FTAG) > 2.5   
  )

ou_summary <- df_ou %>%
  summarise(
    mean_imp_over = mean(norm_over, na.rm = TRUE),
    mean_imp_under = mean(norm_under, na.rm = TRUE),
    actual_over = mean(actual_over, na.rm = TRUE),
    actual_under = 1 - actual_over
  )

print(ou_summary)


# implied vs actual
ou_plot <- tibble(
  Outcome = c("Over 2.5", "Under 2.5"),
  Implied = c(ou_summary$mean_imp_over, ou_summary$mean_imp_under),
  Actual  = c(ou_summary$actual_over, ou_summary$actual_under)
)

library(ggplot2)
ggplot(ou_plot, aes(x = Outcome)) +
  geom_col(aes(y = Implied, fill = "Implied"), position = "dodge") +
  geom_point(aes(y = Actual, color = "Actual"), size = 4) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Over/Under 2.5: Implied vs Actual (Bet365)",
       y = "Probability") +
  theme_minimal()






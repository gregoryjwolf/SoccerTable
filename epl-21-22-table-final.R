################################################################################
# Premier League 2021-2022
# Based on: tashapiro/tanya-data-viz (GitHub)
#
#
################################################################################

library(ggplot2)
#install.packages("gt")
library(gt)
#install.packages("gtExtras")
library(gtExtras)
#install.packages("ggflags")
library(ggflags)
library(paletteer)
library(systemfonts)
#install.packages("sparkline")
library(sparkline)
library(yarrr)
#if you dont have gtExtras-or ggflags uncomment below to install
#remotes::install_github("jthomasmock/gtExtras")
#devtools::install_github("rensa/ggflags")

# set wd
setwd("~/Library/CloudStorage/OneDrive-DrakeUniversity/Documents/Data Science/Projects/epl")

# read results csv
#df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')
df.all.match.results <-readr::read_csv('all_match_results.csv')
#df <- rename(df, match_date = Date)


#df$match_date<-as.Date(df$match_date,'%d-%b-%Y')

#separate 'Result' into two columns: 'score_team1' and 'score_team2'
df.clean <- df.all.match.results %>%
  mutate(match_id = row_number()) %>%
  rename(match_date = Date) %>%    # rename 'Date' = 'match_date'
  mutate(match_date = as.Date(match_date,'%d-%b-%Y')) %>%  #convert character to date ()
  mutate(home.team = HomeTeam) %>%   # create 'home.team' variable
  rename(team1 = HomeTeam) %>%
  rename(team2 = AwayTeam) %>%
  separate(Result, c('score_team1','score_team2'), sep=":") %>%   # split 'Result' to goals for 'team1' and 'team2
  mutate(score_team1 = as.numeric(score_team1)) %>%
  mutate(score_team2 = as.numeric(score_team2)) %>%
  mutate(winner = ifelse(score_team1 > score_team2, team1,        # create match winner variable
                         ifelse(score_team2 > score_team1, team2,
                                'draw')) ) %>%
  mutate(margin = abs(score_team1 - score_team2)) %>%
  mutate(series = "EPL 2021-2022")

# get info for team 1
t1 <- df.clean %>%
  select(team1, score_team1, team2, score_team2, winner, margin, series, match_date, match_id) %>%
  rename(team=team1, goals.for=score_team1, goals.against=score_team2, opponent=team2)

# get info for team 2
t2 <- df.clean %>%
  select(team2, score_team2, team1, score_team1, winner, margin, series, match_date, match_id) %>%
  rename(team=team2, goals.for=score_team2, goals.against=score_team1, opponent=team1)

# combine team 1 and team 2 data, create vertical format for data
df.new <- rbind(t1, t2)

# create win, draw, loss indicators, summary match result, points gained, match 
# goal difference, and cumulative points and goal difference
df.new <- df.new %>%
  mutate(win  = ifelse(goals.for >  goals.against, 1, 0)) %>%
  mutate(draw = ifelse(goals.for == goals.against, 1, 0)) %>%
  mutate(loss = ifelse(goals.for <  goals.against, 1, 0)) %>%
  mutate(match.result = ifelse(win == 1, 1,
                               ifelse(draw == 1, 0.5,
                                      0))) %>% # loss == 0
  mutate(points.gained = ifelse( win == 1, 3,
                         ifelse(draw == 1, 1,
                                0))) %>% # loss == 0
  mutate(goal.diff.match = goals.for - goals.against) %>%
  arrange(match_date) %>%
  group_by(team) %>%
  mutate(cum.pts = cumsum(points.gained)) %>%
  mutate(cum.gd = cumsum(goal.diff.match))

# Create summaries for table with new object df.epl
df.epl <- df.new %>%
  group_by(team) %>%
  summarise(points = sum(points.gained),
            games = n(),
            goals.for = sum(goals.for),
            goals.against = sum(goals.against),
            #avg.goals.for = round(sum(goals.for)/n(),0),
            wins = sum(win),
            losses = sum(loss),
            draws = sum(draw),
            avg.points = round(mean(points.gained),2),
            goal.diff = sum(goal.diff.match)) %>%
  # IMPORTANT: arrange data set based on points and then wins
  arrange(desc(points), wins) %>%
  #add club badges images - images taken from Premier League website
  mutate(
  badge = case_when(
    team == "Arsenal" ~ 'https://resources.premierleague.com/premierleague/badges/70/t3.png',
    team == "Aston Villa" ~ 'https://resources.premierleague.com/premierleague/badges/70/t7.png',
    team == "Brentford" ~ 'https://resources.premierleague.com/premierleague/badges/70/t94.png',
    team == "Brighton and Hove Albion" ~ 'https://resources.premierleague.com/premierleague/badges/50/t36.png',
    team == "Burnley" ~ 'https://resources.premierleague.com/premierleague/badges/70/t90.png',
    team == "Chelsea" ~ 'https://resources.premierleague.com/premierleague/badges/70/t8.png',
    team == "Crystal Palace" ~ 'https://resources.premierleague.com/premierleague/badges/50/t31.png',
    team == "Everton" ~ 'https://resources.premierleague.com/premierleague/badges/70/t11.png',
    team == "Leeds United" ~ 'https://resources.premierleague.com/premierleague/badges/70/t2.png',
    team == "Leicester City" ~ 'https://resources.premierleague.com/premierleague/badges/70/t13.png',
    team == "Liverpool" ~ 'https://resources.premierleague.com/premierleague/badges/70/t14.png',
    team == "Manchester City" ~ 'https://resources.premierleague.com/premierleague/badges/70/t43.png',
    team == "Manchester United" ~ 'https://resources.premierleague.com/premierleague/badges/70/t1.png',
    team == "Newcastle United" ~ 'https://resources.premierleague.com/premierleague/badges/70/t4.png',
    team == "Norwich City" ~ 'https://resources.premierleague.com/premierleague/badges/70/t45.png',
    team == "Southampton" ~ 'https://resources.premierleague.com/premierleague/badges/70/t20.png',
    team == "Tottenham Hotspur" ~ 'https://resources.premierleague.com/premierleague/badges/70/t6.png',
    team == "Watford" ~ 'https://resources.premierleague.com/premierleague/badges/70/t57.png',
    team == "West Ham United" ~ 'https://resources.premierleague.com/premierleague/badges/70/t21.png',
    team == "Wolverhampton Wanderers" ~ 'https://resources.premierleague.com/premierleague/badges/70/t39.png'
    ))%>%
  select(badge, everything())

# Create df containing list of match results (wins, draws, and losses)
wins.losses <- df.new %>% 
  arrange(match_date) %>%
  group_by(team) %>% 
  summarize(outcomes = list(match.result), groups = "drop") %>%
  select(team, outcomes)

# Create df containing list of cumulative points by match
cumulative.points <- df.new %>% # did not select for table
  summarize(cum.points = list(cum.pts), groups = "drop") %>%
  select(team, cum.points)

# Create df containing list of cumulative goal difference by match
cumulative.gd <- df.new %>%
  summarize(cum.goaldiff = list(cum.gd), groups = "drop") %>%
  select(team, cum.goaldiff)

# Create table position vector that is not attached to teams
table.position <- c(1:20)
  
# Merge data frames of lists into df.epl
df.epl <- left_join(df.epl, wins.losses, by=c("team" = "team"))
df.epl <- left_join(df.epl, cumulative.points, by=c("team" = "team"))
df.epl <- left_join(df.epl, cumulative.gd, by=c("team" = "team"))
df.epl$table.position <- table.position

# Create table
table.epl <- df.epl %>%
  select(table.position, badge, team, games, wins, draws, losses, goals.for, goals.against, goal.diff, cum.goaldiff, outcomes, points) %>%
  gt() %>%
  # badges
  gt_img_rows(badge) %>%
  # gd over season sparkline
  gt_plt_sparkline(cum.goaldiff, same_limit=FALSE, label=FALSE, type = "shaded", 
                   palette = c(yarrr::transparent("black", trans.val = 0),  # implement 'yarrr' for transparent colors of unwanted features
                               yarrr::transparent("blue", trans.val = 1),
                               yarrr::transparent("red", trans.val = 1),
                               yarrr::transparent("green", trans.val = 1),
                               yarrr::transparent("black", trans.val = 0)
                               )
                   ) %>%
  # win-draw-loss graphic
  gt_plt_winloss(outcomes, max_wins = 38, palette = c("#013369", "#D50A0A", "#65666f")) %>%
  # column alighment
  cols_align(
    align = "center",
    columns = c(table.position, badge, games, wins, draws, losses, goals.for, 
                goals.against, goal.diff, cum.goaldiff, outcomes, points)) %>%
  # footnotes
  tab_footnote("Goal difference over season not to scale across clubs.",
               locations = cells_column_labels(columns = cum.goaldiff)) %>%
  tab_footnote("Match outcomes: won (blue), drawn (grey), and lost (red).",
               locations = cells_column_labels(columns = outcomes)) %>%
  # column labels
  cols_label(
    table.position = "POS",
    badge = "",
    team = "CLUB",
    games = "PLAYED",
    wins = "WON",
    draws = "DRAWN",
    losses = "LOST",
    goals.for = "GF",
    goals.against = "GA",
    cum.goaldiff = "GD OVER SEASON",
    goal.diff = "GD",
    outcomes = "MATCH OUTCOMES",
    points = "PTS"
  )%>%
  tab_options(heading.title.font.size = 25) %>%
  tab_header(title = "PREMIER LEAGUE TABLE",
             subtitle= "2021-2022 Season")%>%
  tab_source_note("Data from Premier League | @gregoryjwolf") %>%
  # highlight table positions for CL, EL, ECL, and Relegation (note: depending on 
  # theme text is made bold; adjust in some themes with font_weight="lighter")
  gt_highlight_rows(rows = 1:4, fill="#00ff00", alpha=0.15, font_weight = "lighter") %>%
  gt_highlight_rows(rows = 5:6, fill="#0000ff", alpha=.15, font_weight = "lighter") %>%
  gt_highlight_rows(rows = 7, fill="#ffff00", alpha=0.15, font_weight = "lighter") %>%
  #gt_highlight_rows(rows = 8:17, fill = "#ffffff", font_weight = "normal")%>%
  gt_highlight_rows(rows = 18:20, fill="#ff0000", alpha=0.15, font_weight = "lighter") %>%  
  gt_theme_538() # need to use font_weight = "lighter" for lines 194-198 with gt_theme_538()

table.epl

# save gt() table
gtsave(table.epl, "final-epl21-22-table.png")







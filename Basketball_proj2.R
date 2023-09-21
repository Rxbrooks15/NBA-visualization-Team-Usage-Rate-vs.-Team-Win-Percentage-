#Project 2
# Raine Brookshire
# Basketball Usage Rate Variation Study

# Intro -------------------------------------------------------------------

#The NBA has been a topic of interest so I would like to explore some data 
#where I could look at the changes of a team winning percentage based on their 3 point percentage or 
#maybe looking at how teams utilize their players to determine their Efficientcy 

# Libraries ----------------------------------------------------------------

library(tidyverse) # for data manipulation
library(readxl) # for reading excel files into R
library(ggthemes) # add more themes to ggplot2
library(ggrepel) # add the text repel geom for better labeling
install.packages("ggpubr")
install.packages("tidyr")
update.packages()
install.packages("ggpubr", dependencies = TRUE)
library(jpeg)
library(png)
library(ggpubr)
library(grid)
install.packages("tidyr", repos = "https://cran.r-project.org")
5+5

# Data -------------------------------------------------------------

setwd("C:/Users/raine/Downloads/22F Basketball")

player_data <- read_excel(choose.files())
player_data
team_data <- read_excel(choose.files())
team_data                      
#"NBAStandings23.xlsx"


# Data exploration  -------------------------------------------------------
player_data <- read_excel(choose.files())
player_data
team_data <- read_excel(choose.files())
team_data 
player_data(View)
View(player_data)
View(team_data)
#interesting format for the dates 
#Teams are given their initials 
#From player_data maybe we can try to look at how efficient a player is 
player_data %>% 
  View()
team_data %>% 
  select(NW, )

player_data %>% 
  select(`USG%`, Player)

#It seems that we can look at the efficiency between players or teams 
#to look at the difference between organizations, we could look to calculate the varience 
#which compares the usage rates
#We could try to see if teams with higher usage rate would result in more wins 

#Question: Will teams with a greater usage rate (higher variance) result in more wins 
#Win percentage: number of games a team wins out of the total number of games played 
#USage rate in basketball definition:The usage of an NBA player consists of the number of chances a player takes out of 


#the possible chances a team has when that player is on
#extra info: The formula for usage rate is:

#Usage Rate = (Field Goal Attempts + Free Throw Attempts + Turnovers) / (Team Possessions)

#A player with a high usage rate is typically relied upon heavily by their team to create scoring 
#opportunities and handle the ball. This means that they will have the ball in their hands more often and take more shots.


#team usage rate(which is what we are looking at): A high team usage rate means that a team is relying heavily on a few key players 
#to generate offense and create scoring opportunities.
#a low team usage rate means that a team is distributing the ball
#and involving multiple players in their offensive scheme. 

# Cleaning and reshaping data ----------------------------------------------------------------

player_clean <- player_data %>% # save to a new variable
  
  # team_data %>%
  # filter(G > 10, Tm != "TOT") %>%
  # arrange(desc(MP)) %>%
  # group_by(Tm) %>%
  # top_n(8, MP) %>%
  # rename(ID = Rk, team = Tm, games_played = G, minutes_played = MP, usage_rate = `USG%`) %>%
  # arrange(team)

  select(Rk,
         Player,
         Tm, G,
         MP, `USG%`) %>% # select just the columns we want
  rename("ID" = Rk,
         "team" = Tm,
         "games_played" = G,
         "minutes_played" = MP,
         "usage_rate" = `USG%`) %>% # renames colums to make them more useful
  filter(games_played > 10,
         team != "TOT") %>% 
  # removes players with fewer than 10 games 
  arrange(desc(minutes_played)) %>% # sorts
  distinct(Player,
           .keep_all = T) %>% 
  # arrange(minutes_played)) %>% # sorts
  # distinct(Player,
  #          .keep_all = T) %>%# remove duplicate player entries
  group_by(team) %>% # perform action by team
  slice(1:8) %>% # take the top 8 players by minutes
  ungroup() %>% 
  arrange(team) # sort teams alphabetically

team_clean <- team_data %>% # saves to a new var
  #select(Rk, G, MP)
# team_clean %>% 
#View()

  select(Team,
         Overall) %>% 
  separate(col = Overall,
           into = c("wins", "losses"),
           sep = "-") %>% 
  # turns the single record column into a win and a loss column
  # which gives us the win percentage
  mutate("win_percentage" = as.numeric(wins)/(as.numeric(losses)
                                              + as.numeric(wins))) %>%
  # create new win% variable
  rename("team" = Team)

team_clean

# Analysis and working with missing teams----------------------------------------------------------------

player_summed <- player_clean %>% 
  group_by(team) %>% 
  mutate("ave" = mean(usage_rate),
         "diff" = abs(ave - usage_rate),# finds the difference between av usage and 1 teams usage to then get the variance 
         "variance" = sqrt(sum(diff * diff))) %>% # performs equation to get team variance
  #almost like calculating SD
  ungroup() %>% 
  summarize(team, variance) %>% # with distinct, turn table into just team and variance
  distinct() %>% 
  arrange(variance) # sort varience 

team_summed <- team_clean

## check join
missing_teams <- anti_join(player_summed,
                           team_summed,
                           by = "team")
# join tables by team and save entries that DON'T appear in both
# if missing_teams is empty, then the join is successful

missing_teams

joined_data <- merge(team_summed,
                     player_summed,
                     by = "team",
                     all = T) # join tables
joined_data %>% 
  select(team, year)


# Rename teams and "maps" team initials to their actual names 
#assuming some people have no knoweledge of the team names 
joined_data <- joined_data %>%
  mutate(team = case_when(
    team == "ATL" ~ "Atlanta Hawks",
    team == "BOS" ~ "Boston Celtics",
    team == "BRK" ~ "Brooklyn Nets",
    team == "CHO" ~ "Charlotte Hornets",
    team == "CHI" ~ "Chicago Bulls",
    team == "CLE" ~ "Cleveland Cavaliers",
    team == "DAL" ~ "Dallas Mavericks",
    team == "DEN" ~ "Denver Nuggets",
    team == "DET" ~ "Detroit Pistons",
    team == "GSW" ~ "Golden State Warriors",
    team == "HOU" ~ "Houston Rockets",
    team == "IND" ~ "Indiana Pacers",
    team == "LAC" ~ "Los Angeles Clippers",
    team == "LAL" ~ "Los Angeles Lakers",
    team == "MEM" ~ "Memphis Grizzlies",
    team == "MIA" ~ "Miami Heat",
    team == "MIL" ~ "Milwaukee Bucks",
    team == "MIN" ~ "Minnesota Timberwolves",
    team == "NOP" ~ "New Orleans Pelicans",
    team == "NYK" ~ "New York Knicks",
    team == "OKC" ~ "Oklahoma City Thunder",
    team == "ORL" ~ "Orlando Magic",
    team == "PHI" ~ "Philadelphia 76ers",
    team == "PHX" ~ "Phoenix Suns",
    team == "POR" ~ "Portland Trail Blazers",
    team == "SAC" ~ "Sacramento Kings",
    team == "SAS" ~ "San Antonio Spurs",
    team == "TOR" ~ "Toronto Raptors",
    team == "UTA" ~ "Utah Jazz",
    team == "WAS" ~ "Washington Wizards",
    team == "PHO" ~ "Phoenix Suns",
    TRUE ~ team
  ))
#View()

#Final Code ---------------------------------------------------------------

plot <- ggplot(data = joined_data,
               aes(x = variance,
                   y = win_percentage*100)) +
  # establish what the data is and what the axes are
  #interpolate = TRUE
  annotation_custom(rasterGrob(NBA,
                               width = unit(13, "cm"), 
                               height = unit(7, "cm")),
                    xmin=-Inf,
                    xmax = 19,
                    ymin= 65,
                    ymax=Inf) +
  #annotation_custom(rasterGrob(NBA,
                               #interpolate = TRUE),
                   # xmin=-Inf,
                   # xmax=Inf,
                    #ymin=-Inf,
                    #ymax=Inf) +
  
  #Run alternate version of code to have NBA logo centered
  # there does seem to be a little overlap within the tex and the image but 
  #additionally, I found it hard to change the alpha levels of the background image
  
  geom_point(color = "darkgreen", size = 5) +
  # add a point for each entry
  geom_smooth(method ='lm',
              formula = y~x,
              color = "green",
              se = TRUE) +
  # add a smoothed line (but in this case a straight regression line)
  geom_text_repel(aes(label = team),
                  color = "#000000",
                  fontface = "bold",
                  size = 4.5) +
  # add a team label to each data point
  scale_y_continuous(limits = c(0, 100),
                     name = "Win Percentage") +
  scale_x_continuous(limits = c(7, 23),
                     name = "Usage Rate Variance") +
  # change the range of the axes and add a better label
  ggtitle("Variance based on Team Usage Rate vs. Team Win Percentage") +
  # background_image(NBA)+
  
  # add a title
  theme_minimal()
# add a theme
plot

#IMPORTANT
#It is preferred to use the zoom feature because on the regular ploting console
#parts of the image are lost. I have saved and submiteed a copy of the plot
plot
# display the plot
#theme(text = element_text(face = "bold"))

#meant to be used for image on plot 
NBA <- file.choose()
NBA
NBA <- readPNG(NBA)
img<- jpeg::readJPEG(NBA)
NBA <- readJPEG(NBA)
NBA

#NOTE
##Question: Will teams with a greater usage rate (higher variance) result in more wins 

#A team with high variance in usage rate will have some players who are used heavily on offense
#a team with low variance in usage rate will have most of its players with similar usage rates,
#indicating a more evenly distributed offense

#From the data and the findings it appears that the greater the varience in usage rate (having a few major players that bring in the points)
#in more successful in winning more games.
#This is reasonable because having a bunch of average players would result in a lower skill gap than having a few really good players.
#Plus when all the good players are on the court, there will be overall better plays and more points generated.




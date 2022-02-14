library(tidyverse)
library(devtools)
#install_github("BillPetti/baseballr")
library(baseballr)
library(retrosheet)
library(readr)
library(Lahman)
library(pacman)
#devtools::install_github("rossdrucker/sportyR")
library(sportyR)


if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}
devtools::install_github(repo = "BillPetti/baseballr",force=TRUE)

# You can install using the pacman package using the following code:


##### approximate for 2021 data ####

#### data scrape ###
#x <- map_df(.x = seq.Date(as.Date('2021-04-02'), 
#                          as.Date('2021-09-30'), 
#                          'day'), 
#            ~get_game_pks_mlb(date = .x, 
#                              level_ids = c(1,11,12,13,14,15)))

#safe_mlb <- safely(get_pbp_mlb)

#df <- map(.x = x %>%
#            filter(status.codedGameState == "F") %>% 
#            pull(game_pk), 
#          ~safe_mlb(game_pk = .x)) %>%
#  map('result') %>%
#  bind_rows()

#saveRDS(df,"2021_mlb_milb_pbp.rds")
df<-readRDS("2021_mlb_milb_pbp.rds")

### retrosheet event data
#retro<-read_csv("~/Desktop/2021eve/all2021.csv",
#             col_names = FALSE)

#fields<-read.csv("http://bayesball.github.io/baseball/fields.csv")
#colnames(retro)<-fields[,"Header"]    

#setwd("~/Desktop/bayesian_baseball")
#df<-read.csv("mlb_2020_statcast_pitcher.csv")
#player_ref<-read.csv("SFBB-Player-ID-Map.csv")







## visualize zones using Jim Albert's hitting zone view ##
df<-df %>%
#  filter(description %in% c("hit_into_play","hit_into_play_no_out",
#"hit_into_play_score"),
#!events %in% c('home_run')) %>%
  ### rotate labels
  mutate(hc_x_new = hitData.coordinates.coordX,
         hc_y_new = -hitData.coordinates.coordY
  ) %>%
  ##### create hitting zones 
  mutate(
    hit_zone = case_when(
      #### shallow infield? 
      
      #### infield layer
      hc_x_new >= 0 & hc_x_new < 100 & hc_y_new <= -150 & hc_y_new >= -200 ~ "5",
      hc_x_new >= 100 & hc_x_new < 110 & hc_y_new <= -150 & hc_y_new >= -200 ~ "56",
      hc_x_new >= 110 & hc_x_new < 120 & hc_y_new <= -150 & hc_y_new >= -200 ~ "6",
      hc_x_new >= 120 & hc_x_new < 125 & hc_y_new <= -150 & hc_y_new >= -200 ~ "6M",
      hc_x_new >= 125 & hc_x_new < 130 & hc_y_new <= -150 & hc_y_new >= -200 ~ "4M",
      hc_x_new >= 130 & hc_x_new < 140 & hc_y_new <= -150 & hc_y_new >= -200 ~ "4",
      hc_x_new >= 140 & hc_x_new < 150 & hc_y_new <= -150 & hc_y_new >= -200 ~ "34",
      hc_x_new >= 150 & hc_x_new < 250 & hc_y_new <= -150 & hc_y_new >= -200 ~ "3",
      ### deep infield
      hc_x_new >= 0 & hc_x_new < 100 & hc_y_new <= -125 & hc_y_new >= -150 ~ "5D",
      hc_x_new >= 100 & hc_x_new < 110 & hc_y_new <= -125 & hc_y_new >= -150 ~ "56D",
      hc_x_new >= 110 & hc_x_new < 120 & hc_y_new <= -125 & hc_y_new >= -150 ~ "6D",
      hc_x_new >= 120 & hc_x_new < 125 & hc_y_new <= -125 & hc_y_new >= -150 ~ "6MD",
      hc_x_new >= 125 & hc_x_new < 130 & hc_y_new <= -125 & hc_y_new >= -150 ~ "4MD",
      hc_x_new >= 130 & hc_x_new < 140 & hc_y_new <= -125 & hc_y_new >= -150 ~ "4D",
      hc_x_new >= 140 & hc_x_new < 150 & hc_y_new <= -125 & hc_y_new >= -150 ~ "34D",
      hc_x_new >= 150 & hc_x_new < 250 & hc_y_new <= -125 & hc_y_new >= -150 ~ "3D", 
      ## shallow outfield (split by 24 spacing giving wideness; 185 - 65 / 5 = 24)
      hc_x_new >= 0 & hc_x_new < 65 & hc_y_new <= -100 & hc_y_new >= -125 ~ "7LS",
      hc_x_new >= 65 & hc_x_new < 89 & hc_y_new <= -100 & hc_y_new >= -125 ~ "7S",
      hc_x_new >= 89 & hc_x_new < 113 & hc_y_new <= -100 & hc_y_new >= -125 ~ "78S",
      hc_x_new >= 113 & hc_x_new < 137 & hc_y_new <= -100 & hc_y_new >= -125 ~ "8S",
      hc_x_new >= 137 & hc_x_new < 161 & hc_y_new <= -100 & hc_y_new >= -125 ~ "89S",
      hc_x_new >= 161 & hc_x_new < 185 & hc_y_new <= -100 & hc_y_new >= -125 ~ "9S",
      hc_x_new >= 185 & hc_x_new < 250 & hc_y_new <= -100 & hc_y_new >= -125 ~ "9LS", 
      #### Normal Outfield length ### 
      hc_x_new >= 0 & hc_x_new < 65 & hc_y_new <= -75 & hc_y_new >= -100 ~ "7L",
      hc_x_new >= 65 & hc_x_new < 89 & hc_y_new <= -75 & hc_y_new >= -100 ~ "7",
      hc_x_new >= 89 & hc_x_new < 113 & hc_y_new <= -75 & hc_y_new >= -100 ~ "78",
      hc_x_new >= 113 & hc_x_new < 137 & hc_y_new <= -75 & hc_y_new >= -100 ~ "8",
      hc_x_new >= 137 & hc_x_new < 161 & hc_y_new <= -75 & hc_y_new >= -100 ~ "89",
      hc_x_new >= 161 & hc_x_new < 185 & hc_y_new <= -75 & hc_y_new >= -100 ~ "9",
      hc_x_new >= 185 & hc_x_new < 250 & hc_y_new <= -75 & hc_y_new >= -100 ~ "9L", 
      ##### deep outfield ###
      hc_x_new >= 0 & hc_x_new < 65 & hc_y_new <= -50 & hc_y_new >= -75 ~ "7LD",
      hc_x_new >= 65 & hc_x_new < 89 & hc_y_new <= -50 & hc_y_new >= -75 ~ "7D",
      hc_x_new >= 89 & hc_x_new < 113 & hc_y_new <= -50 & hc_y_new >= -75 ~ "78D",
      hc_x_new >= 113 & hc_x_new < 137 & hc_y_new <= -50 & hc_y_new >= -75 ~ "8D",
      hc_x_new >= 137 & hc_x_new < 161 & hc_y_new <= -50 & hc_y_new >= -75 ~ "89D",
      hc_x_new >= 161 & hc_x_new < 185 & hc_y_new <= -50 & hc_y_new >= -75 ~ "9D",
      hc_x_new >= 185 & hc_x_new < 250 & hc_y_new <= -50 & hc_y_new >= -75 ~ "9LD", 
      #### xtra deep out field
      hc_x_new >= 0 & hc_x_new < 113 & hc_y_new <= -25 & hc_y_new >= -50 ~ "78XD",
      hc_x_new >= 113 & hc_x_new < 137 & hc_y_new <= -25 & hc_y_new >= -50 ~ "8XD",
      hc_x_new >= 137 & hc_x_new < 250 & hc_y_new <= -25 & hc_y_new >= -50 ~ "89XD",
      TRUE ~ "No Zone"
    )
  )  #%>%
### filter out plays behind catcher or extra deep
 ## filter(hit_zone != "No Zone")



#visualize zones
df %>%
  ggplot(aes(x=hc_x_new,y=hc_y_new,color=hit_zone)) +
  geom_point(alpha=1/30) +
  geom_path(lwd = 0.01,linetype = 1,
            color="grey") 


df<-df %>%
   mutate(runners = pmap(
     .l = list(matchup.postOnThird.id,matchup.postOnSecond.id,matchup.postOnFirst.id),
     .f = function(...) sum(!rlang::are_na(c(...)))
   ))

df<-df %>%
  mutate(hit_situation = str_c(df$count.balls.start,'--',df$count.strikes.start,'--',df$count.outs.start,'--',df$runners))




##### add mcmc data
new_df<-df %>%
  filter(about.inning <= 9) %>%
  mutate(
    abs = case_when(
    complete.cases(batted.ball.result) ~ 1,
    lead(game_date) != game_date ~ 1,
    lead(about.isTopInning) != about.isTopInning ~ 1,
    TRUE ~ 0
  ),
  ### abs_state
  abs_state = case_when(
      result.event == "Field Out" ~ "out",
      result.event == "Forceout" ~ "out",
      result.event == "Grounded Into DP" ~ "out",
      result.event == "Double Play" ~ "out",
      result.event == "Fielders Choice Out" ~ "out",
      result.event == "Sac Bunt" ~ "out",
      result.event  == "Single" ~ "hit",
      result.event  == "Double" ~ "hit",
      result.event == "Triple Play" ~ "out",
      result.event == "Field Error" ~ "out",   #### converting errors to non hit event
      result.event == "Triple" ~ "hit",
      result.event == "Sac Fly Double Play" ~ "out",
      result.event == "Fielders Choice" ~ "out",
      result.event == "Sac Fly"  ~ "out",
      result.event == "Flyout" ~ "out",
      result.event == "Pop Out" ~ "out",
      result.event == "Groundout" ~ "out",
      result.event == "Lineout" ~ "out",
      result.event == "Bunt Pop Out" ~ "out",
      result.event == "Bunt Groundout" ~ "out", 
      result.event == "Bunt Lineout" ~ "out",
      result.event =="Sac Bunt Double Play" ~ "out",
      TRUE ~ as.character(NA)),
  last_play_abs = ifelse(lag(abs) == 1 | row_number() == 1,"Y","N"),
  batter_team = ifelse(about.isTopInning == "TRUE",as.character(away_team),as.character(home_team)),
  batter_score = ifelse(batter_team == home_team,details.homeScore,details.awayScore),
  batter_level = ifelse(batter_team == home_team,home_level_name,away_league_name),
  batter_level = case_when(
    batter_level == "Major League Baseball" ~ "MLB",
    batter_level == "American League" ~ "MLB",
    batter_level == "National League" ~ "MLB",
    batter_level == "Double-A" ~ "AA",
    batter_level == "Double-A Northeast" ~ "AA",
    batter_level == "High-A" ~ "A",
    batter_level == "High-A Central" ~ "A",
    batter_level == "Low-A" ~ "A",
    batter_level == "Low-A Southeast" ~ "A",
    batter_level == "Triple-A" ~ "AAA",
    batter_level == "Triple-A East" ~ "AAA",
    batter_level == "High-A East" ~ "A",
    batter_level == "Low-A East" ~ "A",
    batter_level == "Double-A Central" ~ "AA",
    batter_level == "Low-A West" ~ "A",
    batter_level == "High-A West" ~ "A",
    batter_level == "Double-A South" ~ "AA",
    batter_level == "Triple-A West" ~ "AAA",
    TRUE ~ as.character(NA))
  ) %>%
  dplyr::select(game_date,result.event,hit_situation,hit_zone,batter_score,matchup.batter.fullName,matchup.batter.id,batter_team,batter_level,abs,abs_state,last_play_abs,hc_x_new,hc_y_new) %>%
  dplyr::group_by(last_play_abs) %>%
  dplyr::mutate(seq = ifelse(last_play_abs == "Y",row_number(),NA)) %>%
  ungroup() %>%
  fill(seq) %>%
  group_by(seq) %>%
  mutate(play_of_seq = row_number()) %>%
  ungroup() %>%
  mutate(plays_in_seq = ifelse(lead(last_play_abs) == "Y",
                               play_of_seq,NA)) %>%
  fill(plays_in_seq,.direction = "up") %>%
  unite(play_state,hit_zone,
        sep = "--",remove = FALSE) %>%
  mutate(next_state = ifelse(abs ==1, abs_state,lead(play_state)))




##### this idea brought in from the following code:
# Ron Yurko NFL Markov: https://github.com/ryurko/nflscrapR-data/blob/master/R/markov_model.R
#Greg Ackerman Erie Otters Markov: https://github.com/gregalytics/2021-Big-Data-Cup/blob/main/Markov%20Chain.R



#### Markov Chain Prep ----
#Calculated freq of each state
trans_state_df <- new_df %>%
  group_by(play_state) %>%
  count() %>%
  ungroup() %>%
  mutate(state_prop = n / sum(n)) %>% 
  arrange(desc(state_prop))

#abs states
absorption_states <- unique(new_df$abs_state)[c(1,2)]


#calculate transition probabilities
transitions <- new_df %>% 
  group_by(play_state, next_state) %>%
  count() %>%
  ungroup() %>%
  group_by(play_state) %>%
  mutate(total_plays = sum(n)) %>%
  ungroup() %>%
  mutate(transition_prob = n / total_plays) %>%
  # Append rows that are just the absorptions for ease in making the 
  # complete transition matrix:
  bind_rows(data.frame(play_state = absorption_states,
                       next_state = absorption_states,
                       transition_prob = rep(1, length(absorption_states)))) %>% 
  filter(!is.na(next_state))

# Create transition matrix 
transition_matrix <- transitions %>%
  dplyr::select(play_state, next_state, transition_prob) %>%
  arrange(desc(play_state), desc(next_state)) %>% 
  spread(next_state, transition_prob) 

transition_matrix[is.na(transition_matrix)]<-0


## Fundamental Matrix Calculation ----

# Find the indices of absorption states:
row_absorption_i <- which(transition_matrix$play_state %in% absorption_states)
col_absorption_i <- which(colnames(transition_matrix) %in% absorption_states)

# Grab the Q matrix - n x n transition matrix for transient states:
q_matrix <- as.matrix(transition_matrix[1:(row_absorption_i[1] - 1),
                                        2:(col_absorption_i[1] - 1)])
# Grab the R matrix - n x r transition matrix to the absorption states:
r_matrix <- as.matrix(transition_matrix[1:(row_absorption_i[1] - 1),
                                        col_absorption_i])

# Calculate the fundamental matrix - (I-Q)**(-1)
fundamental_matrix <- solve(diag(nrow = nrow(q_matrix),
                                 ncol = nrow(q_matrix)) - q_matrix)

expected_n_plays<-rowSums(fundamental_matrix)

prop_abs<-fundamental_matrix %*% r_matrix

absorption_df <- as.data.frame(prop_abs) %>%
  mutate(play_state = rownames(prop_abs),
         expected_n_plays = expected_n_plays) %>% 
  as_tibble() 

absorption_df[colnames(absorption_df) %in% absorption_states] %>% 
  gather(absorbing_state, absorbing_prob) %>%
  group_by(absorbing_state) %>%
  summarise(max_absorbing_prob = max(absorbing_prob))

hit_contribution_values <- absorption_df %>% 
  dplyr::select(play_state, hit) %>% 
  filter(play_state != "NA") %>% 
  arrange(desc(hit))



player_values<-new_df %>%
  left_join(hit_contribution_values) %>%
  dplyr::select(game_date,result.event,hit,hit_situation,hit_zone,batter_score,matchup.batter.fullName,matchup.batter.id,batter_team,batter_level,abs,abs_state,last_play_abs,hc_x_new,hc_y_new) %>%
  mutate(contr = case_when(
    is.na(abs_state) ~ lead(hit) - hit,
    abs_state == "hit" ~ 1 - hit,
    abs_state == "out" ~ 0 - hit)
  )

pv<-player_values %>%
  filter(!is.na(result.event),
         hit_zone != "No Zone") %>%
  dplyr::select(game_date,result.event,matchup.batter.fullName,matchup.batter.id,batter_team,batter_level,contr,hit,hit_situation,hit_zone,batter_score,abs,abs_state,hc_x_new,hc_y_new) %>%
  group_by(matchup.batter.fullName,matchup.batter.id,batter_team,batter_level) %>%
  summarise(contr = sum(contr,na.rm = TRUE),
            n = n())

pv_zone<-player_values %>%
  filter(!is.na(result.event),
         hit_zone != "No Zone") %>%
  dplyr::select(game_date,result.event,matchup.batter.fullName,matchup.batter.id,batter_team,batter_level,contr,hit,hit_situation,hit_zone,batter_score,abs,abs_state,hc_x_new,hc_y_new) %>%
  group_by(matchup.batter.fullName,matchup.batter.id,batter_team,hit_zone,batter_level) %>%
  summarise(contr = sum(contr,na.rm = TRUE),
            n = n())

final_df<-new_df %>%
  left_join(pv_zone,by=c("matchup.batter.fullName","batter_team","hit_zone"))


##### create avg zones ####
### create data frame positions for zones
x <- c(195,173,149,125,101,77,60,        ## deep OF
       195,173,149,125,101,77,60,        ## OF
       195,173,149,125,101,77,60,        ## Shallow OF
       173,155,145,130,120,105,95,77,   ## Deep IF
       167,155,145,130,120,105,95,88,  ## IF
       94,125,163)        ### Xtra Deep OF
y <- c(-62.5,-62.5,-62.5,-62.5,-62.5,-62.5,-62.5,
       -87.5,-87.5,-87.5,-87.5,-87.5,-87.5,-87.5,
       -112.5,-112.5,-112.5,-112.5,-112.5,-112.5,-112.5,
       -137.5,-137.5,-137.5,-137.5,-137.5,-137.5,-137.5,-137.5,
       -160,-160,-160,-160,-160,-160,-160,-160,
       -37.5,-37.5,-37.5)
z <- c("9LD","9D","89D","8D","78D","7D","7LD",
       "9L","9","89","8","78","7","7L",
       "9LS","9S","89S","8S","78S","7S","7LS",
       "3D","34D","4D","4MD","6MD","6D","56D","5D",
       "3","34","4","4M","6M","6","56","5",
       "78XD","8XD","89XD")

des<-c("Deep OF","Deep OF","Deep OF","Deep OF","Deep OF","Deep OF","Deep OF",
       "OF","OF","OF","OF","OF","OF","OF",
       "Shallow OF","Shallow OF","Shallow OF","Shallow OF","Shallow OF","Shallow OF","Shallow OF",
       "Deep IF","Deep IF","Deep IF","Deep IF","Deep IF","Deep IF","Deep IF","Deep IF",
       "IF","IF","IF","IF","IF","IF","IF","IF",
       "xDeep OF","xDeep OF","xDeep OF")
       

d <- data.frame(x = x, y = y,z=z,des=des)

## 
#final_df<-final_df %>%
#  dplyr::select(-x,-y,-des)

## left join onto dataset
final_df<-final_df %>%
  left_join(d,by=c("hit_zone" = "z"))

##### visuals ####
### show hit probability by zone? 
### Top players or prospects? 
### show outs above expectation

### show outs or hits adjust to shifts

### show hard hit % by contribution 



#### add visualizations for a specific batter; create a shiny app ###
### Jake Cronenworth


final_df %>%
  mutate(contr = round(contr,1)) %>%
  filter(matchup.batter.fullName == "Jake Cronenworth",
         #des != "IF",
         batter_team == "San Diego Padres") %>%
  ggplot() +
  geom_point(aes(x=hc_x_new,y=hc_y_new,alpha=1/500,color=abs_state)) +
  geom_text(aes(x=x,y=y,label = contr),size = 3,color="yellow") +
  geom_curve(x = 20, xend = 240, y = -100, yend = -100,
             curvature = -.50,color = "grey") +
  geom_segment(x=125, xend = 20, y=-200, yend = -100,color="grey") +
  geom_segment(x=125, xend = 240, y=-200, yend = -100,color="grey") +
  theme(legend.position = "bottom",
        axis.line = element_blank(),
        panel.background = element_rect(fill = "dark green",
                                        colour = "dark green"),
        panel.border=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +  
  geom_curve(x = 86, xend = 170, y = -160, yend = -160,
  curvature = -.55, linetype = "dotted") +
  ### horizontal zones ###
  geom_segment(x=201, xend = 53, y=-50, yend = -50,linetype="dotted",color="white") +    #XtraDeep
  geom_segment(x=218, xend = 40, y=-75, yend = -75,linetype="dotted",color="white") +    #Deep 
  geom_segment(x=230,xend = 24,y=-100,yend=-100,linetype="dotted",color="white")    +    #Outfield
  geom_segment(x=205,xend = 50,y=-125,yend=-125,linetype="dotted",color="white")    +     # Shallow Outfield
  geom_segment(x=177,xend = 74,y=-150,yend=-150,linetype="dotted",color="white") +       #IF
  ##### vertical zones ###
  geom_segment(x=65,xend=65,y=-125,yend=-50,linetype="dotted",color="white") +   #Far left OF
  geom_segment(x=85,xend=85,y=-160,yend=-50,linetype="dotted",color="white") +   #Left Center OF
  geom_segment(x=110,xend=110,y=-185,yend=-10,linetype="dotted",color="white") +
  geom_segment(x=137,xend=137,y=-185,yend=-10,linetype="dotted",color="white") +
  geom_segment(x=160,xend=160,y=-160,yend=-50,linetype="dotted",color="white") +
  geom_segment(x=180,xend=180,y=-125,yend=-50,linetype="dotted",color="white") +
  geom_segment(x=100,xend=100,y=-175,yend=-125,linetype="dotted",color="white") +
  geom_segment(x=150,xend=150,y=-175,yend=-125,linetype="dotted",color="white") +
  geom_segment(x=125,xend=125,y=-200,yend=-125,linetype="dotted",color="white") +
  labs(
    title = "Jake Cronenworth Total Hit Contributions by Zone",
    subtitle = "2021 season",
    caption = "Approx. Retrosheet Zones: https://www.retrosheet.org/location.htm"
  ) +
  ylim(-205,0) +
  xlim(15,240)

ggsave("Cronenworth_2021.png")






### Top Minor Leaguer ####
#Ryan Vilade & Albuquerque Isotopes
final_df %>%
  mutate(contr = round(contr,1)) %>%
  filter(matchup.batter.fullName == "Ryan Vilade",
         #des != "IF",
         batter_team == "Albuquerque Isotopes") %>%
  ggplot() +
  geom_point(aes(x=hc_x_new,y=hc_y_new,alpha=1/500,color=abs_state)) +
  geom_text(aes(x=x,y=y,label = contr),size = 3,color="yellow") +
  geom_curve(x = 20, xend = 240, y = -100, yend = -100,
             curvature = -.50,color = "grey") +
  geom_segment(x=125, xend = 20, y=-200, yend = -100,color="grey") +
  geom_segment(x=125, xend = 240, y=-200, yend = -100,color="grey") +
  theme(legend.position = "bottom",
        axis.line = element_blank(),
        panel.background = element_rect(fill = "dark green",
                                        colour = "dark green"),
        panel.border=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +  
  geom_curve(x = 86, xend = 170, y = -160, yend = -160,
             curvature = -.55, linetype = "dotted") +
  ### horizontal zones ###
  geom_segment(x=201, xend = 53, y=-50, yend = -50,linetype="dotted",color="white") +    #XtraDeep
  geom_segment(x=218, xend = 40, y=-75, yend = -75,linetype="dotted",color="white") +    #Deep 
  geom_segment(x=230,xend = 24,y=-100,yend=-100,linetype="dotted",color="white")    +    #Outfield
  geom_segment(x=205,xend = 50,y=-125,yend=-125,linetype="dotted",color="white")    +     # Shallow Outfield
  geom_segment(x=177,xend = 74,y=-150,yend=-150,linetype="dotted",color="white") +       #IF
  ##### vertical zones ###
  geom_segment(x=65,xend=65,y=-125,yend=-50,linetype="dotted",color="white") +   #Far left OF
  geom_segment(x=85,xend=85,y=-160,yend=-50,linetype="dotted",color="white") +   #Left Center OF
  geom_segment(x=110,xend=110,y=-185,yend=-10,linetype="dotted",color="white") +
  geom_segment(x=137,xend=137,y=-185,yend=-10,linetype="dotted",color="white") +
  geom_segment(x=160,xend=160,y=-160,yend=-50,linetype="dotted",color="white") +
  geom_segment(x=180,xend=180,y=-125,yend=-50,linetype="dotted",color="white") +
  geom_segment(x=100,xend=100,y=-175,yend=-125,linetype="dotted",color="white") +
  geom_segment(x=150,xend=150,y=-175,yend=-125,linetype="dotted",color="white") +
  geom_segment(x=125,xend=125,y=-200,yend=-125,linetype="dotted",color="white") +
  labs(
    title = "Ryan Vilade Total Hit Contributions by Zone",
    subtitle = "2021 season",
    caption = "Approx. Retrosheet Zones: https://www.retrosheet.org/location.htm"
  ) +
  ylim(-205,0) +
  xlim(15,240)


#### Contribution considered hits above average? 
#### add in vertical lines for zones? 
### change brightness of colors
### show top end of curve



### create a plot that would show face by zone
top_zone<-pv_zone %>%
  filter(n >= 15,
         batter_level == "MLB") %>% 
  group_by(hit_zone) %>%
  top_n(1,contr) %>%
  left_join(d,by=c("hit_zone" = "z"))

top_zone<-paste("baseball.fandom.com/wiki/")

### Top MLB hitter by zone
top_zone %>%
  mutate(contr = round(contr,1)) %>%
  #filter(des != "IF") %>%
  ggplot() +
  geom_text(aes(x=x,y=y,label = matchup.batter.fullName),size = 2,color="yellow",nudge_y = -0.2,check_overlap = TRUE) +
  geom_text(aes(x=x,y=y,label = contr),size = 2,color="white",nudge_y = -4,check_overlap = TRUE) +
  geom_curve(x = 20, xend = 240, y = -100, yend = -100,
             curvature = -.50,color = "grey") +
  geom_segment(x=125, xend = 20, y=-200, yend = -100,color="grey") +
  geom_segment(x=125, xend = 240, y=-200, yend = -100,color="grey") +
  theme(legend.position = "bottom",
        axis.line = element_blank(),
        panel.background = element_rect(fill = "dark green",
                                        colour = "dark green"),
        panel.border=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +  
  geom_curve(x = 86, xend = 170, y = -160, yend = -160,
             curvature = -.55, linetype = "dotted") +
  ylim(-205,0) +
  xlim(15,240) +
  labs(title = "Top Hitters - Zone Contributions",
       subtitle = "2021 MLB Season")

ggsave("Top_MLB_Hitters_Zone_2021.png")












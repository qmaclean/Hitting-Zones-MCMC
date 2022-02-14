# Hitting-Zones-MCMC
Markov Chain for Hitting Zones

# Introduction
Retrosheet data has each section of the field portioned into zones. The purpose of this analysis is to understand what players are hitting well above their contribution for a specific zone as broken out by [Jim Albert](https://baseballwithr.wordpress.com/2021/12/06/downloading-2021-retrosheet-data-and-batted-ball-locations/)

<img src="https://github.com/qmaclean/Hitting-Zones-MCMC/blob/main/Retrosheet_Codes.png" width="100%" />


This will help to isolate a player's true hitting performance regardless of previous context. To do so, I've create a Monte Carlo Markov Chain building upon the methods of [Ron Yurko](https://github.com/ryurko/nflscrapR-data/blob/master/R/markov_model.R) and [Greg Ackerman](https://github.com/gregalytics/2021-Big-Data-Cup/blob/main/Markov%20Chain.R) to identify those with the highest contribution per zone. 

# Application: Jake Cronenworth 2021 Hitting Zone
<img src="https://github.com/qmaclean/Hitting-Zones-MCMC/blob/main/Cronenworth_2021.png" width="100%" />

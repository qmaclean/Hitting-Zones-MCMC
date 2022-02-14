
# Introduction
Retrosheet data has each section of the field portioned into zones. The purpose of this analysis is to understand what players have the best net contributions for a specific zone as broken out by [Jim Albert](https://baseballwithr.wordpress.com/2021/12/06/downloading-2021-retrosheet-data-and-batted-ball-locations/). The higher the value the more probable that zone converts into a hit. Players that lead lower quality zones are able to attack those zones very strategically. 

<img src="https://github.com/qmaclean/Hitting-Zones-MCMC/blob/main/Retrosheet_Codes.png" width="50%" />


This will help to isolate a player's true hitting performance regardless of previous context. To do so, I've create a Monte Carlo Markov Chain building upon the methods of [Ron Yurko](https://github.com/ryurko/nflscrapR-data/blob/master/R/markov_model.R) and [Greg Ackerman](https://github.com/gregalytics/2021-Big-Data-Cup/blob/main/Markov%20Chain.R) to identify those with the highest contribution per zone. Some zones are more probable of converting to a hit than others: 



## Application: Jake Cronenworth 2021 Hitting Zone
<img src="https://github.com/qmaclean/Hitting-Zones-MCMC/blob/main/Cronenworth_2021.png" width="100%" />


## 2021 Top Hitters by Zones
<img src="https://github.com/qmaclean/Hitting-Zones-MCMC/blob/main/Top_MLB_Hitters_Zone_2021.png" width="100%" />

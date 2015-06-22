##
## Analysis code for Thomas et al. (2015): Saami reindeer herders cooperate with social group members and genetic kin
##
## The code in this file produces tables 1 to 4 in the main text.
##
## Author: Matthew Gwynfryn Thomas
##
##      {------- email --------}
##         {-- twitter --}
##      mgt@matthewgthomas.co.uk
##          {------ web -------}
##
##
## Copyright (c) 2015 Matthew Gwynfryn Thomas
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License along
## with this program; if not, write to the Free Software Foundation, Inc.,
## 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
##
library(plyr)
source("data-funcs/import data.r")

######################################################
## Table 1: In-degrees split by whether the herder is on their siida’s leadership board or not
##
ddply(herders, .(leaderYN), summarise, N=length(deg.in), mean=mean(deg.in), sd=sd(deg.in), median=median(deg.in))


######################################################
## Table 2: Counts of people receiving a gift or not
##
ftable( xtabs(~ SameSiida + IsRelated + GiftGiven, data=herders.wide.subset))


######################################################
## Table 3: Best-fitting GEE model
##
source("GEE/GEE models.r")
summary(gee.r_siida)    # unstandardised
summary(gee.r_siida.z)  # standardised


######################################################
## Table 4: Gifts given to older/younger herders
##
herders.wide$IsYounger = ifelse(is.na(herders.wide$AgeDiff), "Unknown",
                                ifelse(herders.wide$AgeDiff < 0, "Younger", "Older"))  # younger recipients are -ve age differences
ftable(xtabs(~ IsRelated + IsYounger, data=subset(herders.wide, GiftGiven==1)))


######################################################
## Table 5: Reasons for gifts
##
# this was produced in Excel using the gift reasons in herders.csv

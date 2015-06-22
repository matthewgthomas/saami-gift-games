##
## Analysis code for Thomas et al. (2015): Saami reindeer herders cooperate with social group members and genetic kin
##
## Load data and create gift network.
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
rm(list=ls())

require(igraph)

################################################
## import data
##
source("./data/relatedness.Rdmpd")  # dataframe 'kin.mat.r'

gifts <- read.csv("./data/gifts.csv", header=T, stringsAsFactors=F)
herders <- read.csv("./data/herders.csv", header=T, stringsAsFactors=T)

givers_list <- as.vector(subset(herders, Interviewed==1, select=c(id))$id)


######################################################
## Set up gift network
##
# make graph with gifts data and vertex info from 'herders' data frame
g <- graph.data.frame(gifts, vertices=herders, directed=T)

# who has the highest in-degree?
deg     <- degree(g)
deg.in  <- degree(g, mode="in")
deg.out <- degree(g, mode="out")

# put degrees into dataframe
herders$deg     <- deg[ as.character(herders$id) ]
herders$deg.in  <- deg.in[ as.character(herders$id) ]
herders$deg.out <- deg.out[ as.character(herders$id) ]


######################################################
## Load dyadic gifts data (or create if doesn't exist)
##
dyadic_data_file = "./data/herders-gifts-wide.csv"

if (file.exists(dyadic_data_file)) {
  herders.wide = read.csv("./data/herders-gifts-wide.csv")
} else {
  source("data-funcs/create dyadic data.r")
}

herders.wide.subset = subset(herders.wide, Ego %in% givers_list)

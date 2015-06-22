##
## Analysis code for Thomas et al. (2015): Saami reindeer herders cooperate with social group members and genetic kin
##
## Create adjacency matrices for correlations (see main text and table S3).
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
library(igraph)
library(car)

source("data-funcs/import data.r")

##
## adj.X.s variable (with .s suffix) are symmetrical matrics
## adj.X variables (no suffix) are 30x75 matrices
##

########################################################################
## gift adjacency matrix
## (received gift=1; else=0)
##
dat <- subset(herders.wide, GiftGiven==1, select=c("Ego", "Alter")) # subset data
g.gift <- graph.data.frame(dat, vertices=herders, directed=T)           # turn relevant subset into a graph object
adj.gift.s <- get.adjacency( g.gift, sparse=F )                           # create adjaceny matrix

# keep only gift givers (interviewees) in rows
adj.gift <- subset(adj.gift.s, row.names(adj.gift.s) %in% givers_list)

adj.gift.s[!(row.names(adj.gift.s) %in% givers_list),] <- NA


########################################################################
## kinship matrix
##
dat <- subset(herders.wide, r>0, select=c("Ego", "Alter", "r"))
# add weight column so will be recognised as a weighted graph
dat$weight <- dat$r
# rename 'r' column so can be visualised in Gephi
names(dat)[names(dat)=="r"] <- "relatedness"
g.kin <- graph.data.frame(dat, vertices=herders, directed=F)
adj.r.s <- get.adjacency( g.kin, sparse=F, attr="relatedness" )

adj.r.s[!(row.names(adj.r.s) %in% givers_list),] <- NA

# keep only gift givers (interviewees) in rows
adj.r <- subset(adj.r.s, row.names(adj.r.s) %in% givers_list)

# make versions for kin (yes/no) and close kin (yes/no)
adj.kin = adj.r
adj.kin[adj.kin > 0] = 1
adj.closekin = adj.r
adj.closekin[adj.closekin >= 0.5] = 1


########################################################################
## siida membership matrix
##
dat <- subset(herders.wide, SameSiida==1, select=c("Ego", "Alter"))
g.siida <- graph.data.frame(dat, vertices=herders, directed=T)
adj.siida.s <- get.adjacency( g.siida, sparse=F )

adj.siida.s[!(row.names(adj.siida.s) %in% givers_list),] <- NA

# keep only gift givers (interviewees) in rows
adj.siida <- subset(adj.siida.s, row.names(adj.siida.s) %in% givers_list)


########################################################################
## Clean up
##
# remove temp data frame and unload igraph package
rm(dat, g.kin, g.siida, g.gift)

detach("package:igraph")

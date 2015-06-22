##
## Analysis code for Thomas et al. (2015): Saami reindeer herders cooperate with social group members and genetic kin
##
## The code in this file corresponds to analyses in the supplementary information.
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
source("data-funcs/import data.r")
library(ggplot2)
library(plyr)

########################################################################
## Fig S1: Boxplot of age distribution
##
summary(herders$Age)
boxplot(herders$Age, ylab="Age (years)")


########################################################################
## Fig S2: Distribution of herd sizes
##
herd.sizes = na.omit(herders$num.reindeer)
summary(herd.sizes)

ggplot(herders, aes(x=num.reindeer)) + geom_histogram(drop=TRUE) +
  #scale_y_continuous(limits=c(0, 1.4e7)) +
  scale_x_continuous("No. reindeer owned in 2012", limits=c(0, 1700)) +
  coord_cartesian(xlim = c(0, 1700), ylim = c(0, 10)) +
  
  theme_bw() +
  #eliminates baground, gridlines, and chart border
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) + theme(axis.line = element_line(color = 'black'))


######################################################
## Table S1: Descriptive statistics for participants
##
interviewed = subset(herders, Interviewed==1)
nrow(subset(interviewed, Sex=="m"))
nrow(subset(interviewed, s3.siida_leader==1))
ddply(interviewed, .(s5.marriage), summarise, n=length(s5.marriage))
ddply(interviewed, .(s6.education_level), summarise, n=length(s6.education_level))
mean(interviewed$Age); median(interviewed$Age); sd(interviewed$Age)
mean(interviewed$num.children); median(interviewed$num.children); sd(interviewed$num.children)
rm(interviewed)


######################################################
## Table S2: Descriptive statistics for the gift network
##
library(igraph)

no.clusters(g)
graph.density(g)
reciprocity(g)
transitivity(g)

detach("package:igraph")


################################################
## Table S3: QAP correlation matrix
##
source("data-funcs/create adjacency matrices.r")

library(sna)

g.qap <- array(dim=c(4, nrow(adj.gift), ncol(adj.gift)))
g.qap[1,,] <- adj.gift
g.qap[2,,] <- adj.kin
g.qap[3,,] <- adj.closekin
g.qap[4,,] <- adj.siida

gcor(g.qap)

detach("package:sna")


################################################
## Table S4: GEE models
##
source("GEE/GEE models.r")
model.sel(gee.r, gee.siida, gee.r_siida, gee.r_siida_int, gee.r_siida_herd, gee.full, rank=QIC)


################################################
## Table S5: Gift reasons
##
# this was produced using Excel and the reasons listed in herders.csv

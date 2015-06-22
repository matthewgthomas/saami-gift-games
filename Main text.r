##
## Analysis code for Thomas et al. (2015): Saami reindeer herders cooperate with social group members and genetic kin
##
## The code in this file corresponds to analyses in the main text.
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
library(igraph)
source("data-funcs/import data.r")


#######################################################################################
## Description of the District and Gift Network
##
# sex
ddply(herders, .(Sex), summarise, N=length(Sex))
# age
median(herders$Age, na.rm=T)
# herd sizes
summary(na.omit(herders$num.reindeer))

# no. gifts
nrow(gifts)
# no. recipients
length(unique(gifts$Alter))

# how many gifts are given from/to the same siida
(gifts.same = nrow(subset(herders.wide.subset, GiftGiven==1 & SameSiida==1)))
# how many gifts are *NOT* given from/to the same siida
(gifts.other <- nrow(subset(herders.wide.subset, GiftGiven==1 & SameSiida==0)))
#! 45 given to the same siida; 26 given to a different siida
# proportions:
gifts.same / (gifts.same + gifts.other)   # 63.4% of gifts donated to same siida
gifts.other / (gifts.same + gifts.other)  # 36.6% of gifts donated to a different siida
# do a proportion test 
prop.test(gifts.same, (gifts.same + gifts.other), 0.5)
# significantly different from 1:1 : higher proportion of gifts given to the same siida
# but proportion 0.5 only just not in 95% CI (0.51 - 0.74)

# how many gifts of each size given by how many people?
ddply(gifts, .(Amount), summarise, count=length(Amount))
counts_per_herder = ddply(gifts, .(Ego), summarise, count=length(Ego))
ddply(counts_per_herder, .(count), summarise, freq=length(count))

# in-degree summaries
range(deg.in)
mean(deg.in)
median(deg.in)
sd(deg.in)

# what is the correlation between in- and out-degrees for each person?
cor.test(deg.in, deg.out)

# sex difference in number of gifts received?
wilcox.test(deg.in ~ Sex, data=herders, conf.int=T)

# sex distribution of recipients
ddply(subset(herders, deg.in>0), .(Sex), summarise, n=length(Sex))


#######################################################################################
## Relatedness in the District
##
source("siida relatedness.r")

# Calc average relatedness in district
(district.mean <- mean(kin.mat.r[row(kin.mat.r) != col(kin.mat.r)]))  # mean of the off-diagonal values
# Grand mean of siida relatedness
(grand.mean <- mean(avg.r$mean))


#######################################################################################
## Analysis of gift giving
##
# Correlations between adjacency matrices
# see the SI code for QAP correlations
source("data-funcs/create adjacency matrices.r")
cor.test(adj.r, adj.siida)
cor.test(adj.r, adj.gift)

# GEE models
source("GEE/GEE models.r")
summary(gee.r_siida.z)  # get coefficients from best model

# are gifts given to younger kin? (the numbers below come from table 4)
chisq.test( matrix(c(16, 19, 14, 13), nrow=2, byrow=F) )

# does age correlate with number of gifts received?
cor.test(~ deg.in + Age, data=herders, method="spearman")

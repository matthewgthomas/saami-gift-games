##
## Analysis code for Thomas et al. (2015): Saami reindeer herders cooperate with social group members and genetic kin
##
## Generalised estimating equations for tables 3 and S4
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
library(geepack)
library(MuMIn)
library(arm)  # for standardize()

source("gee/standardize-gee.r")

# re-order data to sort by ego rather than dyad id
herders.wide.gee = herders.wide.subset[ order(herders.wide.subset$Ego), ]
row.names(herders.wide.gee) = NULL  # reset row names

# keep complete cases only
herders.wide.gee = subset(herders.wide.gee, select=c(Ego, Alter, GiftGiven, r, SameSiida, Ego.num.reindeer.z, Alter.num.reindeer.z))
herders.wide.gee = na.omit(herders.wide.gee)


##########################################################
## Fit model with relatedness and siida membership, plot predicted gift probabilities
##
gee.r = geeglm(GiftGiven ~ r, id=Ego, family="binomial", data=herders.wide.gee, corstr="exchangeable", scale.fix=T)
gee.r.z = standardize.gee(gee.r, standardize.y=F)

gee.siida = geeglm(GiftGiven ~ SameSiida, id=Ego, family="binomial", data=herders.wide.gee, corstr="exchangeable", scale.fix=T)
gee.siida.z = standardize.gee(gee.siida, standardize.y=F)

gee.r_siida = geeglm(GiftGiven ~ r + SameSiida, id=Ego, family="binomial", data=herders.wide.gee, corstr="exchangeable", scale.fix=T)
gee.r_siida.z = standardize.gee(gee.r_siida, standardize.y=F)

gee.r_siida_int = geeglm(GiftGiven ~ r * SameSiida, id=Ego, family="binomial", data=herders.wide.gee, corstr="exchangeable", scale.fix=T)
gee.r_siida_int.z = standardize.gee(gee.r_siida_int, standardize.y=F)

gee.r_siida_herd = geeglm(GiftGiven ~ r + SameSiida + Ego.num.reindeer.z + Alter.num.reindeer.z, id=Ego, family="binomial", data=herders.wide.gee, corstr="exchangeable", scale.fix=T)
gee.r_siida_herd.z = standardize.gee(gee.r_siida_herd, standardize.y=F)

gee.full = geeglm(GiftGiven ~ r * SameSiida + Ego.num.reindeer.z + Alter.num.reindeer.z, id=Ego, family="binomial", data=herders.wide.gee, corstr="exchangeable", scale.fix=T)
gee.full.z = standardize.gee(gee.full, standardize.y=F)

# rank models
model.sel(gee.r, gee.siida, gee.r_siida, gee.r_siida_int, gee.r_siida_herd, gee.full, rank=QIC)

rm(herders.wide.gee)

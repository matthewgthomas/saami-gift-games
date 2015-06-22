##
## Analysis code for Thomas et al. (2015): Saami reindeer herders cooperate with social group members and genetic kin
##
## Create dyadic dataframe for herders and gifts. Run from within 'data-funcs/import data.r'
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
library(data.table)
require(Matrix)

covars = c("Age", "Sex", "SiidaName", "deg.in", "leaderYN", "num.children", "s6.education_level", "s5.marriage", "num.reindeer.z", "Interviewed")


#################################
## Convert data frames to data tables
##
herders = as.data.table(subset(herders, select=c("id", covars)))
setkey(herders, id)

# convert gifts
gifts = as.data.table(gifts)
setkey(gifts, Ego, Alter)

# put known relatives in a data table
herders.r = as.data.table(as.table(kin.mat.r))
setnames(herders.r, c("Ego", "Alter", "r"))
# remove ego==alter relationships
herders.r = herders.r[Ego!=Alter]
# convert ego and alter columns to integers (they're characters by default)
herders.r$Ego = as.integer(herders.r$Ego)
herders.r$Alter = as.integer(herders.r$Alter)
setkeyv(herders.r, c("Ego", "Alter"))

# recode marital status to be never, married/cohabiting, other (separated/divorced/widowed)
library(car)
herders$s5.marriage = recode(herders$s5.marriage, "c('Married', 'Cohabit')='Married/Cohabit'")
herders$s5.marriage = recode(herders$s5.marriage, "c('Sep.Div', 'Widowed')='Other'")


#####################################
## Make main dyadic data frame in wide format
##
herders.wide = data.table(expand.grid(Ego=herders$id, Alter=herders$id))
setkeyv(herders.wide, c("Ego", "Alter"))
# remove dyads where ego==alter
herders.wide = herders.wide[Ego!=Alter]


#####################################
## Set dyad IDs for each ego/alter pair
##
# set dyad ID to be the smallest of ego/alter followed by the largest
herders.wide[, DyadID := ifelse(Ego < Alter, paste(Ego, Alter, sep=""), paste(Alter, Ego, sep=""))]
herders.wide$DyadID = as.integer(herders.wide$DyadID)  # paste() makes it character; convert to number


##################################
## merge in gifts
##
herders.wide = gifts[herders.wide]
setkeyv(herders.wide, c("Ego", "Alter"))  # need to reset keys after merge

# remove NAs
herders.wide[is.na(Amount), Amount := 0]
# create binary response variable for gifts
herders.wide[, GiftGiven := 0]
herders.wide[Amount > 0, GiftGiven := 1]


##################################
## merge in relatedness
##
# merge coefficients of relatedness
herders.wide = herders.r[herders.wide]
# set NAs to zero
herders.wide[is.na(r), r := 0]


##################################
## merge in covariates for ego and alter
##
setkey(herders.wide, Ego)
herders.wide = herders[herders.wide]
# repeat for alter
setkey(herders.wide, Alter)
herders.wide = herders[herders.wide]

# sort out column names and keys
setnames(herders.wide, "id", "Alter")
setnames(herders.wide, "i.id", "Ego")

setnames(herders.wide, covars, paste("Alter", covars, sep="."))
setnames(herders.wide, paste("i", covars, sep="."), paste("Ego", covars, sep="."))

setkey(herders.wide, Ego, Alter)

# calculate within-dyad covariates
## same siida?
herders.wide[, SameSiida := 0]
herders.wide[Ego.SiidaName==Alter.SiidaName, SameSiida := 1]  # belong to same siida?
## related?
herders.wide[, IsRelated := 0]
herders.wide[r > 0, IsRelated := 1]  # related yes/no?
## closely related?
herders.wide[, IsRelatedClosely := 0]
herders.wide[r >= 0.5, IsRelatedClosely := 1]  # closely related yes/no?

## age difference
herders.wide[, AgeDiff := Alter.Age - Ego.Age]  # -ve if giving to younger person
## differences in herd sizes
herders.wide[, ReindeerNumDiff := Alter.num.reindeer.z - Ego.num.reindeer.z]

# reorder the columns into something more sensible
setcolorder(herders.wide, c("DyadID", "Ego", paste("Ego", covars, sep="."),
                            "Alter", paste("Alter", covars, sep="."),
                            "r", "IsRelated", "IsRelatedClosely", "Amount", "GiftGiven", "SameSiida",
                            "AgeDiff", "ReindeerNumDiff"))
setkey(herders.wide, DyadID)
#names(herders.wide)


#################################
## Save output
##
write.csv(herders.wide, "./data/herders-gifts-wide.csv", row.names=F)

# clean up workspace
rm(covars, herders.r)

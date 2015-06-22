##
## Analysis code for Thomas et al. (2015): Saami reindeer herders cooperate with social group members and genetic kin
##
## The code in this file calculates average relatedness within each siida.
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
# get list of siidas
siidas <- levels(herders$SiidaName)

# make dataframe to store average relatednesses
avg.r <- data.frame(siida=rep("", length(siidas)),
                    mean=rep(0, length(siidas)), median=rep(0, length(siidas)), 
                    sd=rep(0, length(siidas)), se=rep(0, length(siidas)), N=rep(0, length(siidas)),
                    stringsAsFactors=F)
siidas.r <- data.frame(siida=as.character(), r=as.numeric(), stringsAsFactors=F)

i <- 1
j <- 1

# loop over all siidas
for (siida in siidas)
{
  #DEBUG
  #siida <- siidas[2]
  
  # get list of IDs in that siida
  siidaIDs <- subset(herders, SiidaName==siida, select=id)
  
  # find out how everyone in SiidaIDs is related to each other
  giver <- as.character(siidaIDs$id) # convert the IDs to characters so can be used in matrix lookup
  receiver <- as.character(siidaIDs$id) # convert the IDs to characters so can be used in matrix lookup
  
  # get relatedness coefficients
  kin.tmp <- kin.mat.r[giver, receiver]
  
  # loop over kin.tmp and put into siidas dataframe
  for (row in 1:nrow(kin.tmp)) {
    for (col in row:ncol(kin.tmp)) {
      if (row != col) {
        siidas.r[j, ] <- c(siida, kin.tmp[row, col])
        j <- j + 1
      }
    }
  }
  
  # calc mean and sd (off-diagonals)
  tmp.mean   <- mean(kin.tmp[ row(kin.tmp) != col(kin.tmp) ])
  tmp.median <- median(kin.tmp[ row(kin.tmp) != col(kin.tmp) ])
  tmp.sd     <- sd(kin.tmp[ row(kin.tmp) != col(kin.tmp) ])
  tmp.n      <- nrow(kin.tmp)
  tmp.se     <- tmp.sd / sqrt(tmp.n)
  
  # put mean and sd into table of relatednesses
  avg.r[i, ] <- c(siida, tmp.mean, tmp.median, tmp.sd, tmp.se, tmp.n)
  
  i <- i + 1  # increment counter
}

rm(siidas, siidaIDs, siida, giver, receiver, kin.tmp, tmp.mean, tmp.median, tmp.sd, tmp.se, tmp.n, i)


########################################################################
## A little bit of data format conversion
##
avg.r$mean <- as.numeric(avg.r$mean)
avg.r$sd   <- as.numeric(avg.r$sd)
avg.r$se   <- as.numeric(avg.r$se)
avg.r$N    <- as.integer(avg.r$N)

# order by siida size
avg.r <- avg.r[order(avg.r$N),] 
#levels(avg.r$siida) <- avg.r$siida
avg.r$siida <- factor(avg.r$siida, levels=avg.r$siida)

siidas.r$r <- as.numeric(siidas.r$r)
siidas.r$siida <- as.factor(siidas.r$siida)

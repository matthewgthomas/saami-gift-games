##
## Analysis code for Thomas et al. (2015): Saami reindeer herders cooperate with social group members and genetic kin
##
## The code in this file produces figure 3.
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
library(ggplot2)
source("data-funcs/import data.r")
source("siida relatedness.r")

# Calc average relatedness in district
district.mean <- mean(kin.mat.r[row(kin.mat.r) != col(kin.mat.r)])  # mean of the off-diagonal values
# Grand mean of relatedness across siidas
grand.mean <- mean(avg.r$mean)

(fig3 = ggplot(avg.r, aes(x=siida, y=mean)) +
  geom_hline(yintercept=district.mean, colour="gray40", linetype=2) +  # plot district mean
  geom_hline(yintercept=grand.mean,    colour="firebrick2", linetype=2) +  # plot grand mean
  
  geom_pointrange(aes(ymax = mean+se, ymin=mean-se)) +  # standard errors
  geom_point(size=6, shape=21, fill="black") +
  geom_text(aes(label=N), size=3, colour="white") +
  
  xlab("\nSiida ID") +
  ylab("Coefficient of relatedness") +
  scale_size_continuous(name="Group size") +
  
  theme_bw() +
  #eliminates baground, gridlines, and chart border
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
    ,axis.text=element_text(size=10)
    ,axis.title=element_text(size=10)
  ) +
  
  # hide legend
  theme(
    legend.justification=c(1,1)
    ,legend.position=c(1,1)
    ,legend.text = element_text(size = 10)
    ,legend.title = element_text(size = 10)
    #,legend.position="none"
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))
)

# save publication-quality image
ggsave(filename="fig3.eps", width=87, height=85, units="mm")

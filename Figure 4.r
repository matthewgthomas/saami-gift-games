##
## Analysis code for Thomas et al. (2015): Saami reindeer herders cooperate with social group members and genetic kin
##
## The code in this file produces figure 4.
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
require(ggplot2)
source("data-funcs/import data.r")
source("multiplot.r")

# plot age differences between gift givers and receivers
age_diffs <- subset(herders.wide, GiftGiven==1, select=c(AgeDiff))
age_diffs_kin <- subset(herders.wide, GiftGiven==1 & r>0, select=c(AgeDiff))
age_diffs_nonkin <- subset(herders.wide, GiftGiven==1 & r==0, select=c(AgeDiff))

plot_age_diffs <- function(age_diffs, title="")
{
  fig <- ggplot(age_diffs, aes(x=AgeDiff)) + 
    geom_histogram(data=subset(age_diffs, AgeDiff < 0, select=c(AgeDiff)), fill="#2F4C63", binwidth=5) +
    geom_histogram(data=subset(age_diffs, AgeDiff > 0, select=c(AgeDiff)), fill="#9A7543", binwidth=5) +
    #geom_histogram(data=subset(age_diffs, AgeDiff == 0, select=c(AgeDiff)), fill="green", alpha=0.2, binwidth=5) +
    # (there are no gifts given to people of the same age)
    
    scale_x_continuous("Age difference (years)") +
    scale_y_continuous("Frequency\n", expand=c(0,0)) +
    
    ggtitle(title) +
    
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
    theme(legend.position="none") +
    
    #draws x and y axis line
    theme(axis.line = element_line(color = 'black'))
  
  return(fig)
}


plots = list()
plots[[1]] <- plot_age_diffs(age_diffs_kin, title="A")
plots[[2]] <- plot_age_diffs(age_diffs_nonkin, title="B")

#pdf("fig4.pdf", height=2, width=7)
postscript("fig4.eps", height=2, width=7)
multiplot(plotlist=plots, cols=2, layout=matrix(c(1,2), nrow=1, byrow=TRUE))
dev.off()

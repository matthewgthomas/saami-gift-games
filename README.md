# Analysis code for Thomas et al. (2015): "Saami reindeer herders cooperate with social group members and genetic kin" [![license GNU](http://b.repl.ca/v1/license-GNU-brightgreen.png)][license]

[license]: https://github.com/matthewgthomas/saami-gift-games/blob/master/LICENSE

This repository contains the analysis code, written in R, for our paper "Saami reindeer herders cooperate with social group members and genetic kin" (published in Behavioral Ecology). The R code is released under a GNU General Public License.
Before running, [download the data files](http://dryad.com) and unzip them into a 'data' subfolder.

## Main files

+ `Main text.r` -- this reproduces the analyses in the main text's results section
+ `Figure 3.r` and `Figure 4.r` -- reproduces figures 3 and 4 respectively
+ `Tables 1 to 4.r` -- reproduces tables 1-4 in main text
+ `SI.r` -- reproduces the analyses in the supplementary information

## Auxiliary files
These files are called from within the main files above so you don't need to worry too much about them.

+ `data-funcs/import data.r` -- imports the data files and creates a social network of the gifts
+ `data-funcs/create dyadic data.r` -- creates a dataframe for every ego-alter pair in the sample
+ `data-funcs/create adjacency matrices.r` -- turns the gift network into adjacency matrices for gift giving, siida membership and relatedness. These are used for correlations in the main text and table S3
+ `multiplot.r` -- function to plot more than one ggplot in a single output (used for figure 4) (source: [Cookbook for R](http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)))
+ `siida relatedness.r` -- calculates within-siida relatedness as well as grand mean and district mean relatedness (for figure 3 and main text)
+ `GEE/GEE models.r` -- generalised estimating equations for tables 3 and S4
+ `GEE/standardize-gee.r` -- modifies the `standardize` function in the package `arm` to work with GEE models

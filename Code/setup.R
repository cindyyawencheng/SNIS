
if(Sys.info()['user'] == "cindycheng"){
pathData = '/Users/cindycheng/Dropbox/Documents/SNIS/Data/'
pathGraphics = '/Users/cindycheng/Dropbox/Documents/SNIS/Graphics/'
}

if (Sys.info()['user'] == 'tranganhdo'){
	pathData = '/Users/tranganhdo/Dropbox/SNIS/Data'
  pathGraphics = '/Users/tranganhdo/Dropbox/SNIS/Graphics'
}


ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

 
packages = c('gtable', 
             'grid', 
             'countrycode', 
             'dplyr', 
             'magrittr', 
             'tm', 
             'lme4', 
             'reshape', 
             'plm', 
             'stringr', 
             'RWeka', 
             'qdap', 
             "RColorBrewer", 
             "ggplot2", 
             "wordcloud", 
             "biclust",
             "cluster", 
             "igraph", 
             "fpc", 
             "qdap",
             'stringi',
             'data.table')
ipak(packages)


setwd(pathData)
load('panel.rda')
panelExt = data.frame(expand.grid(panel$ccode[which(panel$year==2012)], 2013:2015))
names(panelExt) = c('ccode', 'year')
panelExt$cname = panel$cname[match(panelExt$ccode, panel$ccode)]
panelExt$CNTRY_NAME = panel$CNTRY_NAME[match(panelExt$ccode, panel$ccode)]
panelExt = panelExt[, c( 'CNTRY_NAME', 'year', 'cname',   'ccode')]
panel2 = rbind(panel[, c( 'CNTRY_NAME', 'year', 'cname',   'ccode')], panelExt)

#####################################

#####################################
 
cname = function(x){
	x = as.character(x)
	toupper(countrycode(x, 'country.name', 'country.name'))}

#####################################

#####################################
# returns the position number of the positive numbers in a vector

returnPosPosition = function(x){

	if (x[1]<0 & length(x) > 1){
	return(which(x>0))}

	else if( x[1] == 0 & length(x) == 1){
			return(which(x == 0))
		}
	else if (x[1]>=0 ){
		 return(which(x>0))
	}

}

#####################################

#####################################
# allows you to extract multiple words/ngrams, i.e. 'Big Apple', instead of only single ones/unigrams
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 5))


VerbgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 10))


#####################################

#####################################
# function which allos you to do gsub on corpus 

toSpace <- content_transformer(function(x, pattern, sub) gsub(pattern,  sub, x)) # http://stackoverflow.com/questions/14281282/how-to-write-custom-removepunctuation-function-to-better-deal-with-unicode-cha


#####################################

#####################################
# replicate ggplot color palette
# http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

#####################################

#####################################
## allows you to order multiple legends
# http://stackoverflow.com/questions/10035551/ordering-of-multiple-legends-guides-what-is-the-automatic-logic-how-to-change


guides_merge <- function(gdefs) {
  gdefs <- lapply(gdefs, function(g) { g$hash <- paste(g$order, g$hash, sep = "z"); g})
  tapply(gdefs, sapply(gdefs, function(g)g$hash), function(gs)Reduce(guide_merge, gs))
}
environment(guides_merge) <- environment(ggplot)
assignInNamespace("guides_merge", guides_merge, pos = "package:ggplot2")


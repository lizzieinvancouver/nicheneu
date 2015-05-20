### Started 20 May 2015 ###
### By Lizzie ###

## Getting first, last and peak flowering from Gothic data ##
## for the niche-neutral project ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Helper functions, first one from master Regetz

subsetEarliest <- function(dat, by, datevar="date") {
    groups <- do.call("paste", dat[by])
    do.call("rbind", lapply(split(dat, groups),
        function(x) x[which.min(x[[datevar]]),]))
}

subsetLatest <- function(dat, by, datevar="date") {
    groups <- do.call("paste", dat[by])
    do.call("rbind", lapply(split(dat, groups),
        function(x) x[which.max(x[[datevar]]),]))
}

subsetPeak <- function(dat, by, flowervar="value") {
    groups <- do.call("paste", dat[by])
    do.call("rbind", lapply(split(dat, groups),
        function(x) x[which.max(x[[flowervar]]),]))
}

setwd("~/Documents/git/projects/nicheneu")

##
## data, fully cleaned (er, as best we know!)
##

# Two types, the one Jim and I cleaned, and ...
# the one received in 2010 (phenodata_minus.txt), more on that below
gothclean <- read.csv("analyses/input/gothicclean.csv", header=TRUE)
gothclean$date <- as.Date(gothclean$date, format="%Y-%m-%d")
gothclean$year <- format(gothclean$date, "%Y")

##
## get total 'flower' number
##
# this is not exact, if someone wrote 'first few' we miss it but it gets actual numbers
gothclean$capitula <- as.numeric(as.character(gothclean$capitula))
gothclean$clusters <- as.numeric(as.character(gothclean$clusters))
gothclean$flowers <- as.numeric(as.character(gothclean$flowers))
gothclean$inflorescences <- as.numeric(as.character(gothclean$inflorescences))
gothclean$other <- as.numeric(as.character(gothclean$other))
# not flowers
gothclean$plants <- as.numeric(as.character(gothclean$plants))
# make NAs 0
gothclean$capitula[is.na(gothclean$capitula)] <- 0
gothclean$clusters[is.na(gothclean$clusters)] <- 0
gothclean$flowers[is.na(gothclean$flowers)] <- 0
gothclean$inflorescences[is.na(gothclean$inflorescences)] <- 0
gothclean$other[is.na(gothclean$other)] <- 0
gothclean$plants[is.na(gothclean$plants)] <- 0
# need to change someday to check that these categories never overlap...
# for now, just add them up
gothclean$totalflowers <- gothclean$capitula + gothclean$clusters +
    gothclean$flowers + gothclean$inflorescences + gothclean$other

##
## get peak and first flower by plot and 'site' which is plot without number
## such as 'em', `wm' etc.
##

# get rid of zeros (otherwise you end up with a lot of FFD where totalflowers is zero)
gothclean.nozero <- subset(gothclean, totalflowers>0)

gothpfd <- subsetPeak(gothclean.nozero, c("species","plotNums", "year"),
    flowervar="totalflowers")
gothpfd.bysite <- subsetPeak(gothclean.nozero, c("species","plots", "year"),
    flowervar="totalflowers")
gothffd <- subsetEarliest(gothclean.nozero, c("species","plotNums", "year"))
gothffd.bysite <- subsetEarliest(gothclean.nozero, c("species","plots", "year"))
gothlfd <- subsetLatest(gothclean.nozero, c("species","plotNums", "year"))
gothlfd.bysite <- subsetLatest(gothclean.nozero, c("species","plots", "year"))

gothpfd$event <-  "pfd"
gothpfd.bysite$event <- "pfd"
gothffd$event <-  "ffd"
gothffd.bysite$event <- "ffd"
gothlfd$event <-  "lfd"
gothlfd.bysite$event <- "lfd"

# reduce columns
selectcols <- c("species", "date", "year", "plotNums", "event")
selectcols.bysite <- c("species", "date", "year", "plots", "event", "totalflowers")

gothpfd.sm <- subset(gothpfd, select=selectcols)
gothffd.sm <- subset(gothffd, select=selectcols)
gothlfd.sm <- subset(gothlfd, select=selectcols)

gothpfd.bysite.sm <- subset(gothpfd.bysite, select=selectcols.bysite)
gothffd.bysite.sm <- subset(gothffd.bysite, select=selectcols.bysite)
gothlfd.bysite.sm <- subset(gothlfd.bysite, select=selectcols.bysite)

# merge it altogether!
gothbyplot <- rbind(gothpfd.sm, gothffd.sm, gothlfd.sm)
gothbysite <- rbind(gothpfd.bysite.sm, gothffd.bysite.sm, gothlfd.bysite.sm)

gothbyplot$doy <- format(gothbyplot$date, "%j")
gothbysite$doy <- format(gothbysite$date, "%j")

write.csv(gothbyplot, "analyses/output/gothcleanbyplot.csv", row.names=FALSE)
write.csv(gothbysite, "analyses/output/gothcleanbysite.csv", row.names=FALSE)


####################
## the other data ##
####################

# see mergegothicsubplots.R and mergegothicpeakflowering.R for creation of
# the 2009 data by plot and by plot with peak flowering
# in Subversion/phenology/notposting/merge extras
gothicsubplot <- read.csv("analyses/input/gothicsubplot.csv", header=TRUE)
gothicpeakplot <- read.csv("analyses/input/gothicpeakflowplots.csv",
    header=TRUE)

gothbyplot.old <- subset(gothicsubplot, select=c("plot", "event", "year",
    "doy", "date", "genus", "species"))

gothpeakbyplot.old <- subset(gothicpeakplot, select=c("plot", "event", 
    "year", "doy", "date", "genus", "species", "value"))

# right, I did almost nothing but reduce the columns
write.csv(gothpeakbyplot.old, "analyses/output/gothicpdf.olddata.csv", row.names=FALSE)
write.csv(gothbyplot.old, "analyses/output/gothicffd.olddata.csv", row.names=FALSE)

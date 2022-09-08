## ----echo=FALSE---------------------------------------------------------------
isRNRFA <- requireNamespace("rnrfa", quietly = TRUE)
isZOO <- requireNamespace("zoo", quietly = TRUE)
isHTTR <- requireNamespace("httr", quietly = TRUE)
isUTF8 <- requireNamespace("utf8", quietly = TRUE)
is1 <- (isHTTR & isUTF8)
is2 <- (is1 & isRNRFA & isZOO)

## ----echo=FALSE,eval=isUTF8---------------------------------------------------
library(utf8)

## ----echo=FALSE,eval=TRUE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
#'arg' should be one of “default”, “cerulean”, “journal”, “flatly”, “darkly”, “readable”, “spacelab”, “united”, “cosmo”, “lumen”, “paper”, “sandstone”, “simplex”, “yeti”

## ----setup--------------------------------------------------------------------
library(winfapReader)
### the get_* functions only works once you are connected to the internet
### they also need one to have the library httr installed 
### verify if you have the library with (!requireNamespace("httr", quietly = TRUE)) 
### if FALSE install it with 
### install.packages("httr")

## ----showAmax, eval=is1-------------------------------------------------------
if(curl::has_internet()) amaxEx <- get_amax(c(42003,72014)) 
names(amaxEx); class(amaxEx)
# let's look at only one of these
a42003 <- amaxEx[["42003"]]
## what is the output
head(a42003)

## ----showPOT, eval=is1--------------------------------------------------------
if(curl::has_internet()) potEx <- get_pot(c(42003,72014)) 
names(potEx); class(potEx)
# let's look at only one of these
p42003 <- potEx[["42003"]]
## what is the output
class(p42003); names(p42003)

## ----showtablePOT, eval=is1---------------------------------------------------
head(p42003$tablePOT)
## notice: several events in the 1982 no events in 1983

## ----showWaterYearInfo, eval=is1----------------------------------------------
head(p42003$WaterYearInfo)

## ----showdateRange, eval=is1--------------------------------------------------
(p42003$dateRange)

## ----showWaterYearInfowithAmax, eval=is1--------------------------------------
p42003withAmax <- get_pot(42003, getAmax = TRUE)
head(p42003withAmax$WaterYearInfo, 10)

## ----showCD, eval=is1---------------------------------------------------------
if(curl::has_internet()) cdEx <- get_cd(c(42003,72014)) 
names(cdEx); class(cdEx)
# let's look at only one of these
c42003 <- cdEx[["42003"]]
## what is the output
class(c42003); names(c42003)

## ----showCDall, eval=is1------------------------------------------------------
if(curl::has_internet()) cd42003all <- get_cd(42003, fields = "all") 
names(cd42003all)

## ---- eval=is2----------------------------------------------------------------
## Lancaster coordinates:  54.04, -2.8
## let's look around the city
rivLanc <- rnrfa::catalogue(bbox = list(lat_min = 54.04-0.2, lat_max = 54.04+0.2, 
                                        lon_min = -2.8-0.2, lon_max = -2.8+0.2))
### let's select stations which have been deemed to be suitable for pooling
### that's the highest quality flag for annual maxima 
table(rivLanc[,"feh-pooling"]) ### 5 stations are suitable for pooling
rivLanc <- subset(rivLanc,subset = as.vector(rivLanc[,"feh-pooling",drop=TRUE]))
rivLanc[,1:3]
### notice that rnrfa outputs a tibble and not a data.frame
idLanc <- rivLanc[,"id",drop=TRUE] ## a vector of ids
amaxLanc <- winfapReader::get_amax(idLanc)
names(amaxLanc)

## ---- echo=TRUE, eval=is2-----------------------------------------------------
par(mfrow=c(2,3))
invisible(
  sapply(amaxLanc, 
       function(x) with(x,plot(WaterYear,Flow,
                               type="h",col=ifelse(Rejected,2,4), 
                               main = unique(Station)))))

## ---- eval=is2----------------------------------------------------------------
par(mfrow=c(1,1))
### the annual maxima for 72014 from rnrfa
maxflow72014 <- rnrfa::get_ts(72014, type = "amax-flow", full_info = TRUE) 
### the annual maxima for 72014 from winfapReader
xx <- amaxLanc[["72014"]][,c("Date","Flow","Rejected")]
plot(xx[,"Flow"], maxflow72014[,"amax-flow"]); abline(0,1) ### same information 
which(xx$Rejected) ## but two years should be rejected
which(maxflow72014$rejected == 1) ## same two years

## ---- eval=is2----------------------------------------------------------------
par(mfrow=c(1,1))
# the pot records for 75001 from rnrfa
pot75001 <- rnrfa::get_ts(75001, type = "pot-flow", full_info = TRUE)
pot75001[9:12,]
# using winfapReader
p75001 <- get_pot(75001)
p75001$tablePOT[9:12,]
# the same peaks are identified
p75001$WaterYearInfo[1:5,] ### but notice that 1975 had a low proportion missing records
#  the lack of data in 1975 is due to all flow being low

## ----dayAndAmax, eval=is2-----------------------------------------------------
### get daily data from NRFA 
daily72014 <- rnrfa::get_ts(72014, type = "gdf") 
## make daily data into data.frame 
daily72014 <- data.frame(Day = zoo::index(daily72014), 
                         DFlow = as.vector(daily72014))
plot(xx[,c("Date","Flow")], col = ifelse(xx$Rejected, 2, 4),
     pch = 4, ylim =c(0,1.05*max(xx$Flow)))
title(main = "The Conder at Galgate")
points(daily72014, type="l")


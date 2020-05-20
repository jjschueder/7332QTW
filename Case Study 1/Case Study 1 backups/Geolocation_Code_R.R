
options(digits = 2)

# getwd()

setwd("C:/Users/kevinm/Documents/SMU/MSDS7333_QuantifyingTheWorld/Week1")

#----------------------------------------------------------------
# Reading and understanding the data
#----------------------------------------------------------------

txt = readLines("offline.final.trace.txt")

# Rows in the dataset that have comments starting with "#"
sum(substr(txt, 1, 1) == "#")

# Total number of rows in the dataset including comment rows
length(txt)

strsplit(txt[4], ";")[[1]]

#----------------------------------------------------------------
# Processing the Raw Data
#----------------------------------------------------------------

# Substring the row at a ';', '=' or ',' character
tokens = strsplit(txt[4], "[;=,]")[[1]]

tokens[1:10]

# We can extract the values of these variables with
tokens[c(2, 4, 6:8, 10)]

tokens[ - ( 1:10 ) ]

tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)
mat = cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow = nrow(tmp),
                   ncol = 6, byrow = TRUE), 
            tmp)

dim(mat)

# Create a function named processLine
processLine =
              function(x)
              {
                tokens = strsplit(x, "[;=,]")[[1]]
                tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)
                cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow = nrow(tmp),
                             ncol = 6, byrow = TRUE), tmp)
              }

# Apply the processLine function to rows 4 through 20
tmp = lapply(txt[4:20], processLine)

sapply(tmp, nrow)

offline = as.data.frame(do.call("rbind", tmp))
dim(offline)

# exclude rows starting with "#"
lines = txt[ substr(txt, 1, 1) != "#" ]

# Apply the function processLine to each row/line
tmp = lapply(lines, processLine)

processLine = function(x)
{
  tokens = strsplit(x, "[;=,]")[[1]]

# Exclude rows that have only 10 fields exactly...return as NULL
  if (length(tokens) == 10) 
    return(NULL)
  
  tmp = matrix(tokens[ - (1:10) ], , 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6, 
               byrow = TRUE), tmp)
}

options(error = recover, warn = 1)
tmp = lapply(lines, processLine)
offline = as.data.frame(do.call("rbind", tmp), 
                        stringsAsFactors = FALSE)

dim(offline)

#----------------------------------------------------------------
# Cleaning the Data and Building a Representation for Analysis
#----------------------------------------------------------------

# Rename the fields with meaningful names
names(offline) = c("time", "scanMac", "posX", "posY", "posZ", 
                   "orientation", "mac", "signal", 
                   "channel", "type")

# we convert the position, signal, and time variables to numeric
numVars = c("time", "posX", "posY", "posZ", 
            "orientation", "signal")
offline[ numVars ] =  lapply(offline[ numVars ], as.numeric)

# drop all records for adhoc measurements and remove the type variable from our data frame
offline = offline[ offline$type == "3", ]
offline = offline[ , "type" != names(offline) ]
dim(offline)

#-------------------------------------------------------------------------------------------
'Next we consider the time variable. According to the documentation, time is measured
in the number of milliseconds from midnight on January 1st, 1970. This is the origin used
for the POSIXt format, but with POSIXt, it is the number of seconds, not milliseconds. We
can scale the value of time to seconds and then simply set the class of the time element in
order to have the values appear and operate as date-times in R. We keep the more precise
time in rawTime just in case we need it. We perform the conversion as follows:'
#-------------------------------------------------------------------------------------------
offline$rawTime = offline$time
offline$time = offline$time/1000
class(offline$time) = c("POSIXt", "POSIXct")

# Now that we have completed these conversions, we check the types of the variables in the data frame
# and verify that they are what we want:
unlist(lapply(offline, class))

'We have the correct shape for the data and even the correct types. We next verify that
the actual values of the data look reasonable. There are many approaches we can take to
do this. We start by looking at a summary of each numeric variable with'

summary(offline[, numVars])

# We also convert the character variables to factors and examine them with summary
summary(sapply(offline[ , c("mac", "channel", "scanMac")],
               as.factor))

'There is only one value for scanMac, the MAC address for the hand-held device from
which the measurements were taken. We might as well discard this variable from our
data frame. However, we may want to note this value to compare it with the online
data.
. All of the values for posZ, the elevation of the hand-held device, are 0. This is because
all of the measurements were taken on one floor of the building. We can eliminate this
variable also. We modify our data frame accordingly,'

offline = offline[ , !(names(offline) %in% c("scanMac", "posZ"))]

#----------------------------------------------------------------
# Exploring Orientation
#----------------------------------------------------------------

length(unique(offline$orientation))

# Clearly, this is not the case. Let's examine the distribution of orientation:

plot(ecdf(offline$orientation))

pdf(file = "Geo_ECDFOrientation.pdf", width = 10, height = 7)
oldPar = par(mar = c(4, 4, 1, 1))
plot(ecdf(offline$orientation), pch = 19, cex = 0.3,
     xlim = c(-5, 365), axes = FALSE,
     xlab = "orientation", ylab = "Empirical CDF", main = "")
box()
axis(2)
axis(side = 1, at = seq(0, 360, by = 45))
par(oldPar)
dev.off()

pdf(file = "Geo_DensityOrientation.pdf", width = 10, height = 5)
oldPar = par(mar = c(4, 4, 1, 1))
plot(density(offline$orientation, bw = 2), 
     xlab = "orientation", main = "")
par(oldPar)
dev.off()

'Although the experiment was designed to measure signal strength at 8 orientations -
45 degree intervals from 0 to 315 - these orientations are not exact. However, it may be
useful in our analysis to work with values corresponding to the 8 equi-spaced angles. That
is, we want to map 47.5 to 45, and 358.2 to 0, and so on. To do this, we take each value
and find out to which of the 8 orientations it is closest and we return that orientation. We
must handle values such as 358.2 carefully as we want to map them to 0, not to the closer
315.'

roundOrientation = function(angles) {
  refs = seq(0, by = 45, length  = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}

# We use roundOrientation() to create the rounded angles with

offline$angle = roundOrientation(offline$orientation)

pdf(file = "Geo_BoxplotAngle.pdf", width = 10)
oldPar = par(mar = c(4, 4, 1, 1))

par(oldPar)
dev.off()

#----------------------------------------------------------------
# Exploring MAC Addresses
#----------------------------------------------------------------
'From the summary() information, it seems that there may be a one-to-one mapping between
the MAC address of the access points and channel. For example, the summary statistics
show there are 126,529 occurrences of the address 00:14:bf:3b:c7:c6 and the same
number of occurrences of channel 2432000000. To help us ascertain if we do have a oneto-
one mapping, we look at the relationship between the MAC address and channel.
How many unique addresses and channels do we have? There should be the same number,
if there is a one-to-one mapping.'

c(length(unique(offline$mac)), length(unique(offline$channel)))

'There are 12 MAC addresses and 8 channels.We were given the impression from the building
plan (see Figure 1.1) that there are only 6 access points. Why are there 8 channels and 12
MAC addresses? Rereading the documentation we find that there are additional access
points that are not part of the testing area and so not seen on the floor plan. Let's check
the counts of observations for the various MAC addresses with table():'

table(offline$mac)

'Clearly the first and the last two MAC addresses are not near the testing area or were only
working/active for a short time during the measurement process because their counts are
very low. It's probably also the case that the third and fifth addresses are not among the
access points displayed on the map because they have much lower counts than the others
and these are far lower than the possible 146,080 recordings (recall that there are potentially
signals recorded at 166 grid points, 8 orientations, and 110 replications).
According to the documentation, the access points consist of 5 Linksys/Cisco and one
Lancom L-54g routers. We look up these MAC addresses at the http://coffer.com/
mac_find/ site to find the vendor addresses that begin with 00:14:bf belong to Linksys
devices, those beginning with 00:0f:a3 belong to Alpha Networks, and Lancom devices
start with 00:a0:57 (see Figure 1.4). We do have 5 devices with an address that begins
00:14:bf, which matches with the Linksys count from the documentation. However, none
of our MAC addresses begin with 00:a0:57 so there is a discrepancy with the documentation.
Nonetheless, we have discovered valuable information for piecing together a better
understanding of the data. For now, let's keep the records from the top 7 devices. We do
this with'

subMacs = names(sort(table(offline$mac), decreasing = TRUE))[1:7]
offline = offline[ offline$mac %in% subMacs, ]

'Finally, we create a table of counts for the remaining MAC×channel combinations and
confirm there is one non-zero entry in each row'

macChannel = with(offline, table(mac, channel))
apply(macChannel, 1, function(x) sum(x > 0))

'Indeed we see that there is a one-to-one correspondence between MAC address and channel
for these 7 devices. This means we can eliminate channel from offline, i.e.,'

offline = offline[ , "channel" != names(offline)]

#----------------------------------------------------------------
# Exploring the Position of the Hand-Held Device
#----------------------------------------------------------------
'Lastly, we consider the position variables, posX and posY. For how many different locations
do we have data? The by() function can tally up the numbers of rows in our data frame
for each unique (x, y) combination. We begin by creating a list containing a data frame for
each location as follows:'

locDF = with(offline, 
             by(offline, list(posX, posY), function(x) x))
length(locDF)

'Note that this list is longer than the number of combinations of actual (x, y) locations at
which measurements were recorded. Many of these elements are empty:'

sum(sapply(locDF, is.null))

'The null values correspond to the combinations of the xs and ys that were not observed.
We drop these unneeded elements as follows:'

locDF = locDF[ !sapply(locDF, is.null) ]

# and confirm that we now have only 166 locations with

length(locDF)

'We can operate on each of these data frames to, e.g., determine the number of observations
recorded at each location with'

locCounts = sapply(locDF, nrow)

# And, if we want to keep the position information with the location, we do this with

locCounts = sapply(locDF, 
                   function(df) 
                     c(df[1, c("posX", "posY")], count = nrow(df)))

# We confirm that locCounts is a matrix with 3 rows with

class(locCounts)

dim(locCounts)

# We examine a few of the counts

locCounts[ , 1:8]

'We see that there are roughly 5,500 recordings at each position. This is in accord with 8
orientations × 110 replications × 7 access points, which is 6,160 signal strength measurements.
We can visualize all 166 counts by adding the counts as text at their respective locations,
changing the size and angle of the characters to avoid overlapping text. We first transpose
the matrix so that the locations are columns of the matrix and then we make our plot with'

pdf(file = "Geo_XYByCount.pdf", width = 10)
oldPar = par(mar = c(3.1, 3.1, 1, 1))

locCounts = t(locCounts)
plot(locCounts, type = "n", xlab = "", ylab = "")
text(locCounts, labels = locCounts[,3], cex = .8, srt = 45)

par(oldPar)
dev.off()

#----------------------------------------------------------------
# Creating a Function to Prepare the Data
#----------------------------------------------------------------
'We have examined all the variables except time and signal. This process has helped us
clean our data and reduce it to those records that are relevant to our analysis. We leave the
examination of the signals to the next section where we study its distributional properties.
As for time, while this variable is not directly related to our model, it indicates the order
in which the observations were taken. In an experiment, this can be helpful in uncovering
potential sources of bias. For example, the person carrying the hand-held device may have
changed how the device was carried as the experiment progressed and this change may lead
to a change in the strength of the signal. Plots and analyses of the relationship between time
and other variables can help us uncover such potential problems. We leave this investigation
as an exercise.
Since we also want to read the online data in R, we turn all of these commands into
a function called readData(). Additionally, if we later change our mind as to how we want
to handle some of these special cases, e.g., to keep channel or posZ, then we can make a
simple update to our function and rerun it. We might even add a parameter to the function
definition to allow us to process the data in different ways. We leave it as an exercise to
create readData().
We call readData() to create the offline data frame with'

readData = 
  function(filename = 'offline.final.trace.txt', 
           subMacs = c("00:0f:a3:39:e1:c0", "00:0f:a3:39:dd:cd", "00:14:bf:b1:97:8a",
                       "00:14:bf:3b:c7:c6", "00:14:bf:b1:97:90", "00:14:bf:b1:97:8d",
                       "00:14:bf:b1:97:81"))
  {
    txt = readLines(filename)
    lines = txt[ substr(txt, 1, 1) != "#" ]
    tmp = lapply(lines, processLine)
    offline = as.data.frame(do.call("rbind", tmp), 
                            stringsAsFactors= FALSE) 
    
    names(offline) = c("time", "scanMac", 
                       "posX", "posY", "posZ", "orientation", 
                       "mac", "signal", "channel", "type")
    
    # keep only signals from access points
    offline = offline[ offline$type == "3", ]
    
    # drop scanMac, posZ, channel, and type - no info in them
    dropVars = c("scanMac", "posZ", "channel", "type")
    offline = offline[ , !( names(offline) %in% dropVars ) ]
    
    # drop more unwanted access points
    offline = offline[ offline$mac %in% subMacs, ]
    
    # convert numeric values
    numVars = c("time", "posX", "posY", "orientation", "signal")
    offline[ numVars ] = lapply(offline[ numVars ], as.numeric)
    
    # convert time to POSIX
    offline$rawTime = offline$time
    offline$time = offline$time/1000
    class(offline$time) = c("POSIXt", "POSIXct")
    
    # round orientations to nearest 45
    offline$angle = roundOrientation(offline$orientation)
    
    return(offline)
  }

offlineRedo = readData()

# Then we use the identical() function to check this version of the data frame against the one
# that we already created:

library(codetools)

identical(offline, offlineRedo)

findGlobals(readData, merge = FALSE)$variables


pdf(file = "Geo_BoxplotSignalByMacAngle.pdf", width = 7)
oldPar = par(mar = c(3.1, 3, 1, 1))

library(lattice)
bwplot(signal ~ factor(angle) | mac, data = offline, 
       subset = posX == 2 & posY == 12 
       & mac != "00:0f:a3:39:dd:cd", 
       layout = c(2,3))

par(oldPar)
dev.off()

summary(offline$signal)

pdf(file = "Geo_DensitySignalByMacAngle.pdf", width = 8, height = 12)
oldPar = par(mar = c(3.1, 3, 1, 1))

densityplot( ~ signal | mac + factor(angle), data = offline,
             subset = posX == 24 & posY == 4 & 
               mac != "00:0f:a3:39:dd:cd",
             bw = 0.5, plot.points = FALSE)

par(oldPar)
dev.off()

#offline = offline[ offline$mac != "00:0f:a3:39:dd:cd", ]

offline$posXY = paste(offline$posX, offline$posY, sep = "-")

byLocAngleAP = with(offline, 
                    by(offline, list(posXY, angle, mac), 
                       function(x) x))

signalSummary = 
  lapply(byLocAngleAP,            
         function(oneLoc) {
           ans = oneLoc[1, ]
           ans$medSignal = median(oneLoc$signal)
           ans$avgSignal = mean(oneLoc$signal)
           ans$num = length(oneLoc$signal)
           ans$sdSignal = sd(oneLoc$signal)
           ans$iqrSignal = IQR(oneLoc$signal)
           ans
         })

offlineSummary = do.call("rbind", signalSummary)     

pdf(file = "Geo_BoxplotSignalSDByAvg.pdf", width = 10)
oldPar = par(mar = c(3.1, 3, 1, 1))

breaks = seq(-90, -30, by = 5)
bwplot(sdSignal ~ cut(avgSignal, breaks = breaks),
       data = offlineSummary, 
       subset = mac != "00:0f:a3:39:dd:cd",
       xlab = "Mean Signal", ylab = "SD Signal")

par(oldPar)
dev.off()

pdf(file = "Geo_ScatterMean-Median.pdf", width = 10)
oldPar = par(mar = c(4.1, 4.1, 1, 1))

with(offlineSummary,
     smoothScatter((avgSignal - medSignal) ~ num,
                   xlab = "Number of Observations", 
                   ylab = "mean - median"))
abline(h = 0, col = "#984ea3", lwd = 2)

lo.obj = 
  with(offlineSummary,
       loess(diff ~ num, 
             data = data.frame(diff = (avgSignal - medSignal),
                               num = num)))

lo.obj.pr = predict(lo.obj, newdata = data.frame(num = (70:120)))
lines(x = 70:120, y = lo.obj.pr, col = "#4daf4a", lwd = 2)

par(oldPar)
dev.off()

oneAPAngle = subset(offlineSummary, 
                    mac == subMacs[5] & angle == 0)


library(fields)
smoothSS = Tps(oneAPAngle[, c("posX","posY")], 
               oneAPAngle$avgSignal)

vizSmooth = predictSurface(smoothSS)

plot.surface(vizSmooth, type = "C")

points(oneAPAngle$posX, oneAPAngle$posY, pch=19, cex = 0.5)

surfaceSS = function(data, mac, angle = 45) {
  require(fields)
  oneAPAngle = data[ data$mac == mac & data$angle == angle, ]
  smoothSS = Tps(oneAPAngle[, c("posX","posY")], 
                 oneAPAngle$avgSignal)
  vizSmooth = predictSurface(smoothSS)
  plot.surface(vizSmooth, type = "C", 
               xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  points(oneAPAngle$posX, oneAPAngle$posY, pch=19, cex = 0.5) 
}

parCur = par(mfrow = c(2,2), mar = rep(1, 4))

mapply(surfaceSS, mac = subMacs[ rep(c(5, 1), each = 2) ], 
       angle = rep(c(0, 135), 2),
       data = list(data = offlineSummary))

par(parCur)

offlineSummary = subset(offlineSummary, mac != subMacs[2])

AP = matrix( c( 7.5, 6.3, 2.5, -.8, 12.8, -2.8,  
                1, 14, 33.5, 9.3,  33.5, 2.8),
             ncol = 2, byrow = TRUE,
             dimnames = list(subMacs[ -2 ], c("x", "y") ))

AP

diffs = offlineSummary[ , c("posX", "posY")] - 
  AP[ offlineSummary$mac, ]

offlineSummary$dist = sqrt(diffs[ , 1]^2 + diffs[ , 2]^2)

xyplot(signal ~ dist | factor(mac) + factor(angle), 
       data = offlineSummary, pch = 19, cex = 0.3,
       xlab ="distance")

pdf(file="Geo_ScatterSignalDist.pdf", width = 7, height = 10)
oldPar = par(mar = c(3.1, 3.1, 1, 1))
library(lattice)
xyplot(signal ~ dist | factor(mac) + factor(angle), 
       data = offlineSummary, pch = 19, cex = 0.3,
       xlab ="distance")
par(oldPar)
dev.off()

macs = unique(offlineSummary$mac)
online = readData("Data/online.final.trace.txt", subMacs = macs)

online$posXY = paste(online$posX, online$posY, sep = "-")

length(unique(online$posXY))

tabonlineXYA = table(online$posXY, online$angle)
tabonlineXYA[1:6, ]

keepVars = c("posXY", "posX","posY", "orientation", "angle")
byLoc = with(online, 
             by(online, list(posXY), 
                function(x) {
                  ans = x[1, keepVars]
                  avgSS = tapply(x$signal, x$mac, mean)
                  y = matrix(avgSS, nrow = 1, ncol = 6,
                             dimnames = list(ans$posXY, names(avgSS)))
                  cbind(ans, y)
                }))

onlineSummary = do.call("rbind", byLoc)  

dim(onlineSummary)

names(onlineSummary)
m = 3; angleNewObs = 230
refs = seq(0, by = 45, length  = 8)
nearestAngle = roundOrientation(angleNewObs)

if (m %% 2 == 1) {
  angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
} else {
  m = m + 1
  angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
  if (sign(angleNewObs - nearestAngle) > -1) 
    angles = angles[ -1 ]
  else 
    angles = angles[ -m ]
}
angles = angles + nearestAngle
angles[angles < 0] = angles[ angles < 0 ] + 360
angles[angles > 360] = angles[ angles > 360 ] - 360

offlineSubset = 
  offlineSummary[ offlineSummary$angle %in% angles, ]

reshapeSS = function(data, varSignal = "signal", 
                     keepVars = c("posXY", "posX","posY")) {
  byLocation =
    with(data, by(data, list(posXY), 
                  function(x) {
                    ans = x[1, keepVars]
                    avgSS = tapply(x[ , varSignal ], x$mac, mean)
                    y = matrix(avgSS, nrow = 1, ncol = 6,
                               dimnames = list(ans$posXY,
                                               names(avgSS)))
                    cbind(ans, y)
                  }))
  
  newDataSS = do.call("rbind", byLocation)
  return(newDataSS)
}

# set training set

trainSS = reshapeSS(offlineSubset, varSignal = "avgSignal")

selectTrain = function(angleNewObs, signals = NULL, m = 1){
  # m is the number of angles to keep between 1 and 5
  refs = seq(0, by = 45, length  = 8)
  nearestAngle = roundOrientation(angleNewObs)
  
  if (m %% 2 == 1) 
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
  else {
    m = m + 1
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
    if (sign(angleNewObs - nearestAngle) > -1) 
      angles = angles[ -1 ]
    else 
      angles = angles[ -m ]
  }
  angles = angles + nearestAngle
  angles[angles < 0] = angles[ angles < 0 ] + 360
  angles[angles > 360] = angles[ angles > 360 ] - 360
  angles = sort(angles) 
  
  offlineSubset = signals[ signals$angle %in% angles, ]
  reshapeSS(offlineSubset, varSignal = "avgSignal")
}

train130 = selectTrain(130, offlineSummary, m = 3)

head(train130)

length(train130[[1]])

findNN = function(newSignal, trainSubset) {
  diffs = apply(trainSubset[ , 4:9], 1, 
                function(x) x - newSignal)
  dists = apply(diffs, 2, function(x) sqrt(sum(x^2)) )
  closest = order(dists)
  return(trainSubset[closest, 1:3 ])
}

predXY = function(newSignals, newAngles, trainData, 
                  numAngles = 1, k = 3){
  
  closeXY = list(length = nrow(newSignals))
  
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    closeXY[[i]] = 
      findNN(newSignal = as.numeric(newSignals[i, ]), trainSS)
  }
  
  estXY = lapply(closeXY, 
                 function(x) sapply(x[ , 2:3], 
                                    function(x) mean(x[1:k])))
  estXY = do.call("rbind", estXY)
  return(estXY)
}
estXYk3 = predXY(newSignals = onlineSummary[ , 6:11], 
                 newAngles = onlineSummary[ , 4], 
                 offlineSummary, numAngles = 3, k = 3)

estXYk1 = predXY(newSignals = onlineSummary[ , 6:11], 
                 newAngles = onlineSummary[ , 4], 
                 offlineSummary, numAngles = 3, k = 1)

floorErrorMap = function(estXY, actualXY, trainPoints = NULL, AP = NULL){
  
  plot(0, 0, xlim = c(0, 35), ylim = c(-3, 15), type = "n",
       xlab = "", ylab = "", axes = FALSE)
  box()
  if ( !is.null(AP) ) points(AP, pch = 15)
  if ( !is.null(trainPoints) )
    points(trainPoints, pch = 19, col="grey", cex = 0.6)
  
  points(x = actualXY[, 1], y = actualXY[, 2], 
         pch = 19, cex = 0.8 )
  points(x = estXY[, 1], y = estXY[, 2], 
         pch = 8, cex = 0.8 )
  segments(x0 = estXY[, 1], y0 = estXY[, 2],
           x1 = actualXY[, 1], y1 = actualXY[ , 2],
           lwd = 2, col = "red")
}

trainPoints = offlineSummary[ offlineSummary$angle == 0 & 
                                offlineSummary$mac == "00:0f:a3:39:e1:c0" ,
                              c("posX", "posY")]

pdf(file="GEO_FloorPlanK3Errors.pdf", width = 10, height = 7)
oldPar = par(mar = c(1, 1, 1, 1))
floorErrorMap(estXYk3, onlineSummary[ , c("posX","posY")], 
              trainPoints = trainPoints, AP = AP)
par(oldPar)
dev.off()

pdf(file="GEO_FloorPlanK1Errors.pdf", width = 10, height = 7)
oldPar = par(mar = c(1, 1, 1, 1))
floorErrorMap(estXYk1, onlineSummary[ , c("posX","posY")], 
              trainPoints = trainPoints, AP = AP)
par(oldPar)
dev.off()

calcError = 
  function(estXY, actualXY) 
    sum( rowSums( (estXY - actualXY)^2) )

actualXY = onlineSummary[ , c("posX", "posY")]
sapply(list(estXYk1, estXYk3), calcError, actualXY)


v = 11
permuteLocs = sample(unique(offlineSummary$posXY))
permuteLocs = matrix(permuteLocs, ncol = v, 
                     nrow = floor(length(permuteLocs)/v))

onlineFold = subset(offlineSummary, posXY %in% permuteLocs[ , 1])

reshapeSS = function(data, varSignal = "signal", 
                     keepVars = c("posXY", "posX","posY"),
                     sampleAngle = FALSE, 
                     refs = seq(0, 315, by = 45)) {
  byLocation =
    with(data, by(data, list(posXY), 
                  function(x) {
                    if (sampleAngle) {
                      x = x[x$angle == sample(refs, size = 1), ]}
                    ans = x[1, keepVars]
                    avgSS = tapply(x[ , varSignal ], x$mac, mean)
                    y = matrix(avgSS, nrow = 1, ncol = 6,
                               dimnames = list(ans$posXY,
                                               names(avgSS)))
                    cbind(ans, y)
                  }))
  
  newDataSS = do.call("rbind", byLocation)
  return(newDataSS)
}

offline = offline[ offline$mac != "00:0f:a3:39:dd:cd", ]

keepVars = c("posXY", "posX","posY", "orientation", "angle")

onlineCVSummary = reshapeSS(offline, keepVars = keepVars, 
                            sampleAngle = TRUE)

onlineFold = subset(onlineCVSummary, 
                    posXY %in% permuteLocs[ , 1])

offlineFold = subset(offlineSummary,
                     posXY %in% permuteLocs[ , -1])

estFold = predXY(newSignals = onlineFold[ , 6:11], 
                 newAngles = onlineFold[ , 4], 
                 offlineFold, numAngles = 3, k = 3)

actualFold = onlineFold[ , c("posX", "posY")]
calcError(estFold, actualFold)

K = 20
err = rep(0, K)

for (j in 1:v) {
  onlineFold = subset(onlineCVSummary, 
                      posXY %in% permuteLocs[ , j])
  offlineFold = subset(offlineSummary,
                       posXY %in% permuteLocs[ , -j])
  actualFold = onlineFold[ , c("posX", "posY")]
  
  for (k in 1:K) {
    estFold = predXY(newSignals = onlineFold[ , 6:11],
                     newAngles = onlineFold[ , 4], 
                     offlineFold, numAngles = 3, k = k)
    err[k] = err[k] + calcError(estFold, actualFold)
  }
}

pdf(file = "Geo_CVChoiceOfK.pdf", width = 10, height = 6)
oldPar = par(mar = c(4, 3, 1, 1))
plot(y = err, x = (1:K),  type = "l", lwd= 2,
     ylim = c(1200, 2100),
     xlab = "Number of Neighbors",
     ylab = "Sum of Square Errors")

rmseMin = min(err)
kMin = which(err == rmseMin)[1]
segments(x0 = 0, x1 = kMin, y0 = rmseMin, col = gray(0.4), 
         lty = 2, lwd = 2)
segments(x0 = kMin, x1 = kMin, y0 = 1100,  y1 = rmseMin, 
         col = grey(0.4), lty = 2, lwd = 2)

#mtext(kMin, side = 1, line = 1, at = kMin, col = grey(0.4))
text(x = kMin - 2, y = rmseMin + 40, 
     label = as.character(round(rmseMin)), col = grey(0.4))
par(oldPar)
dev.off()

estXYk5 = predXY(newSignals = onlineSummary[ , 6:11], 
                 newAngles = onlineSummary[ , 4], 
                 offlineSummary, numAngles = 3, k = 5)

calcError(estXYk5, actualXY)

predXY = function(newSignals, newAngles, trainData, 
                  numAngles = 1, k = 3){
  
  closeXY = list(length = nrow(newSignals))
  
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    closeXY[[i]] = findNN(newSignal = as.numeric(newSignals[i, ]),
                          trainSS)
  }
  
  estXY = lapply(closeXY, function(x)
    sapply(x[ , 2:3], 
           function(x) mean(x[1:k])))
  estXY = do.call("rbind", estXY)
  return(estXY)
}
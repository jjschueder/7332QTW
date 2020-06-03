# # Environment Options ------------------------------------------------------
options(max.print = 999999999)

# # Libraries ------------------------------------------------------

library(XML)
library(rvest)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)

# # URLs ------------------------------------------------------
womenURLs = c("1999/cb99f.html",
              "2000/Cb003f.htm",
              "2001/oof_f.html",
              "2002/ooff.htm",
              "2003/CB03-F.HTM",
              "2004/women.htm",
              "2005/womennet.htm",
              "2006/womennet.htm",
              "2007/women.htm",
              "2008/women.htm",
              "2009/09cucb-F.htm",
              "2010/2010cucb10m-f.htm",
              "2011/2011cucb10m-f.htm",
              "2012/2012cucb10m-f.htm")

menURLs = c("1999/cb99m.html", 
            "2000/Cb003m.htm", 
            "2001/oof_m.html",
            "2002/oofm.htm", 
            "2003/CB03-M.HTM",
            "2004/men.htm", 
            "2005/CB05-M.htm", 
            "2006/men.htm", 
            "2007/men.htm", 
            "2008/men.htm", 
            "2009/09cucb-M.htm",
            "2010/2010cucb10m-m.htm", 
            "2011/2011cucb10m-m.htm",
            "2012/2012cucb10m-m.htm")

ubase = "http://www.cherryblossom.org/results/"
years = 1999:2012
url = paste(ubase, womenURLs, sep="")
sex = "women"


# # Functions ----------------------------------------------------

findColLocs = function(spacerRow) 
{
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else 
    return(c(0, spaceLocs))
}


selectCols = function(colNames, headerRow, searchLocs) 
{
  sapply(colNames, 
         function(name, headerRow, searchLocs)
         {
           startPos = regexpr(name, headerRow)[[1]]
           if (startPos == -1) 
             return( c(NA, NA) )
           index = sum(startPos >= searchLocs)
           c(searchLocs[index] + 1, searchLocs[index + 1] - 1)
         },
         headerRow = headerRow, searchLocs = searchLocs )
}

convertTime = function(time) {
  timePieces = strsplit(time, ":")
  timePieces = sapply(timePieces, as.numeric)
  sapply(timePieces, function(x) {
    if (length(x) == 2) x[1] + x[2]/60
    else 60*x[1] + x[2] + x[3]/60
  }
  )
}

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

#===========================================================================
# WOMEN DATA - CHERRY BLOSSOM 10K 
#===========================================================================
# # 1999 -----------------------------------------------------------
seq <- 1
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place <-     substring(body,0,5)
div_total <- substring(body,6,15)
numId <-     substring(body,16,16)
name <-      substring(body,16,37)
age <-       substring(body,38,40)
hometown <-  substring(body,41,59)
guntime <-   substring(body,16,16)
netTime <-   substring(body,60,67)
pace <-      substring(body,68,73)

df_women_10K <- data.frame(year,
                            sex,
                            place,
                            div_total,
                            numId,
                            name,
                            age,
                            hometown,
                            guntime,
                            netTime,
                            pace)

# # 2000 -----------------------------------------------------------
seq <- 2
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    ff = getNodeSet(doc, "//font")
                    txt = xmlValue(ff[[4]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place <-     substring(body,0,5)
div_total <- substring(body,6,15)
numId <-     substring(body,16,21)
name <-      substring(body,22,43)
age <-       substring(body,44,46)
hometown <-  substring(body,47,65)
guntime <-   substring(body,66,73)
netTime <-   substring(body,76,82)
pace <-      substring(body,83,83)

df_women_10K <- rbind(df_women_10K,data.frame ( year,
                                                sex,
                                                place,
                                                div_total,
                                                numId,
                                                name,
                                                age,
                                                hometown,
                                                guntime,
                                                netTime,
                                                pace))

# # 2001 -----------------------------------------------------------
seq <- 3
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = 3 #grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <-  substring(body,1,5)
div_total <- substring(body,6,6)
numId    <-  substring(body,6,11)
name     <-  substring(body,12,33)
age      <-  substring(body,34,36)
hometown <-  substring(body,37,55)
guntime  <-  substring(body,56,63)
netTime  <-  substring(body,64,71)
pace <-      substring(body,6,6)

df_women_10K <- rbind(df_women_10K,data.frame ( year,
                                                sex,
                                                place,
                                                div_total,
                                                numId,
                                                name,
                                                age,
                                                hometown,
                                                guntime,
                                                netTime,
                                                pace))

# # 2002 -----------------------------------------------------------
seq <- 4
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = 3 #grep("^===", file)
eqIndex
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <-  substring(body,1,5)
div_total <- substring(body,6,6)
numId    <-  substring(body,6,11)
name     <-  substring(body,12,33)
age      <-  substring(body,34,36)
hometown <-  substring(body,37,55)
guntime  <-  substring(body,64,72)
netTime  <-  substring(body,56,63)
pace <-      substring(body,6,6)

df_women_10K <- rbind(df_women_10K,data.frame ( year,
                                                sex,
                                                place,
                                                div_total,
                                                numId,
                                                name,
                                                age,
                                                hometown,
                                                guntime,
                                                netTime,
                                                pace))

# # 2003 -----------------------------------------------------------
seq <- 5
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <-  substring(body,1,5)
div_total <- substring(body,6,15)
numId    <-  substring(body,16,21)
name     <-  substring(body,22,53)
age      <-  substring(body,54,55)
hometown <-  substring(body,56,75)
guntime  <-  substring(body,76,83)
netTime  <-  substring(body,86,92)
pace <-      substring(body,6,6)

df_women_10K <- rbind(df_women_10K,data.frame ( year,
                                                sex,
                                                place,
                                                div_total,
                                                numId,
                                                name,
                                                age,
                                                hometown,
                                                guntime,
                                                netTime,
                                                pace))

# # 2004 -----------------------------------------------------------
seq <- 6
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <-  substring(body,1,5)
div_total <- substring(body,6,15)
numId    <-  substring(body,16,21)
name     <-  substring(body,22,52)
age      <-  substring(body,53,54)
hometown <-  substring(body,55,74)
guntime  <-  substring(body,83,90)
netTime  <-  substring(body,75,82)
pace <-      substring(body,6,6)

df_women_10K <- rbind(df_women_10K,data.frame ( year,
                                                sex,
                                                place,
                                                div_total,
                                                numId,
                                                name,
                                                age,
                                                hometown,
                                                guntime,
                                                netTime,
                                                pace))

# # 2005 -----------------------------------------------------------
seq <- 7
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <- substring(body,1,5)
div_total <- substring(body,6,15)
numId    <- substring(body,16,22)
name     <- substring(body,23,45)
age      <- substring(body,46,48)
hometown <- substring(body,49,67)
guntime  <- substring(body,68,75)
netTime  <- substring(body,76,82)
pace <-     substring(body,6,6)

df_women_10K <- rbind(df_women_10K,data.frame ( year,
                                                sex,
                                                place,
                                                div_total,
                                                numId,
                                                name,
                                                age,
                                                hometown,
                                                guntime,
                                                netTime,
                                                pace))

# # 2006 -----------------------------------------------------------
seq <- 8
urls <- url[seq]
year <- years[seq]


extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <- substring(body,1,5)
div_total <- substring(body,6,15)
numId    <- substring(body,16,22)
name     <- substring(body,23,45)
age      <- substring(body,46,48)
hometown <- substring(body,49,64)
guntime  <- substring(body,65,72)
netTime  <- substring(body,73,79)
pace     <- substring(body,81,87)

df_women_10K <- rbind(df_women_10K,data.frame ( year,
                                                sex,
                                                place,
                                                div_total,
                                                numId,
                                                name,
                                                age,
                                                hometown,
                                                guntime,
                                                netTime,
                                                pace))

# # 2007 -----------------------------------------------------------
seq <- 9
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <-  substring(body,1,5)
div_total <- substring(body,6,17)
numId    <-  substring(body,18,24)
name     <-  substring(body,25,48)
age      <-  substring(body,49,50)
hometown <-  substring(body,51,70)
guntime  <-  substring(body,6,6)
netTime  <-  substring(body,71,77)
pace     <-  substring(body,79,84)

df_women_10K <- rbind(df_women_10K,data.frame ( year,
                                                sex,
                                                place,
                                                div_total,
                                                numId,
                                                name,
                                                age,
                                                hometown,
                                                guntime,
                                                netTime,
                                                pace))

# # 2008 -----------------------------------------------------------
seq <- 10
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <- substring(body,1,5)
div_total <- substring(body,6,17)
numId    <- substring(body,18,24)
name     <- substring(body,25,48)
age      <- substring(body,49,50)
hometown <- substring(body,51,70)
guntime  <- substring(body,6,6)
netTime  <- substring(body,99,106)
pace     <- substring(body,107,112)


df_women_10K <- rbind(df_women_10K,data.frame ( year,
                                                sex,
                                                place,
                                                div_total,
                                                numId,
                                                name,
                                                age,
                                                hometown,
                                                guntime,
                                                netTime,
                                                pace))

# # 2009 -----------------------------------------------------------
seq <- 11
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <-  substring(body,1,5)
div_total <- substring(body,6,17)
numId    <-  substring(body,18,24)
name     <-  substring(body,25,48)
age      <-  substring(body,49,50)
hometown <-  substring(body,51,72)
guntime  <-  substring(body,73,79)
netTime  <-  substring(body,80,87)
pace     <-  substring(body,89,94)

df_women_10K <- rbind(df_women_10K,data.frame ( year,
                                                sex,
                                                place,
                                                div_total,
                                                numId,
                                                name,
                                                age,
                                                hometown,
                                                guntime,
                                                netTime,
                                                pace))

# # 2010 -----------------------------------------------------------
seq <- 12
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <- substring(body,1,5)
div_total <- substring(body,6,17)
numId    <- substring(body,18,24)
name     <- substring(body,25,48)
age      <- substring(body,49,50)
hometown <- substring(body,51,72)
guntime  <- substring(body,81,87)
netTime  <- substring(body,88,95)
pace     <- substring(body,97,102)

df_women_10K <- rbind(df_women_10K,data.frame ( year,
                                                sex,
                                                place,
                                                div_total,
                                                numId,
                                                name,
                                                age,
                                                hometown,
                                                guntime,
                                                netTime,
                                                pace))

# # 2011 -----------------------------------------------------------
seq <- 13
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <-  substring(body,1,5)
div_total <- substring(body,6,17)
numId    <-  substring(body,18,24)
name     <-  substring(body,25,48)
age      <-  substring(body,49,50)
hometown <-  substring(body,51,72)
guntime  <-  substring(body,81,87)
netTime  <-  substring(body,89,95)
pace     <-  substring(body,98,102)

df_women_10K <- rbind(df_women_10K,data.frame ( year,
                                                sex,
                                                place,
                                                div_total,
                                                numId,
                                                name,
                                                age,
                                                hometown,
                                                guntime,
                                                netTime,
                                                pace))

# # 2012 -----------------------------------------------------------
seq <- 14
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <-  substring(body,1,5)
div_total <- substring(body,6,17)
numId    <-  substring(body,18,24)
name     <-  substring(body,25,48)
age      <-  substring(body,49,50)
hometown <-  substring(body,51,72)
guntime  <-  substring(body,6,6)
netTime  <-  substring(body,81,87)
pace     <-  substring(body,89,94)

df_women_10K <- rbind(df_women_10K,data.frame ( year,
                                                sex,
                                                place,
                                                div_total,
                                                numId,
                                                name,
                                                age,
                                                hometown,
                                                guntime,
                                                netTime,
                                                pace))

#===========================================================================
# MEN DATA - CHERRY BLOSSOM 10K
#===========================================================================
url = paste(ubase, menURLs, sep="")
sex = "men"

# # 1999 -----------------------------------------------------------
seq <- 1
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex] 
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place     <- substring(body,0,5)
div_total <- substring(body,6,15)
numId     <- substring(body,6,6)
name      <- substring(body,16,37)
age       <- substring(body,38,40)
hometown  <- substring(body,41,59)
guntime   <- substring(body,6,6)
netTime   <- substring(body,60,67)
pace      <- substring(body,68,73)


df_men_10K <- data.frame (year,
                          sex,
                          place,
                          div_total,
                          numId,
                          name,
                          age,
                          hometown,
                          guntime,
                          netTime,
                          pace)

# # 2000 -----------------------------------------------------------
seq <- 2
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    ff = getNodeSet(doc, "//font")
                    txt = xmlValue(ff[[4]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place     <- substring(body,0,5)
div_total <- substring(body,6,15)
numId     <- substring(body,16,21)
name      <- substring(body,22,43)
age       <- substring(body,44,46)
hometown  <- substring(body,47,65)
guntime   <- substring(body,66,73)
netTime   <- substring(body,76,82)
pace      <- substring(body,6,6)

df_men_10K <- rbind(df_men_10K,data.frame ( year,
                                              sex,
                                              place,
                                              div_total,
                                              numId,
                                              name,
                                              age,
                                              hometown,
                                              guntime,
                                              netTime,
                                              pace))

# # 2001 -----------------------------------------------------------
seq <- 3
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place     <- substring(body,1,5)
div_total <- substring(body,6,6)
numId     <- substring(body,6,11)
name      <- substring(body,12,33)
age       <- substring(body,34,36)
hometown  <- substring(body,37,55)
guntime   <- substring(body,56,63)
netTime   <- substring(body,64,71)
pace      <- substring(body,6,6)

df_men_10K <- rbind(df_men_10K,data.frame ( year,
                                              sex,
                                              place,
                                              div_total,
                                              numId,
                                              name,
                                              age,
                                              hometown,
                                              guntime,
                                              netTime,
                                              pace))

# # 2002 -----------------------------------------------------------
seq <- 4
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place     <- substring(body,1,5)
div_total <- substring(body,6,6)
numId     <- substring(body,6,11)
name      <- substring(body,12,33)
age       <- substring(body,34,36)
hometown  <- substring(body,37,55)
guntime   <- substring(body,64,72)
netTime   <- substring(body,56,63)
pace      <- substring(body,6,6)

df_men_10K <- rbind(df_men_10K,data.frame ( year,
                                              sex,
                                              place,
                                              div_total,
                                              numId,
                                              name,
                                              age,
                                              hometown,
                                              guntime,
                                              netTime,
                                              pace))

# # 2003 -----------------------------------------------------------
seq <- 5
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place     <- substring(body,1,5)
div_total <- substring(body,6,15)
numId     <- substring(body,16,21)
name      <- substring(body,22,53)
age       <- substring(body,54,55)
hometown  <- substring(body,56,75)
guntime   <- substring(body,76,83)
netTime   <- substring(body,86,92)
pace      <- substring(body,6,6)

df_men_10K <- rbind(df_men_10K,data.frame ( year,
                                              sex,
                                              place,
                                              div_total,
                                              numId,
                                              name,
                                              age,
                                              hometown,
                                              guntime,
                                              netTime,
                                              pace))

# # 2004 -----------------------------------------------------------
seq <- 6
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <- substring(body,1,5)
div_total <- substring(body,6,15)
numId    <- substring(body,16,21)
name     <- substring(body,22,52)
age      <- substring(body,53,54)
hometown <- substring(body,55,74)
guntime  <- substring(body,83,90)
netTime  <- substring(body,75,82)
pace      <- substring(body,6,6)

df_men_10K <- rbind(df_men_10K,data.frame ( year,
                                            sex,
                                            place,
                                            div_total,
                                            numId,
                                            name,
                                            age,
                                            hometown,
                                            guntime,
                                            netTime,
                                            pace))

# # 2005 -----------------------------------------------------------
seq <- 7
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place     <- substring(body,1,5)
div_total <- substring(body,6,15)
numId     <- substring(body,6,6)
name      <- substring(body,17,38)
age       <- substring(body,40,41)
hometown  <- substring(body,43,61)
netTime   <- substring(body,62,69)
guntime   <- substring(body,70,77)
pace      <- substring(body,78,82)

df_men_10K <- rbind(df_men_10K,data.frame ( year,
                                            sex,
                                            place,
                                            div_total,
                                            numId,
                                            name,
                                            age,
                                            hometown,
                                            guntime,
                                            netTime,
                                            pace))

# # 2006 -----------------------------------------------------------
seq <- 8
urls <- url[seq]
year <- years[seq]


extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place    <- substring(body,1,5)
div_total <-substring(body,6,15)
numId    <- substring(body,16,22)
name     <- substring(body,23,45)
age      <- substring(body,46,48)
hometown <- substring(body,49,64)
guntime  <- substring(body,65,72)
netTime  <- substring(body,73,79)
pace     <- substring(body,81,87)

df_men_10K <- rbind(df_men_10K,data.frame ( year,
                                            sex,
                                            place,
                                            div_total,
                                            numId,
                                            name,
                                            age,
                                            hometown,
                                            guntime,
                                            netTime,
                                            pace))

# # 2007 -----------------------------------------------------------
seq <- 9
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place     <- substring(body,1,5)
div_total <- substring(body,6,17)
numId     <- substring(body,18,24)
name      <- substring(body,25,48)
age       <- substring(body,49,50)
hometown  <- substring(body,51,70)
guntime   <- substring(body,6,6)
netTime   <- substring(body,71,77)
pace      <- substring(body,79,84)

df_men_10K <- rbind(df_men_10K,data.frame ( year,
                                              sex,
                                              place,
                                              div_total,
                                              numId,
                                              name,
                                              age,
                                              hometown,
                                              guntime,
                                              netTime,
                                              pace))

# # 2008 -----------------------------------------------------------
seq <- 10
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place     <- substring(body,1,5)
div_total <- substring(body,6,17)
numId     <- substring(body,18,24)
name      <- substring(body,25,48)
age       <- substring(body,49,50)
hometown  <- substring(body,51,70)
guntime   <- substring(body,6,6)
netTime   <- substring(body,99,106)
pace      <- substring(body,107,112)

df_men_10K <- rbind(df_men_10K,data.frame ( year,
                                              sex,
                                              place,
                                              div_total,
                                              numId,
                                              name,
                                              age,
                                              hometown,
                                              guntime,
                                              netTime,
                                              pace))

# # 2009 -----------------------------------------------------------
seq <- 11
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
{ doc = htmlParse(urls)
div1 = getNodeSet(doc, "//div[@class='Section1']")
pres = getNodeSet(div1[[1]], "//pre")
els = sapply(pres, xmlValue)
return(els)
}

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
body <- substring(body,1,160)
body <- str_trim(gsub("Â","",body[1:length(body)]))
place <- str_trim(substring(body,1,4)) 
#place <- as.data.frame(place)
div_total <- str_trim(substring(body,5,17)) 
#div_total <- as.data.frame(div_total)
numId <- str_trim(gsub("[a-zA-Z ]", "", substring(body,18,24))) 
#numId <- as.data.frame(numId)
name <- str_trim(gsub('[0-9]+', '',substring(body,15,47))) 
#name <- as.data.frame(name)
age <- str_trim(gsub("[a-zA-Z ]", "",substring(body,45,50))) 
#age <- as.data.frame(age)
hometown <- str_trim(gsub('[0-9]+', '',substring(body,48,70))) 
#hometown <- as.data.frame(hometown)
guntime  <-  str_trim(
                      stri_replace_all_fixed(
                                              substring(body,71,78)
                                              , pattern = c("#", "*"), replacement = c(""), vectorize_all = FALSE)
                      ) 
#guntime <- as.data.frame(guntime)
netTime  <-  str_trim(
                      stri_replace_all_fixed(
                        substring(body,79,86)
                        , pattern = c("#", "*"), replacement = c(""), vectorize_all = FALSE)
                    ) 

#netTime <- as.data.frame(netTime)
pace     <-  str_trim(
                        stri_replace_all_fixed(
                          substring(body,87,99)
                          , pattern = c("#", "*"), replacement = c(""), vectorize_all = FALSE)
                      ) 
#pace <- as.data.frame(pace)
df_men_10K <- rbind(df_men_10K,data.frame ( year,
                                            sex,
                                            place,
                                            div_total,
                                            numId,
                                            name,
                                            age,
                                            hometown,
                                            guntime,
                                            netTime,
                                            pace))

# # 2010 -----------------------------------------------------------
seq <- 12
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place     <- substring(body,1,5)
div_total <- substring(body,6,17)
numId     <- substring(body,18,24)
name      <- substring(body,25,48)
age       <- substring(body,49,50)
hometown  <- substring(body,51,72)
guntime   <- substring(body,81,87)
netTime   <- substring(body,88,95)
pace      <- substring(body,97,102)

df_men_10K <- rbind(df_men_10K,data.frame ( year,
                                              sex,
                                              place,
                                              div_total,
                                              numId,
                                              name,
                                              age,
                                              hometown,
                                              guntime,
                                              netTime,
                                              pace))

# # 2011 -----------------------------------------------------------
seq <- 13
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place     <- substring(body,1,5)
div_total <- substring(body,6,17)
numId     <- substring(body,18,24)
name      <- substring(body,25,48)
age       <- substring(body,49,50)
hometown  <- substring(body,51,72)
guntime   <- substring(body,81,87)
netTime   <- substring(body,89,95)
pace      <- substring(body,98,102)

df_men_10K <- rbind(df_men_10K,data.frame ( year,
                                              sex,
                                              place,
                                              div_total,
                                              numId,
                                              name,
                                              age,
                                              hometown,
                                              guntime,
                                              netTime,
                                              pace))

# # 2012 -----------------------------------------------------------
seq <- 14
urls <- url[seq]
year <- years[seq]

extractResTable = function(urls)
                  {
                    doc = htmlParse(urls)
                    preNode = getNodeSet(doc, "//pre")
                    txt = xmlValue(preNode[[1]])
                    els = strsplit(txt, "\r\n")[[1]]
                    return(els)
                  }

file = extractResTable(urls)

eqIndex = grep("^===", file)
spacerRow = file[eqIndex]
headerRow = tolower(file[ eqIndex - 1 ])
body = file[ -(1 : eqIndex) ]
place     <- substring(body,1,5)
div_total <- substring(body,6,17)
numId     <- substring(body,18,24)
name      <- substring(body,25,48)
age       <- substring(body,49,50)
hometown  <- substring(body,51,72)
guntime   <- substring(body,6,6)
netTime   <- substring(body,81,87)
pace      <- substring(body,89,94)

df_men_10K <- rbind(df_men_10K,data.frame ( year,
                                              sex,
                                              place,
                                              div_total,
                                              numId,
                                              name,
                                              age,
                                              hometown,
                                              guntime,
                                              netTime,
                                              pace))


# # Cleaning dataframes ------------------------------------------------------
# trim leading and training whitespaces
df_women_10K <- data.frame(lapply(df_women_10K, trimws),stringsAsFactors = FALSE)


# convert place, numid, year and age to numerical
df_women_10K$place = as.numeric(df_women_10K$place)
df_women_10K$numId = as.numeric(df_women_10K$numId)
df_women_10K$year = as.numeric(df_women_10K$year)
df_women_10K$age = as.numeric(df_women_10K$age)


# convert guntime, nettime and pace to time
df_women_10K$guntime = convertTime(df_women_10K$guntime)
df_women_10K$netTime = convertTime(df_women_10K$netTime)
df_women_10K$pace = convertTime(df_women_10K$pace)


# trim leading and training whitespaces
df_men_10K <- data.frame(lapply(df_men_10K, trimws),stringsAsFactors = FALSE)


# convert place, numid, year and age to numerical
df_men_10K$place = as.numeric(df_men_10K$place)
df_men_10K$numId = as.numeric(df_men_10K$numId)
df_men_10K$year = as.numeric(df_men_10K$year)
df_men_10K$age = as.numeric(df_men_10K$age)


# Replace age=251 with age =25 (verified by age from prior years)
# df_men_10K$age = gsub(251,25,df_men_10K$age)


# convert guntime, nettime and pace to time
df_men_10K$guntime = convertTime(df_men_10K$guntime)
df_men_10K$netTime = convertTime(df_men_10K$netTime)
df_men_10K$pace = convertTime(df_men_10K$pace)


# # Merging dataframes -------------------------------------------------------
# Consolidate the dataframes for the men and the women
df_men_and_women_10K_Final <- rbind(df_women_10K,df_men_10K)


# Remove records where the age or nettime is NA
df_women_10K <- subset(df_women_10K, df_women_10K$place!="NA") 
df_men_10K <- subset(df_men_10K, df_men_10K$place!="NA") 
df_men_and_women_10K_Final <- subset(df_men_and_women_10K_Final, df_men_and_women_10K_Final$place!="NA")
df_men_and_women_10K_Final <- subset(df_men_and_women_10K_Final, df_men_and_women_10K_Final$age!="NA")
df_men_and_women_10K_Final <- subset(df_men_and_women_10K_Final, df_men_and_women_10K_Final$netTime!="NA")


# Replace a single record with age 251 with age 25 after verifying the runner's age from prior years'
df_men_and_women_10K_Final$age <- gsub(251,25,df_men_and_women_10K_Final$age)


# Dropping columns that have no value to our analysis or do not have data for all years i.e. guntime
df_men_and_women_10K_Final = subset(df_men_and_women_10K_Final, select = -c(div_total,numId,guntime) )


# # Writing to csv -----------------------------------------------------------
write.csv(df_women_10K,'df_women_10K_Final.csv')
write.csv(df_men_10K,'df_men_10K_Final.csv')
write.csv(df_men_and_women_10K_Final,'df_men_and_women_10K_Final.csv')
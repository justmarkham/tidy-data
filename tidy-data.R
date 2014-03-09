# load libraries
library(reshape2)   # needed for melt(), dcast()
library(stringr)    # needed for str_replace(), str_sub()
library(dplyr)      # needed for arrange(), select(), inner_join()
library(lubridate)  # needed for ymd(), weeks()



## Problem 1: column headers are values instead of variable names

# read pew.txt (tab-delimited file) into a data frame
pew.raw <- read.delim("data/pew.txt", check.names=FALSE, stringsAsFactors=FALSE)

# view data frame: variables are religion, income, count
View(pew.raw)

# melt pew.raw: second argument is columns that are already variables
pew.tidy <- melt(pew.raw, "religion")

# fix column names
names(pew.tidy) <- c("religion", "income", "n")



## Problem 2: multiple variables in one column

# read tb.csv (comma-separated file) into a data frame
tb.raw <- read.csv("data/tb.csv", stringsAsFactors=FALSE)

# view data frame: variables are country, year, sex, age, count
View(tb.raw)

# remove an unnecessary column, then remove "new_sp_" from each column name
tb.raw$new_sp <- NULL
names(tb.raw) <- str_replace(names(tb.raw), "new_sp_", "")

# melt tb.raw and remove NA values
tb.tidy <- melt(tb.raw, id=c("iso2", "year"), na.rm=TRUE)

# rename column "value" to be "cases"
names(tb.tidy)[4] <- "cases"

# reorder the rows of the data frame by iso2, then variable, then year
tb.tidy <- arrange(tb.tidy, iso2, variable, year)

# check that string subsets of "variable" will work as desired...
# character 1 through character 1 will become the sex variable
head(str_sub(tb.tidy$variable, 1, 1), 20)
# character 2 through the end will become the age variable
head(str_sub(tb.tidy$variable, 2), 20)

# construct a lookup table using a named character vector
ages <- c("04"="0-4", "514"="5-14", "014"="0-14", "1524"="15-24",
          "2534"="25-34", "3544"="35-44", "4554"="45-54",
          "5564"="55-64", "65"="65+", "u"=NA)

# check that the string subset of age using "ages" vector will work as desired
head(ages[str_sub(tb.tidy$variable, 2)], 20)

# add the sex variable to the data frame as a character
tb.tidy$sex <- str_sub(tb.tidy$variable, 1, 1)

# add the age variable to the data frame as a factor
tb.tidy$age <- factor(ages[str_sub(tb.tidy$variable, 2)], levels = ages)

# remove the "variable" column, since it is no longer needed
tb.tidy$variable <- NULL

# rearrange the data frame columns so that "cases" is last
tb.tidy <- select(tb.tidy, iso2, year, sex, age, cases)



## Problem 3: variables in rows and columns

# read weather.txt (tab-delimited file) into a data frame
weather.raw <- read.delim("data/weather.txt", stringsAsFactors=FALSE)

# view data frame: variables are id, year, month, day, min temp, max temp
View(weather.raw)

# melt weather.raw and remove NA values
weather.raw1 <- melt(weather.raw, id=c("id", "year", "month", "element"),
                     na.rm=TRUE)

# create a new variable "day" from the variable named "variable":
# remove "d" and convert it to an integer
weather.raw1$day <- as.integer(str_replace(weather.raw1$variable, "d", ""))

# remove the "variable" column, since it is no longer needed
weather.raw1$variable <- NULL

# change values in "element" column to be lowercase
weather.raw1$element <- tolower(weather.raw1$element)

# rearrange the data frame columns, then reorder the rows
weather.raw1 <- select(weather.raw1, id, year, month, day, element, value)
weather.raw1 <- arrange(weather.raw1, year, month, day, element)

# shift "element" variable from a row to a column
weather.tidy <- dcast(weather.raw1, ... ~ element)



## Problem 4: multiple types in the same table

# read billboard.csv (comma-separated file) into a data frame
billboard.raw <- read.csv("data/billboard.csv", stringsAsFactors = FALSE)

# view data frame: variables are artist, track, genre, time, date, week, rank
View(billboard.raw)

# remove an unnecessary column
billboard.raw$date.peaked <- NULL

# change encoding of artist column, then remove certain characters from track column
billboard.raw$artist.inverted <- iconv(billboard.raw$artist.inverted, from="MAC", to="UTF-8")
billboard.raw$track <- str_replace(billboard.raw$track, " \\(.*?\\)", "")

# change the names of every column (except for the first 6) to 1 through 76
names(billboard.raw)[-(1:6)] <- 1:76

# melt billboard.raw (except for the first 6 columns) and remove NA values
billboard.tidy <- melt(billboard.raw, 1:6, na.rm = TRUE)

# create a new variable "week" by converting "variable" from a factor to an integer
billboard.tidy$week <- as.integer(as.character(billboard.tidy$variable))

# remove the "variable" column, since it is no longer needed
billboard.tidy$variable <- NULL

# convert the date into a POSIXct object (so that we can do math on it)
billboard.tidy$date.entered <- ymd(billboard.tidy$date.entered)

# create an explicit date column as a function of "date.entered" and "week"
billboard.tidy$date <- billboard.tidy$date.entered + weeks(billboard.tidy$week - 1)

# remove the "date.entered" column, since it is no longer needed
billboard.tidy$date.entered <- NULL

# rename columns "artist.inverted" and "value"
names(billboard.tidy)[2] <- "artist"
names(billboard.tidy)[6] <- "rank"

# rearrange the data frame columns, then reorder the rows
billboard.tidy <- select(billboard.tidy, year, artist, track, time, genre,
                         week, date, rank)
billboard.tidy <- arrange(billboard.tidy, year, artist, track, week)

# create a new data frame for song information only, and remove the row names
billboard.song <- unique(select(billboard.tidy, artist, track, genre, time))
rownames(billboard.song) <- NULL

# add a new song_id column to billboard.song, then make it the first column
billboard.song$song_id <- 1:nrow(billboard.song)
billboard.song <- select(billboard.song, song_id, artist, track, genre, time)

# create a new data frame for rank information only
billboard.rank <- inner_join(billboard.tidy, billboard.song)
billboard.rank <- select(billboard.rank, song_id, week, date, rank)

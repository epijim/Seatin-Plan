################################
## Variable to set            ##
################################

# Location of files
  dir <- "~/funwithcode_local/seatingplan"
# Name of input file from google form
  input <- "toR.csv"
# Name of input file from college
  input2 <- "toR2.csv"
# Fellows (reserved seats)
  fellow_HT <- 2
  fellow_2HT <- 2

################################
## Seats in hall              ##
################################

hightable <- 18
lowerhightable <- 18

north1 <- 8
north2 <- 8
north3 <- 8
north4 <- 6

south1 <- 8
south2 <- 8
south3 <- 8
south4 <- 6

centretable <- 36

# Make dataframe of tables
allseats <- c(
  rep("hightable",hightable),
  rep("lowerhightable",lowerhightable),
  rep("north1",north1),
  rep("north2",north2),
  rep("north3",north3),
  rep("north4",north4),
  rep("south1",south1),
  rep("south2",south2),
  rep("south3",south3),
  rep("south4",south4),
  rep("centretable",centretable)
)

################################
## Load in and clean data     ##
################################

setwd(dir)

data <- read.csv(input,
                 stringsAsFactors=F,
                 na.strings = "")

data2 <- read.csv(input2,
                 stringsAsFactors=F,
                 na.strings = "")

# Create groups
t_names <- NULL
t_group <- NULL
  for(i in 1:nrow(data2)){
    t_currgroup <- unlist(
      strsplit(data2[i,],", ")
    )
    t_currlength <- as.numeric(length(t_currgroup))
    t_currlength <- rep(i,t_currlength)
    t_names <-c(t_names,t_currgroup)
    t_group <- c(t_group,t_currlength)
  }

groups <- data.frame(t_names, t_group, 
                     stringsAsFactors=F)

# Check for duplicates!
  intwice <- groups[duplicated(groups$t_names),]
  # Sort duplicates
  intwice <- intwice[order(intwice$t_names),]

# Remove duplicates
  library(plyr)
  groups <- unique(ddply(groups, .(t_names), 
               function(x) data.frame(
                 t_names=x[,"t_names"], 
                 t_group=min(x$t_group)
                 )))
  groups$t_names <- as.character(groups$t_names)

# Count number in each group
  groups$sum <- as.numeric(
    ave(groups$t_names, groups$t_group, FUN=length)
  )

# Order 
  groups <- groups[with(groups, order(-sum,t_group)),]

# See who is not in a group!
  notingroup <-
    setdiff(c(na.omit(data$Name),na.omit(data$Guest.name)),
        groups$t_names
    )  
# Double check people not in a group aren't with a guest
notingroup

################################
## Output for python          ##
################################


################################
## Run python                 ##
################################
library(rPython)

# Load/run the main Python script
python.load("GetNewRedditSubmissions.py")

# Get the variable
new_subs_data <- python.get("new_subs")

# Load/run re-fecth script
python.load("RefreshNewSubs.py")

# Get the updated variable
new_subs_data <- python.get("new_subs")

head(new_subs_data)

################################
## Summary                    ##
################################

  numberPeople <- as.numeric(length(fulllist))
  numberJesuans <- as.numeric(nrow(data))
  numberGuests <- numberPeople-numberJesuans

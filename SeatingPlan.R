################################
## Variable to set            ##
################################

# Location of files
  dir <- "~/funwithcode_local/seatingplan"
# Name of input file from college
  input <- "toR.csv"
# Name of input file from google form
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
## Algorithm                  ##
################################
allseats <- data.frame(allseats, stringsAsFactors=F)
allseats$guest <- NA

  # get unique groups
  group_sizes <- subset(groups,select=c(t_group,sum))
  group_sizes <- unique(group_sizes)

## add back in fellow seats
for(i in 1:fellow_HT){
  allseats$guest[10+i] <- "FELLOW"
}
for(i in 1:fellow_2HT){
  allseats$guest[20+i] <- "FELLOW"
}

# count free spaces
  allseats$free <- ifelse(is.na(allseats$guest),
                           1,0)
  allseats$total_free <- as.numeric(
    ave(allseats$free, allseats$allseats, FUN=sum)
  )
  # sort and put biggest free table at the top
  allseats <- allseats[with(allseats, order(-total_free,allseats,
                                           free)),]


# loop through groups
for(i_groups in 1:nrow(group_sizes)){
  # add table
  i_people <- group_sizes[i_groups,"sum"]
  i_currgroup <- group_sizes[i_groups,"t_group"]
  i_done <- subset(groups,t_group==i_currgroup,select=t_names)
  i_spaceoncurrenttable <- allseats[1,"total_free"]
  if (i_spaceoncurrenttable<i_people) break
    for(i_table in 1:i_people){
      allseats[i_table,"guest"] <- i_done[i_table,]
    }
  allseats$free <- ifelse(is.na(allseats$guest),
                          1,0)
  allseats$total_free <- as.numeric(
    ave(allseats$free, allseats$allseats, FUN=sum)
  )
  allseats <- allseats[with(allseats, order(-free,
                                            -total_free,allseats
                                            )),]
}

# add in people not in groups
for(i_groups in 1:length(notingroup)){
  # add table
  i_person <- notingroup[i_groups]
  allseats[i_table,"guest"] <- paste0(i_person," (Alone)")
  allseats$free <- ifelse(is.na(allseats$guest),
                          1,0)
  allseats$total_free <- as.numeric(
    ave(allseats$free, allseats$allseats, FUN=sum)
  )
  allseats <- allseats[with(allseats, order(-free,
                                            total_free,allseats
  )),]
}

fulllist <- c(
  na.omit(data$Name),
  na.omit(data$Guest.name))
  


## reorder
  
allseats <- allseats[order(as.numeric(rownames(allseats))),]

################################
## Summary                    ##
################################

  numberPeople <- as.numeric(length(fulllist))
  numberJesuans <- as.numeric(nrow(data))
  numberGuests <- numberPeople-numberJesuans
  numberSeated <- length(na.omit(allseats$guest))
  numberFellows <- fellow_2HT+fellow_HT

allseats <- subset(allseats, select=c(allseats,guest))
write.csv(allseats, "allseats.csv")

worked <- ifelse((numberPeople+numberFellows)==numberSeated,
                 "Success!","Failure! some not in seats")

paste0(worked,
       " : There are ",numberSeated," in the seating plan. ",
       numberPeople," brought a ticket (",
       numberJesuans," are Jesuans) and ",
       numberFellows," are fellows.")
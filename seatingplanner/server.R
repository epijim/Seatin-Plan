library(shiny)
library(dplyr)

shinyServer(function(input, output) {
  
#######################################################################  
## Clean raw data             ##
################################
  
  # load sold tickets data
  input_raw <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    input_raw <- input$input_raw
    
    if (is.null(input_raw))
      return(NULL)
    
    read.csv(
      input_raw$datapath, 
      header=TRUE, 
      sep=',', 
      quote='',
      stringsAsFactors=F,
      na.strings = "")
  })
  
  input_cleaned <- reactive({
    # get data
      input_raw <- input_raw()
      
      if (is.null(input_raw))
        return(NULL)
      
    # take first name
      first_name <- stringr::word(input_raw[,1],1)
    # take last name
      last_name <- stringr::word(input_raw[,2],1)
    # party size 
      party_size <- input_raw[,3]
    # build guest list
      input_raw <- data.frame(
        First = first_name,
        Last = last_name,
        Party_Size = party_size,
        stringsAsFactors = FALSE
      )
      # deal with duplicates
        # this happens when guest tickets are brought at a
        # different time to the first ticket
        input_raw <- input_raw %>%
          group_by(First,Last) %>%
          summarise(
            Party_Size=sum(Party_Size)
            ) %>% as.data.frame

      # subset to those with party size > 1
      with_guests <- input_raw %>%
        filter(Party_Size>1) %>%
        mutate(Guests=Party_Size-1)
      # make guest list
        # make guest list as null
        guest_list <- NULL
        # loop through building list
      for(i_guestlist in 1:nrow(with_guests)){
        number_of_guests <- with_guests[i_guestlist,]
        i_guestcount <- number_of_guests[,"Guests"]
        for(i_makeguests in 1:i_guestcount){
          temp <- data.frame(
            First = number_of_guests["First"],
            Last = paste0(
              number_of_guests["Last"],
              "'s guest"),
            Party_Size = NA,
            stringsAsFactors = FALSE
          )
          guest_list <- rbind(
            guest_list,temp
          )
        }
      }
    # append guests to main data
    input_raw <- rbind(
      input_raw,guest_list
    )
    # reorder
    input_raw <- input_raw %>%
      mutate(Name=paste0(
        First," ",Last)
      ) %>%
      arrange(Name) %>%
      select(Name)
  })
  
  output$input_raw <- renderTable({
    input_cleaned()
  })
  
  # download results (google form)
  output$downloadGoogle <- downloadHandler(
    filename = function() { 
      "listforgoogleform.csv" 
    },
    content = function(file) {
      write.csv(input_cleaned(), file)
    }
  )
  
#######################################################################  
## Data sold tickets          ##
################################  
  
  # load sold tickets data
  data_sold <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    data_sold <- input$tickets_sold
    
    if (is.null(data_sold))
      return(NULL)
    
    read.csv(
      data_sold$datapath, 
      header=TRUE, 
      sep=',', 
      quote='',
      stringsAsFactors=F,
      na.strings = "")
  })
  
  output$data_sold <- renderTable({
    head(data_sold())
  })
  
#######################################################################  
## Data requests              ##
################################
  
  data_requests <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    data_requests <- input$ticket_requests
    
    if (is.null(data_requests))
      return(NULL)
    
    read.csv(
      data_requests$datapath, 
      header=TRUE, 
      sep=',', 
      quote='"',
      stringsAsFactors=F,
      na.strings = "")
  })
  
  output$data_requests <- renderTable({
    head(data_requests())
  })
  
#######################################################################  
## Fellows coming             ##
################################
 
  hightable_fellows <- reactive({
    hightable_fellows <- input$hightable_fellows
  })  
  
  secondhightable_fellows <- reactive({
    secondhightable_fellows <- input$secondhightable_fellows
  }) 
  
################################
## Seats in hall              ##
################################
  hall_plan_upload <- reactive({
    hall_plan <- input$hall_plan
    
    if (is.null(hall_plan))
      return(NULL)
    
    read.csv(
      hall_plan$datapath, 
      header=TRUE, 
      sep=',', 
      quote='"',
      stringsAsFactors=F,
      na.strings = "")
  })
  
  # hall plan
  hall_plan <- reactive({
    # if none uploaded, use default
    hall_plan <- hall_plan_upload()
    if (is.null(hall_plan)) {
      hightable <- 18
      lowerhightable <- 18
      
      north1 <- 8
      north2 <- 8
      north3 <- 8
      north4 <- 6
      north5 <- 6
      
      south1 <- 8
      south2 <- 8
      south3 <- 8
      south4 <- 6
      south5 <- 6
      
      centretable <- 36
      
      # Make vector of tables
      allseats <- c(
        rep("hightable",hightable),
        rep("lowerhightable",lowerhightable),
        rep("north1",north1),
        rep("north2",north2),
        rep("north3",north3),
        rep("north4",north4),
        rep("north5",north5),
        rep("south1",south1),
        rep("south2",south2),
        rep("south3",south3),
        rep("south4",south4),
        rep("south5",south5),
        rep("centretable",centretable)
      )
    }
    if (!is.null(hall_plan)) {
      print(hall_plan)
      allseats <- NULL
      for(i in 1:nrow(hall_plan)){
        allseats <- c(
          allseats,
          rep(hall_plan[i,1],hall_plan[i,2])
        )
      }
    }
    allseats
  })
   
#######################################################################  
## Now the matching           ##
################################
  
  list_output <- reactive({
    # Fellows (reserved seats)
    fellow_HT <- hightable_fellows()
    fellow_2HT <- secondhightable_fellows()
    
  allseats <- hall_plan()
    
    ################################
    ## Load in and clean data     ##
    ################################
    

    data_sold <- data_sold()
    
    data_request <- data_requests()
    
    if (is.null(data_sold))
      return(NULL)
    
    if (is.null(data_request))
      return(NULL)
    
    # Create groups
    t_names <- NULL
    t_group <- NULL
    for(i in 1:nrow(data_request)){
      t_currgroup <- unlist(
        strsplit(data_request[i,],", ")
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
                             t_group=max(x$t_group)
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
      setdiff(c(na.omit(data_sold$Name),na.omit(data_sold$Guest.name)),
              groups$t_names
      )  
    # Double check people not in a group aren't with a guest
    notingroup
    
    ## Put not in group with guests in as groups of two
    # last group
    last_group <- as.numeric(max(groups$t_group))+1
    # 
    notingroup_guests <- notingroup[grep("'s guest", notingroup)]
    notingroup_hosts <- unlist(strsplit(notingroup_guests, 
                                        split="'s guest", 
                                        fixed=TRUE))
    notingroup_withguest <- c(notingroup_hosts,notingroup_guests)
    # in right order
    notingroup_withguest <- notingroup_withguest[order(notingroup_withguest)]
    # as data frame
    notingroup_withguest <- data.frame(t_names=notingroup_withguest,
                                       t_group=rep(
                                         last_group:((length(notingroup_withguest)/2)+last_group-1),
                                         each=2),
                                       sum=rep(2,length(notingroup_withguest)))
    
    #### ADD TO GROUPS
    groups <- rbind(groups,notingroup_withguest)
    
    ## Not in group - and not with guest!
    notingroup <- notingroup[!(notingroup %in% notingroup_withguest$t_names)]
    
    
    
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
      allseats$guest[i] <- "FELLOW"
    }
    
      # 
      hightable <- allseats %>% filter(allseats=="hightable") %>% nrow
    for(i in 1:fellow_2HT){
      allseats$guest[hightable+i] <- "FELLOW"
    }
    
    # count free spaces
    allseats$free <- ifelse(is.na(allseats$guest),
                            1,0)
    allseats$total_free <-  ave(allseats$free, allseats$allseats, 
                                FUN=sum)
    
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
      na.omit(data_sold$Name),
      na.omit(data_sold$Guest.name))
    
    numberPeople <- as.numeric(length(fulllist))
    numberJesuans <- as.numeric(nrow(data_sold))
    numberSeated <- length(na.omit(allseats$guest))
    numberFellows <- fellow_2HT+fellow_HT
    
    # build list
    
    list(
      numberPeople = as.numeric(length(fulllist)),
      numberJesuans = as.numeric(nrow(data_sold)),
      numberGuests = numberPeople-numberJesuans,
      numberSeated = length(na.omit(allseats$guest)),
      numberFellows = fellow_2HT+fellow_HT,
      allseats = subset(allseats, select=c(allseats,guest)),
      worked = ifelse((numberPeople+numberFellows)==numberSeated,
                      "Success!","Failure! some not in seats")
    )
  })
  
  output$list_output <- renderTable({
    list_output()$allseats
  })
  
  # download seating plan
  output$downloadPlan <- downloadHandler(
    filename = function() { 
      "seatingplan.csv" 
    },
    content = function(file) {
      write.csv(list_output()$allseats, file)
    }
  )
  
})
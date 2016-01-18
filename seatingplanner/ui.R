library(shiny)

shinyUI(navbarPage("Seating planner",
  #  Prepare data tab  ###############################
  tabPanel("Prepare for google form",
      # Prepare raw list of sold
      titlePanel("Get raw sold formatted"),
      sidebarLayout(
        sidebarPanel(
          p("This tab takes a csv with the raw sold ticket data, and turns it into a new csv that has each person, and their guest, as a seperate line."),
          p("Before running, make sure to remove the Manciple's tickets for fellows (i.e. this is the list of people that can be in table requests - so we don't want to count the fellows."),
          p("Some manual edits maybe needed, e.g. for Burns night, if the manciple brought tickets for the speakers, then you'll need to add the names manually to the csv when you delete the manciple's tickets from the list."),
          p("Load a csv with 3 columns (the order matters, not the column names):"),
          p(
            code("Forename"),", ",
            code("Surname"),", ",
            code("Party.Size")
            ),
          a("Example input here",target="_blank",href="http://r.epijim.uk/share/seatingplan/from_manciple.csv"),
          fileInput('input_raw', 'Raw tickets sold',
                    accept=c('text/csv', 
                             'text/comma-separated-values,text/plain', 
                             '.csv')),
          downloadButton('downloadGoogle', 'Download for google form')
        ),
        mainPanel(
          h5("List for google form"),
          tableOutput('input_raw')
        )
        ) # close sidebar
      ), # close tab
                   tabPanel("Seating algorithm",
  titlePanel("MCR seating plan maker"),
  sidebarLayout(
    sidebarPanel(
      p("Names in the table requests must match the tickets sold exactly (this can be achieved by copy and pasting the tickets sold as options into the google form)."),
      p("Some people will be in multiple tables - this algorithm will assign them to the first table they were requested to be in."),
      fileInput('tickets_sold', 'Tickets sold',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      p("Tickets sold should be a csv file, with  2 columns ",
        code("Name")," and ",code("Guest.name"), " an example file is ",
        a("here",target="_blank",href="http://r.epijim.uk/share/seatingplan/into_gform.csv")
      ),
      fileInput('ticket_requests', 'Table requests',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      p("Table requests sold should be a csv file, with  1 column ",
        code("Responses")," where each table is a single cell, people seperated by a comma. An example file is ",
        a("here",target="_blank",href="http://r.epijim.uk/share/seatingplan/from_gform.csv")
      ),
      numericInput("hightable_fellows", 
                   label = h6("Fellows on high table"), 
                   value = 1),
      numericInput("secondhightable_fellows", 
                   label = h6("Fellows on second high table"), 
                   value = 1),
      fileInput('hall_plan', 'csv with table setup',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      p("Uploading hall plan is optional - default assumes no balcony."),
      p("Two columns needed:",
        code("Table.Names"),code("Seats"),
        " where 1st column is the name of each table, and 2nd column is the number of seats at that table."),
      p("Hightable and 2nd hightable must be first and second table (as algo puts fellows on first two tables)"),
      p("Example hall plan is here (this one has balcony, and four rather than 6 on two side tables furthest from the hall:",
        a("here",target="_blank",href="http://r.epijim.uk/share/seatingplan/hallplan.csv")
      ),
      downloadButton('downloadPlan', 'Download seating plan'),
      tags$hr()
    ),
    mainPanel(
      h5("First 6 rows of the sold tickets"),
      tableOutput('data_sold'),
      h5("First 6 rows of the table requests"),
      tableOutput('data_requests'),
      h5("Seating plan"),
      tableOutput('list_output')
    )
  )
  ) # close tab

))
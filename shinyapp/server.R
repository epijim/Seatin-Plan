shinyServer(function(input, output) {
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    
    file_college <- input$file_college
    
    if (is.null(file_college))
      return(NULL)
    
    read.csv(file_college$datapath, header=input$header, sep=input$sep, quote=input$quote)
  })
})
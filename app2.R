library(shiny)
library(ggplot2)  # for the plot.
library(plyr) # for counting data.
library(RColorBrewer) # for collors in plot.

ui <- fluidPage(
  title = "Gff v3 uploader",
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose GFF version 3 File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Raw data", DT::dataTableOutput("mytable1")),
        tabPanel("Summary", verbatimTextOutput("mytable2")),
        tabPanel("Bar plot", plotOutput("mytable3")),
        tabPanel("Percentage bar plot", plotOutput("mytable4"))
      )
    )
  )
)

server <- function(input, output) {
  # Print a interactive datatabel.
  output$mytable1 <- DT::renderDataTable({
    validate(
      need(input$file1$datapath, 'Please upload first a gff v3 file.')
    )
    df <- read.delim(input$file1$datapath, header = T, col.names = c("Sequence", "Source", "Feature", "Start", "End", "Score", "Strand", "Phase", "Attributes"), comment.char="#")
    DT::datatable(df)
    return(df)
  })

  # Print summary of gff file.
  output$mytable2 <- renderPrint({
    summary(df)
  })
  
  # Print a plot.
  output$mytable3 <- renderPlot({
    ct <- count(df$Source)
    tot <- sum(ct$freq)
    prop <- sprintf("%.1f%%", ct$freq / tot * 100)
    
    plot <- ggplot(ct, aes(x = ct$x, y = ct$freq)) +
      geom_bar(aes(label = ct$freq), stat = "identity")+
      geom_text(aes(label = prop), vjust = -0.3, size = 3.5)
    
    plot <- plot + labs(x = "Source names",
                        y = "Number of gff entries",
                        title = "Plot of all the different sources"
    )+
      theme(
        plot.title = element_text(hjust = 0.5, size = 14),    # Center title position and size
        plot.subtitle = element_text(hjust = 0.5),            # Center subtitle
        plot.caption = element_text(hjust = 0, face = "italic"), # move caption to the left
        legend.position = "none"
      )
    print(plot)
  })
  
  # Print a plot.
  output$mytable4 <- renderPlot({
    ct <- count(df$Source)
    
    plot2 <- ggplot(ct, aes(fill=ct$x, y=ct$freq, x="test")) + 
      geom_bar(position="fill", stat="identity")
    
    print(plot2 + scale_fill_brewer(palette="Set1"))
  })
  
}

shinyApp(ui, server)


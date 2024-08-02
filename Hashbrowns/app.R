#Hashbrowns App
#A web application for making cryptographic hashes
#Mark Reuter
#August 2024
#contact mark.reuter@googlemail.com
#git https://github.com/drmarkreuter/hashbrowns/tree/main

library(shiny)
library(digest) #required for hashing
library(DT) #required for table output

hashbrown <- function(string,algo) {
  hash = digest(string,algo = algo,
                errormode = "warn")
  return(hash)
}

hashbrownPrefix <- function(prefix,string,algo) {
  hash = paste0(
    prefix,
    digest(string,algo = algo,
           errormode = "warn")
  )
  return(hash)
}

rainbow <- function(string,algo) {
  hash = digest(string,algo = algo,
                errormode = "warn")
  rainbow.df <- data.frame(string=as.character(),
                           hash=as.character(),
                           algo=as.character(),
                           stringsAsFactors = FALSE)
  rainbow.df[1,1] <- string
  rainbow.df[1,2] <- hash
  rainbow.df[1,3] <- algo
  
  return(rainbow.df)
}

rainbowPrefix <- function(prefix,string,algo) {
  hash = digest(string,algo = algo,
                errormode = "warn")
  rainbow.df <- data.frame(
    prefix=as.character(),
    string=as.character(),
    hash=as.character(),
    uri=as.character(),
    algo=as.character(),
    stringsAsFactors = FALSE)
  
  rainbow.df[1,1] <- prefix
  rainbow.df[1,2] <- string
  rainbow.df[1,3] <- hash
  rainbow.df[1,4] <- paste0(
    prefix,
    hash
  )
  rainbow.df[1,5] <- algo
  
  return(rainbow.df)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Hashbrowns"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          #radio buttons here
          radioButtons("hashType",
                       "Hash Type",
                       c("md5" = "md5",
                         "sha1" = "sha1",
                         "sha256" = "sha256",
                         "sha512" = "sha512")
          ),
          textInput("inputString",
                    "string to hash",
                    value = "elephant and castle",
                    width = NULL,
                    placeholder = NULL),
          actionButton("hashGo",
                       "Create",
                       width = 150),
          
          hr(),
          
          fileInput(
            inputId = "loadText",
            label = "Upload text file",
            multiple = FALSE,
            accept = ".txt",
            width = 250,
            placeholder = ""),
          actionButton("hashFileGo",
                       "Create",
                       width = 150),
          
          hr(),
        ),

        
        mainPanel(
           #output here
          uiOutput("inputString"),
          uiOutput("simpleHash"),
          uiOutput("rainbowTitle"),
          dataTableOutput("rainbow"),
          hr(),
          uiOutput("inputStringsFile"),
          uiOutput("simpleHashsFile"),
          uiOutput("rainbowTitleFile"),
          dataTableOutput("rainbowFile"),
          downloadButton("downloadRainbow",
                         label = "Download Rainbow table"),
          hr()
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$hashGo,{
    stringInput <- input$inputString
    print(stringInput)
    algo <- input$hashType
    print(algo)
    hash <- hashbrown(stringInput,algo)
    
    hash.df <- rainbow(stringInput,algo)
    
    output$inputString <- renderUI({
      HTML(paste0('<h4>',
                  stringInput,
                  '</h4>'))
    })
    
    output$simpleHash <- renderUI({
      HTML(paste0('<h3>',
                  hash,
                  '</h3>'))
    })
    
    output$rainbow <- renderDT({
      hash.df
    })
  
    
  })
  
  output$rainbowTitle <- renderUI({
    HTML(paste0('<h2>',
                "Rainbow Table",
                '</h2>'))
  })
  
  output$rainbowTitleFile <- renderUI({
    HTML(paste0('<h2>',
                "Rainbow Table (from file)",
                '</h2>'))
  })
  
  observeEvent(input$hashFileGo,{
    stringVec <- vector()
    hashVec <- vector()
    algo <- input$hashType
    print(algo)
    
    # print(class(input$loadText))
    # print(input$loadText)
    uploadPath <- input$loadText$datapath
    # print(uploadPath)
    uploadContent <- read.csv(uploadPath,header = FALSE)
    print(uploadContent)
    for (i in 1:nrow(uploadContent)){
      stringVec[i] <- uploadContent[i,1]
      hashVec[i] <- hashbrown(uploadContent[i,1],algo)
    }
    print(stringVec)
    print(hashVec)
    
    fileHashDf <- data.frame(string=as.character(),
                             hash=as.character(),
                             algo=as.character(),
                             stringsAsFactors = FALSE)
    for (i in 1:nrow(uploadContent)){
      tempDf <- rainbow(uploadContent[i,1],algo)
      fileHashDf <- rbind(fileHashDf,
                          tempDf)
    }

    output$inputStringsFile <- renderUI({
      HTML(paste0('<h5>',
                  stringVec,
                  '</h5>'))
    })
    
    output$simpleHashsFile <- renderUI({
      HTML(paste0('<h4>',
                  hashVec,
                  '</h4>'))
    })

    output$rainbowFile <- renderDT({
      fileHashDf
    })
    
    ##download handler
    output$downloadRainbow <- downloadHandler(
      filename = function() {
        paste0('rainbow_',Sys.Date(),".csv")
      },
      content = function(file) {
        write.csv(fileHashDf,
                  file,
                  row.names = FALSE)
      }
    )
    
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

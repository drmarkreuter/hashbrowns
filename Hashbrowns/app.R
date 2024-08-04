#Hashbrowns App
#A web application for making cryptographic hashes
#Mark Reuter
#August 2024
#contact mark.reuter@googlemail.com
#git https://github.com/drmarkreuter/hashbrowns/tree/main

library(shiny)
library(shinythemes)
library(digest) #required for hashing
library(DT) #required for table output

#### functions ####
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

##function to return random string of letters + numbers
randString <- function(){
  char.vec <- c(LETTERS,letters,seq(0,9,1))
  r <- paste0(sample(char.vec,
                     16,
                     replace = FALSE),
              collapse = "")
  return(r)
}

#### UI ####
ui <- fluidPage(theme = shinytheme("cyborg"),
  
  titlePanel("Hashbrowns"),
  
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
      textInput("prefixURI",
                "URI prefix",
                value = "https://example.com/",
                width = NULL,
                placeholder = NULL),
      actionButton("hashPrefixGo",
                   "URI + Hash",
                   width = 150),
      
      actionButton("hashFilePrefixGo",
                   "URI + Hash (from file)",
                   width = 200),
      hr(),
      actionButton("randomHash",
                   "Random hash",
                   width = 200),
      hr()
    ),
    
    #### main panel ####
    mainPanel(
    
      uiOutput("hash"),
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
      hr(),
      uiOutput("inputStringPrefix"),
      uiOutput("simpleHashPrefix"),
      dataTableOutput("rainbowPrefixTable"),
      
      hr(),
      uiOutput("inputStringsPrefixFile"),
      uiOutput("simpleHashsPrefixFile"),
      dataTableOutput("rainbowPrefixFile"),
      downloadButton("downloadPrefixRainbow",
                     label = "Download Prefix Rainbow table"),
      hr(),
      uiOutput("randomTitle"),
      uiOutput("randomHash"),
      hr()
      
      
    )
  )
)

#### server ####
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
  
  output$hash <- renderUI({
    HTML(paste0('<h2>',
                "Hash",
                '</h2>'))
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
  
  ##URI prefix
  observeEvent(input$hashPrefixGo,{
    stringInput <- input$inputString
    print(stringInput)
    algo <- input$hashType
    print(algo)
    prefix <- input$prefixURI
    print(prefix)
    prefixhash <- hashbrownPrefix(prefix,stringInput,algo)
    print(prefixhash)
    
    prefixhash.df <- rainbowPrefix(prefix,stringInput,algo)
    print(prefixhash.df)
    
    output$inputStringPrefix <- renderUI({
      HTML(paste0('<h4>',
                  paste0(prefix,URLencode(stringInput)),
                  '</h4>'))
    })
    
    output$simpleHashPrefix <- renderUI({
      HTML(paste0('<h3>',
                  prefixhash,
                  '</h3>'))
    })
    
    output$rainbowPrefixTable <- renderDT({
      prefixhash.df
    })
    
  })
  
  ##file + prefix
  observeEvent(input$hashFilePrefixGo,{
    stringPrefixVec <- vector()
    hashPrefixVec <- vector()
    prefix <- input$prefixURI
    algo <- input$hashType
    print(algo)
    
    uploadPath <- input$loadText$datapath
    
    uploadContent <- read.csv(uploadPath,header = FALSE)
    print(uploadContent)
    for (i in 1:nrow(uploadContent)){
      stringPrefixVec[i] <- paste0(prefix,
                                   URLencode(uploadContent[i,1])
                                   )
      hashPrefixVec[i] <- hashbrownPrefix(prefix,uploadContent[i,1],algo)
    }
    print(stringPrefixVec)
    print(hashPrefixVec)
    
    filehashPrefixDf <- data.frame(
      prefix=as.character(),
      string=as.character(),
      hash=as.character(),
      uri=as.character(),
      algo=as.character(),
      stringsAsFactors = FALSE)
      
    for (i in 1:nrow(uploadContent)){
      tempPrefixDf <- rainbowPrefix(prefix,uploadContent[i,1],algo)
      filehashPrefixDf <- rbind(filehashPrefixDf,
                                tempPrefixDf)
    }
    
    output$inputStringsPrefixFile <- renderUI({
      HTML(paste0('<h5>',
                  stringPrefixVec,
                  '</h5>'))
    })
    
    output$simpleHashsPrefixFile <- renderUI({
      HTML(paste0('<h4>',
                  hashPrefixVec,
                  '</h4>'))
    })
    
    output$rainbowPrefixFile <- renderDT({
      filehashPrefixDf
    })
    
    ##download handler
    output$downloadPrefixRainbow <- downloadHandler(
      filename = function() {
        paste0('rainbow_prefix_',Sys.Date(),".csv")
      },
      content = function(file) {
        write.csv(filehashPrefixDf,
                  file,
                  row.names = FALSE)
      }
    )
    
  })
  
  output$randomTitle <- renderUI({
    HTML(paste0('<h2>',
                "Random Hash",
                '</h2>'))
  })
  
  observeEvent(input$randomHash,{
    inputString <- randString()
    outputHash <- hashbrown(inputString,input$hashType)
    
    output$randomHash <- renderUI(
      HTML(paste0('<h4>',
                  inputString,
                  "<br>",
                  outputHash,
                  '</h4>')
           )
    )
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

##https://abominable.shinyapps.io/Hashbrowns/

##multiverse internet
##to do - salt and pepper



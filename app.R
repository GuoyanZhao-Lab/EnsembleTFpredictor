# Load packages ----
library(shiny)
source("MultiRank.R")

# Source 

# User interface ----
ui <- fluidPage(
  titlePanel("EnsembleTFPredictor: Multi-Method Rankings Generator"),
  
  helpText("Select tools to upload results from."),

  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "tools",
        "Methods Used:",
        c("MORA", "HOMER", "AME", "PSCAN", "BART2", "LISA2")
        
      ),
      
     # Input: Select a file ----
      fileInput("MORA", "Upload MORA Results",
                multiple = FALSE,
                accept = ".csv"),
     actionButton("resetM", "Reset Input"),
     
      fileInput("HOMER", "Upload HOMER Results",
                multiple = FALSE,
                accept = ".txt"),
     actionButton("resetH", "Reset Input"),
     
     fileInput("AME", "Upload AME Results",
               multiple = FALSE,
               accept = ".tsv"),
     actionButton("resetP", "Reset Input"),
     
      fileInput("PSCAN", "Upload PSCAN Results",
                multiple = FALSE,
                accept = ".tsv"),
     actionButton("resetP", "Reset Input"),
     
      fileInput("BART2", "Upload BART2 Results",
                multiple = FALSE,
                accept = ".txt"),
     actionButton("resetB", "Reset Input"),
     
      fileInput("LISA2", "Upload LISA2 Results",
                multiple = FALSE,
                accept = ".tsv"),
     actionButton("resetL", "Reset Input"),



      helpText("After uploading results from all desired tools, submit all results using button below"),
      
    actionButton("button", "Submit All Results"),
    
    helpText("Confirm desired results were uploaded correctly and click button below to generate multi-method rank table"),
    
    actionButton("generate", "Generate Multi-Method Rank Table")
    
    
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      
      tableOutput("contents"),
      tableOutput("contents2"),
      downloadButton("downloadData", "Download Full Table CSV File")
      
  )
)
)

# Server logic
# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  
  
  
  values_MORA <- reactiveValues(
    upload_state_MORA = NULL
  )
  
  observeEvent(input$MORA, {
    values_MORA$upload_state_MORA <- 'uploaded'
  })
  
  observeEvent(input$resetM, {
    values_MORA$upload_state_MORA <- 'reset'
  })
  
  file_input_MORA <- reactive({
    if (is.null(values_MORA$upload_state_MORA)) {
      return(NULL)
    } else if (values_MORA$upload_state_MORA == 'uploaded') {
      return(input$MORA)
    } else if (values_MORA$upload_state_MORA == 'reset') {
      return(NULL)
    }
  })
  
  
  values_HOMER <- reactiveValues(
    upload_state_HOMER = NULL
  )
  
  observeEvent(input$HOMER, {
    values_HOMER$upload_state_HOMER <- 'uploaded'
  })
  
  observeEvent(input$resetH, {
    values_HOMER$upload_state_HOMER <- 'reset'
  })
  
  file_input_HOMER <- reactive({
    if (is.null(values_HOMER$upload_state_HOMER)) {
      return(NULL)
    } else if (values_HOMER$upload_state_HOMER == 'uploaded') {
      return(input$HOMER)
    } else if (values_HOMER$upload_state_HOMER == 'reset') {
      return(NULL)
    }
  })
  
  
  
  
  
  values_PSCAN <- reactiveValues(
    upload_state_PSCAN = NULL
  )
  
  observeEvent(input$PSCAN, {
    values_PSCAN$upload_state_PSCAN <- 'uploaded'
  })
  
  observeEvent(input$resetP, {
    values_PSCAN$upload_state_PSCAN <- 'reset'
  })
  
  file_input_PSCAN <- reactive({
    if (is.null(values_PSCAN$upload_state_PSCAN)) {
      return(NULL)
    } else if (values_PSCAN$upload_state_PSCAN == 'uploaded') {
      return(input$PSCAN)
    } else if (values_PSCAN$upload_state_PSCAN == 'reset') {
      return(NULL)
    }
  })
  
  
  values_BART2 <- reactiveValues(
    upload_state_BART2 = NULL
  )
  
  observeEvent(input$BART2, {
    values_BART2$upload_state_BART2 <- 'uploaded'
  })
  
  observeEvent(input$resetB, {
    values_BART2$upload_state_BART2 <- 'reset'
  })
  
  file_input_BART2 <- reactive({
    if (is.null(values_BART2$upload_state_BART2)) {
      return(NULL)
    } else if (values_BART2$upload_state_BART2 == 'uploaded') {
      return(input$BART2)
    } else if (values_BART2$upload_state_BART2 == 'reset') {
      return(NULL)
    }
  })
  
  values_AME <- reactiveValues(
    upload_state_AME = NULL
  )
  
  observeEvent(input$AME, {
    values_AME$upload_state_AME <- 'uploaded'
  })
  
  observeEvent(input$resetB, {
    values_AME$upload_state_AME <- 'reset'
  })
  
  file_input_AME <- reactive({
    if (is.null(values_AME$upload_state_AME)) {
      return(NULL)
    } else if (values_AME$upload_state_AME == 'uploaded') {
      return(input$AME)
    } else if (values_AME$upload_state_AME == 'reset') {
      return(NULL)
    }
  })
  
  values_LISA2 <- reactiveValues(
    upload_state_LISA2 = NULL
  )
  
  observeEvent(input$LISA2, {
    values_LISA2$upload_state_LISA2 <- 'uploaded'
  })
  
  observeEvent(input$resetL, {
    values_LISA2$upload_state_LISA2 <- 'reset'
  })
  
  file_input_LISA2 <- reactive({
    if (is.null(values_LISA2$upload_state_LISA2)) {
      return(NULL)
    } else if (values_LISA2$upload_state_LISA2 == 'uploaded') {
      return(input$LISA2)
    } else if (values_LISA2$upload_state_LISA2 == 'reset') {
      return(NULL)
    }
  })
  
  
  output$contents <- renderTable({
    
    
    req(input$button)
    file_names_table <- data.frame()
    
    
    if ("MORA" %in% input$tools){
      file_name_MORA <- input$MORA
      file_name_MORA$tool <- "MORA"
      file_name_MORA <- file_name_MORA[c("name", "tool")]
      file_names_table <- rbind(file_names_table, file_name_MORA)
    }
    
    
    if ("HOMER" %in% input$tools){
      file_name_HOMER <- input$HOMER
      file_name_HOMER$tool <- "HOMER"
      file_name_HOMER <- file_name_HOMER[c("name", "tool")]
      file_names_table <- rbind(file_names_table, file_name_HOMER)
    }
    
    
    if ("AME" %in% input$tools){
      file_name_AME <- input$AME
      file_name_AME$tool <- "AME"
      file_name_AME <- file_name_AME[c("name", "tool")]
      file_names_table <- rbind(file_names_table, file_name_AME)
    }
    
    if ("PSCAN" %in% input$tools){
      file_name_PSCAN <- input$PSCAN
      file_name_PSCAN$tool <- "PSCAN"
      file_name_PSCAN <- file_name_PSCAN[c("name", "tool")]
      file_names_table <- rbind(file_names_table, file_name_PSCAN)
    }
    
    if ("BART2" %in% input$tools){
      file_name_BART2 <- input$BART2
      file_name_BART2$tool <- "BART2"
      file_name_BART2 <- file_name_BART2[c("name", "tool")]
      file_names_table <- rbind(file_names_table, file_name_BART2)
    }
    
    if ("LISA2" %in% input$tools){
      file_name_LISA2 <- input$LISA2
      file_name_LISA2$tool <- "LISA2"
      file_name_LISA2 <- file_name_LISA2[c("name", "tool")]
      file_names_table <- rbind(file_names_table, file_name_LISA2)
    }
    
    return(file_names_table)
    
    
  })
  
  MORA <- data.frame(matrix(ncol=2))
  colnames(MORA) <- c("MORA", "DBID")
  HOMER <- data.frame(matrix(ncol=2))
  colnames(HOMER) <- c("HOMER", "DBID")
  AME <- data.frame(matrix(ncol=2))
  colnames(AME) <- c("AME", "DBID")
  PSCAN <- data.frame(matrix(ncol=2))
  colnames(PSCAN) <- c("PSCAN", "DBID")
  BART2 <- data.frame(matrix(ncol=2))
  colnames(BART2) <- c("BART2", "DBID")
  LISA2 <- data.frame(matrix(ncol=2))
  colnames(LISA2) <- c("LISA2", "DBID")
  
  final_table <- reactive({
    
    
    req(input$button)
  
  CISBP_db <- read.csv("data/CISBP_v2.00_HumanMouseRatCombined_QCed_DB_Final_Motif_IC_score_info.csv")
  CISBP_db_clean <- CISBP_db[!apply(CISBP_db == "", 1, all),]
  CISBP_db_clean$TF_Name <- toupper(CISBP_db_clean$TF_Name)
  CISBP_db_clean <- CISBP_db_clean[c("TF_Name", "DBID")]
  CISBP_db_clean <- unique(CISBP_db_clean)
  
  
  
  if ("MORA" %in% input$tools){
    df_MORA <- Read_MORA(file_input_MORA()$datapath)
    MORA_final <- dplyr::left_join(df_MORA, CISBP_db_clean, by = c("MORA" = "TF_Name"))
    MORA_final <- MORA_final[c("MORA", "DBID")]
    MORA <- rbind(MORA_final, MORA)
    
  }
  
  if ("AME" %in% input$tools){
    df_AME <- Read_AME(file_input_AME()$datapath)
    AME_final <- dplyr::left_join(df_AME, CISBP_db_clean, by = c("AME" = "TF_Name"))
    AME_final <- AME_final[c("AME", "DBID")]
    AME <- rbind(AME_final, AME)
    
  }
  
  if ("HOMER" %in% input$tools){
    HOMERMasterTable <- readRDS("data/HOMER_master_table.Rds")
    df_HOMER <- read.delim(file_input_HOMER()$datapath)
    df_HOMER$HomerID <- df_HOMER$Motif.Name
    df_HOMER$TF_Name <- sapply(strsplit(df_HOMER$HomerID, "\\("), function(x){x[1]})
    df_HOMER$TF_Name <- toupper(df_HOMER$TF_Name)
    Homer_CISBP_ref_table_final <- read.csv("data/Homer_DB_motifs_TF_With_CISBP_info_merged_FinalRefTable.csv")
    df_HOMER_clean <- merge(df_HOMER, Homer_CISBP_ref_table_final, by = c("HomerID", "TF_Name"), all = FALSE)
    HOMER_final <-as.data.frame(sapply(df_HOMER_clean, toupper),stringsAsFactors=FALSE)
    HOMER_final <- HOMER_final["TF_Name"]
    colnames(HOMER_final)[1]<- "HOMER"
    HOMER_final <- dplyr::left_join(HOMER_final,HOMERMasterTable, by = c("HOMER"="TF_Name"))
    HOMER_final <- HOMER_final[c("HOMER", "DBID")]
    HOMER <- rbind(HOMER_final, HOMER)
    
  }
  

  
  if ("PSCAN" %in% input$tools){
    df_PSCAN <- Read_PSCAN(file_input_PSCAN()$datapath)
    PSCAN_final <- dplyr::left_join(df_PSCAN, CISBP_db_clean, by = c("PSCAN" = "TF_Name"))
    PSCAN <- rbind(PSCAN_final, PSCAN)
  }
  
  if ("BART2" %in% input$tools){
    df_BART2 <- Read_BART2(file_input_BART2()$datapath)
    df_BART2 <- df_BART2["BART2"]
    BART2_final <- dplyr::left_join(df_BART2, CISBP_db_clean, by = c("BART2" = "TF_Name"))
    BART2 <- rbind(BART2_final, BART2)
    
  }
  
  if ("LISA2" %in% input$tools){
    df_LISA2 <- Read_LISA2(file_input_LISA2()$datapath)
    df_LISA2 <- df_LISA2["LISA2"]
    LISA2_final <- dplyr::left_join(df_LISA2, CISBP_db_clean, by = c("LISA2" = "TF_Name"))
    LISA2 <- rbind(LISA2_final, LISA2)
    
  }
  
  
  List_results <- list(MORA, HOMER, AME, PSCAN, BART2, LISA2)
  final <- RankTF(List_results, OutputFile = "data/final.csv")
  final <- final[, colSums(final != 0) > 0]
  return(final)
 
  
    })
  
  output$contents2 <- renderTable({
    req(input$generate)
    final_trunc <- final_table()
    final_trunc <- final_trunc[final_trunc$Num_Yes >= 2,]
    
    
    
    return(final_trunc)
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('EnsembleTFPredictor', Sys.Date(), 'final_table.csv', sep='_')
    },
    content = function(con) {
      write.csv(final_table(), con)
    }
  )

  
}
# Run the app ----
shinyApp(ui, server)
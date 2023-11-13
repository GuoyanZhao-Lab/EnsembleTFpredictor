#Load libraries
library(stringr)
library(tidyverse)
library(dplyr)
library(purrr)
#Create function for each method

## Function to read MORA result file
Read_MORA <- function(InputFile) {
  
  MORA <- read.csv(InputFile)
  MORA <-as.data.frame(sapply(MORA, toupper),stringsAsFactors=FALSE)
  MORA <- MORA[c("TF_Name", "Family_Name")]
  MORA$TF_Name <- toupper(MORA$TF_Name)
  colnames(MORA)[1]<- "MORA"
  return(MORA)
}


## Function to read AME result file
Read_AME <- function(InputFile) {
  
  AME <- read.delim(InputFile)
  AME <- as.data.frame(sapply(AME, toupper),stringsAsFactors=FALSE)
  AME$adj_p.value <- as.numeric(AME$adj_p.value)
  AME <- AME[AME$adj_p.value <= 0.05,]
  AME$TF_Name <- toupper(AME$motif_alt_ID)
  AME$TF_Name <- sapply(strsplit(AME$TF_Name, "\\)"), function(x){x[1]})
  AME$TF_Name <- str_replace_all(AME$TF_Name, "\\(", "")
  AME <- AME[c("TF_Name")]
  colnames(AME)[1]<- "AME"
  return(AME)
}


## Function to read HOMER result file
Read_HOMER <- function(InputFile) {
  HOMERMasterTable <- readRDS("data/HOMER_master_table.Rds")
  
  HOMER <- read.csv(InputFile)
  HOMER <-as.data.frame(sapply(HOMER, toupper),stringsAsFactors=FALSE)
  
  HOMER <- HOMER["TF_Name"]
  colnames(HOMER)[1]<- "HOMER"
  HOMER <- dplyr::left_join(HOMER,HOMERMasterTable, by = c("HOMER"="TF_Name"))
  return(HOMER)
}


## Function to read PSCAN result file
Read_PSCAN <- function(InputFile) {
  PSCANMasterTable <- readRDS("data/PSCAN_master_table.Rds")
  
  PSCAN <- read.delim(InputFile)
  PSCAN <-as.data.frame(sapply(PSCAN, toupper),stringsAsFactors=FALSE)
  PSCAN$P_VALUE <- as.numeric(PSCAN$P_VALUE)
  PSCAN <- PSCAN[PSCAN$P_VALUE <= 0.05,]
  PSCAN <- PSCAN["MATRIX_ID"]
  PSCAN <- merge(PSCAN,PSCANMasterTable, by = "MATRIX_ID")
  PSCAN <- PSCAN["TF_NAME"]
  colnames(PSCAN)[1]<- "PSCAN"
  return(PSCAN)
}


## Function to read BART2 result file
Read_BART2 <- function(InputFile) {
  BART2MasterTable <- readRDS("data/BART2_master_table.Rds")
  
  BART2 <- read.delim(InputFile)
  BART2 <-as.data.frame(sapply(BART2, toupper),stringsAsFactors=FALSE)
  BART2$irwin_hall_pvalue <- as.numeric(BART2$irwin_hall_pvalue)
  BART2 <- BART2[BART2$irwin_hall_pvalue <= 0.05,]
  BART2 <- BART2["TF"]
  colnames(BART2)[1]<- "BART2"
  BART2 <- dplyr::left_join(BART2,BART2MasterTable, by = c("BART2"="TF_Name"), relationship = "many-to-many")
  return(BART2)
}


## Function to read LISA2 result file
Read_LISA2 <- function(InputFile) {
  LISA2MasterTable <- readRDS("data/LISA2_master_table.Rds")
  
  LISA2 <- read.delim(InputFile)
  LISA2 <-as.data.frame(sapply(LISA2, toupper),stringsAsFactors=FALSE)
  LISA2$summary_p_value <- as.numeric(LISA2$summary_p_value)
  LISA2 <- LISA2[LISA2$summary_p_value <= 0.05,]
  LISA2 <- LISA2["factor"]
  colnames(LISA2)[1]<- "LISA2"
  LISA2 <- dplyr::left_join(LISA2,LISA2MasterTable, by = c("LISA2"="factor"), relationship = "many-to-many")
  return(LISA2)
}



# Function to integrate results from multiple tools and rank TFs
RankTF <- function(List_results) {

  
  MethodNames <- names(List_results)
  MethodNames <- unlist(lapply(List_results, function(x)  colnames(x)[[1]]))
  # ID refers ENSEMBLE ID if has one or other IDs
  new <- List_results %>% reduce(full_join, by = "DBID")
  new <- dplyr::left_join(new, CISBP_db_clean[,c(1,2)], by="DBID")
  new <-  new %>% mutate_at(MethodNames, ~replace(., !is.na(.), 1))
  new <-  new %>% mutate_at(MethodNames, ~replace(., is.na(.), 0))
  new <-  new %>% mutate_at(MethodNames, as.numeric)
  
  new2 <- new[order(new$TF_Name), ] #sort by TF_Name 
  # If a TF is predicted at least once then all values of this TF_Name will be change to 1 to signify predicted.
  new2 <- new2 %>%  dplyr::group_by(TF_Name) %>% mutate_at(MethodNames, max)
  new2 <-dplyr::distinct(new2,TF_Name,DBID, .keep_all = TRUE)
  new2 <- dplyr::group_by(new2,TF_Name) %>%
    mutate(DBID  = paste(DBID, collapse =","))
  new2 <-dplyr::distinct(new2,TF_Name,.keep_all = TRUE)
  new2$Num_Yes <- rowSums(new2[,MethodNames], na.rm=TRUE)
  new2 <- dplyr::arrange(new2,desc(Num_Yes))
  new2 <-dplyr::distinct(new2,TF_Name, .keep_all = TRUE)
  new2 <- new2[!is.na(new2$TF_Name),]
  new2$Rank <- rank(-new2$Num_Yes,ties.method= "min")
  Ranks <- new2$Rank
  Conf_lvl_ranks <- match(Ranks, sort(unique(Ranks)))
  new2$Conf_lvl_ranks <- Conf_lvl_ranks
  final_rank_table <- new2[c("TF_Name", "MORA", "HOMER", "AME", "PSCAN", "BART2", "LISA2", "Num_Yes", "Conf_lvl_ranks")]
  #write.csv(final_rank_table, file=OutputFile, row.names=FALSE)
  return(final_rank_table)
}



#Create master table of TF_Name and DBID from CISBP database

CISBP_db <- read.csv("data/CISBP_v2.00_HumanMouseRatCombined_QCed_DB_Final_Motif_IC_score_info.csv")
CISBP_db_clean <- CISBP_db[!apply(CISBP_db == "", 1, all),]
CISBP_db_clean$TF_Name <- toupper(CISBP_db_clean$TF_Name)
CISBP_db_clean <- CISBP_db_clean[c("TF_Name", "DBID")]
CISBP_db_clean <- unique(CISBP_db_clean)





















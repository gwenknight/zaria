## Shiny app for Empiric Prescribing calculations
# National level, with global data

# Required libraries
library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(rworldmap)
library(RColorBrewer)
library(reshape2)
library(DT)
library(readxl)  
library(tidyverse)
library(magrittr)
library("shinyMatrix")
library(tidytext)

###********************************************************************************************************************************************************************#####
#### REQUIRED DATA AND FUNCTIONS ####
###********************************************************************************************************************************************************************#####

drg_bkdwn <- read.csv("drug_bkdwn.csv", fileEncoding="UTF-8-BOM") # baseline empiric therapy recommendations
drg_bkdwn_group <- read.csv("drug_bkdwn_groups.csv", fileEncoding="UTF-8-BOM") # baseline empiric therapy recommendations with drug groupings instead of individual drugs
sp_bkdwn_lit <- read_csv("sp_bkdwn.csv") # baseline contributing bacteria distributions from literature
sp_bkdwn_zambia <- read_csv("sp_bkdwn_zambia.csv") # baseline contributing bacteria distributions from Zambia
sp_all <- read.csv("sp_all.csv", fileEncoding="UTF-8-BOM") # has just the list of species to include

# Zambian res and cost levels
res_levels_Zambia<-read_excel_allsheets("input_resistance_2207.xlsx")
cost_levels_Zambia<-read_excel_allsheets("antibiotics_cost.xlsx")

# Function to keep NAs if all NA
suma = function(x) if (all(is.na(x))) x[NA_integer_] else max(x, na.rm = TRUE)

# Remove all in column 
not_all_na <- function(x) any(!is.na(x))

# To read in excel data sheets
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheet <- readxl::excel_sheets(filename)
  
  data_frame = lapply(setNames(sheet, sheet), 
                      function(x) read_excel(filename, sheet=x, skip = 1))
  
  # attaching all dataframes together
  data_frame = bind_rows(data_frame, .id="Sheet")
  return(data_frame)
}



###********************************************************************************************************************************************************************#####
#### SHINY CODE ####
###********************************************************************************************************************************************************************#####
ui <- fluidPage(  
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  titlePanel("Empiric Prescribing ZAR:IA: Zambia focus"),
  
  sidebarPanel(h1("Inputs"),
               
               # Syndrome input
               selectInput("variable", "Choose syndrome:",
                           c("Sepsis" = "sep",
                             "Complicated UTI" = "c.uti",
                             "Hospital acquired pneumonia" = "hap",
                             #"Community acquired pneumonia" = "cap",
                             "Cellulitis / skin abcess" = "skin"#,
                             #"Purulent urthritis / cervicitis" = "puc",
                             #"Upper respiratory tract infection" = "urti",
                             # "Bacterial meningitis" = "meng",
                             #"Septic arthritis" = "sepa"
                           )),
               
               # # Age input
               # selectInput("age", "Age", 
               #             c("Adult" = "adult",
               #               "Child" = "child",
               #               "Neonate" = "infant")),
               
               hr(), 
               
               
               # Resistance cutoff input
               numericInput("res_cut", em("Resistance cutoff (%)"), 15, min = 0, max = 100),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Select a file ----
               fileInput("file_res", "Resistance distribution: Choose XLS File",
                         multiple = FALSE
                         # accept = c("text/csv",
                         #            "text/comma-separated-values,text/plain",
                         #            ".csv")
               ),
               
               # Copy the line below to make a checkbox
               checkboxInput("checkbox_file_res", label = "Or use Zambian data", value = FALSE),
               
               fileInput("file_cost", "Cost distribution: Choose XLS File",
                         multiple = FALSE
                         # accept = c("text/csv",
                         #            "text/comma-separated-values,text/plain",
                         #            ".csv")
               ),
               checkboxInput("checkbox_file_cost", label = "Or use Zambian costs", value = FALSE),
               
               hr(), 
               
               h4("Therapy options"),
               
               # Therapy input
               selectInput(inputId = 'drug1.1',
                           label = '1st: Choose a first line, first drug:',
                           choices = read.csv("data/antibiotics_avail.csv", header = FALSE)),
               
               # Therapy input
               selectInput(inputId = 'drug1.2',
                           label = 'Choose a first line, second drug (if required):',
                           choices = read.csv("data/antibiotics_avail.csv", header = FALSE)),
               
               # Therapy input
               selectInput(inputId = 'drug1.3',
                           label = 'Choose a first line, third drug (if required):',
                           choices = read.csv("data/antibiotics_avail.csv", header = FALSE)),
               
               hr(), 
               
               # Therapy input
               selectInput(inputId = 'drug2.1',
                           label = '2nd: Choose a second line, first drug:',
                           choices = read.csv("data/antibiotics_avail.csv", header = FALSE)),
               
               # Therapy input
               selectInput(inputId = 'drug2.2',
                           label = 'Choose a second line, second drug (if required):',
                           choices = read.csv("data/antibiotics_avail.csv", header = FALSE)),
               
               # Therapy input
               selectInput(inputId = 'drug2.3',
                           label = 'Choose a second line, third drug (if required):',
                           choices = read.csv("data/antibiotics_avail.csv", header = FALSE)),
               hr(), 
               
               # Therapy input
               selectInput(inputId = 'drug3.1',
                           label = '3rd: Choose a third line, first drug:',
                           choices = read.csv("data/antibiotics_avail.csv", header = FALSE)),
               
               # Therapy input
               selectInput(inputId = 'drug3.2',
                           label = 'Choose a third line, second drug (if required):',
                           choices = read.csv("data/antibiotics_avail.csv", header = FALSE)),
               
               # Therapy input
               selectInput(inputId = 'drug3.3',
                           label = 'Choose a third line, third drug (if required):',
                           choices = read.csv("data/antibiotics_avail.csv", header = FALSE)),
               
               hr(), 
               
               # Therapy input
               selectInput(inputId = 'drug4.1',
                           label = '4th: Choose a fourth line, first drug:',
                           choices = read.csv("data/antibiotics_avail.csv", header = FALSE)),
               
               # Therapy input
               selectInput(inputId = 'drug4.2',
                           label = 'Choose a fourth line, second drug (if required):',
                           choices = read.csv("data/antibiotics_avail.csv", header = FALSE)),
               
               # Therapy input
               selectInput(inputId = 'drug4.3',
                           label = 'Choose a fourth line, third drug (if required):',
                           choices = read.csv("data/antibiotics_avail.csv", header = FALSE)),
               
               hr(), 
               
               h4("Bacterial distribution inputs"),
               
               # Copy the line below to make a checkbox
               checkboxInput("checkbox", label = "Zambian data", value = FALSE),
               
               # Barchart of bug composition
               plotOutput("inputplot")
               
  ),
  
  mainPanel(
    
    # Output: Tabset w/ plot, summary, and table
    tabsetPanel(type = "tabs", 
                
                # Opening tab: Explainer
                tabPanel("Home page", 
                         h4("Empiric prescribing recommendation support"), 
                         p("This online dashboard aims to support decision 
                            making around empiric prescribing. It should not be used without clinical and
                           pharmacist training and support"),
                         p("This framework is designed to support first analysis of resistance data to 
                           act as a starting point for empiric prescribing tailored to local resistance
                           patterns at the level of infection syndromes. This app creates a weighted 
                  average resistance level at the syndrome level combining the bacterial aetiology with 
                  the proportion of each bacteria that are resistant to each antibiotic."),
                  p("For more information about the original concept please see our",
                    a("publication.", 
                      href = "https://wellcomeopenresearch.org/articles/4-140")),
                p("This version was designed and piloted with collaborators in Zambia. As such we have 
                collected local aetiology, resistance and costs data that users can use to explore
                the framework or users have the option to upload their own data. For more information on the 
                Zambian setting and the aims of this version of the app, please check out our preprint ",
                  a("here.", 
                    href = "https://wellcomeopenresearch.org/articles/4-140")),
                
                br(),
                h5("User instructions"),
                p("To use this dashboard, firstly work through the Inputs in the side panel"),
                p("(1) Choose the infection syndrome you wish to focus on."),
                p("(2) Choose the resistance cutoff (as a percentage) that would result in a change
                           away from this antibiotic to another"),
                p("(3) Resistance distribution: Users can choose to upload information from their local 
                           setting on the proportion of bacteria isolated from patients with set infection
                           syndromes that are resistant to antibiotics or users can use the data from our Zambian pilot.
                           For a blank template xls file use this ",
                  a("link.", 
                    href = "http://shiny.rstudio.com")),
                p("(4) Cost distribution: Users can choose to upload information from their local 
                           setting on the costs of antibiotics in their setting or users can use the data from our Zambian pilot
                           For a blank template xls file use this ",
                  a("link.", 
                    href = "http://shiny.rstudio.com")),
                p("(5) Therapy options: Users can choose up to four antibiotic therapy options consisting
                  of up to three separate antibiotics. Please use the dropdown menus to create the therapies
                  for consideration as empiric therapy options. Without these choices the app will not run."),
                p("(6) Bacterial distribution inputs: the aetiology of the infection syndrome is closely 
                  linked to the syndrome level resistance. There is great uncertainty and likely much
                  local variation in these levels. Current distributions are based on rapid literature reviews
                  and can be adjusted manually in the tab",em("Contributing pathogen distribution"),
                  "or users can tick the checkbox and use the data collected in our Zambian pilot."),
                p("(7) Once the above are inputted, users must click on the Input tab",em("Contributing pathogen distribution"),
                  "to check distributions before the application will run."),
                h5("Outputs"),
                p("Outputs are given terms of plots of the resistance data across each bacteria",em("Output: Data Visulation"),
                  "and weighted average resistance analyses in", em("Output: Table of recommendations")),
                ),
    
    
    # First tab: bacterial distribution 
    tabPanel("Input: Contributing pathogen distribution", 
             textOutput("sliders_total"),
             verbatimTextOutput("text_warning_glas"), # warnings if use data other than ATLAS - FUTURE area to update: e.g. grey out syndromes in input potential
             verbatimTextOutput("text_warning_resm"),
             verbatimTextOutput("text_warning_ecdc"),
             h2("\n"),
             uiOutput("sliders"),
             h6("Proportions must sum to 100")
    ),
    
    # Second tab: Underlying data table
    tabPanel("Output: Data Visualisation", 
             plotOutput("dataplot")),
    
    # Third tab: outputs
    tabPanel("Output: Table of recommendations.",
             h4("Summary table"),
             tableOutput("recctable"), 
             h6("*SRL = Syndrome resistance level to this antibiotic (%)"),
             h6("Note a therapy is not recommended if the SRL for any antibiotic is over the inputted threshold."),
             h6("*Miss 1st/2nd/3rd (%) = Percentage of the bacteria causing this syndrome for which there is no resistance data for this antibiotic (1st / 2nd / 3rd drug in combination where relevant)."),
             h6("*(Med, High SRL 1st/2nd/3rd) assumes 50% or 100% resistance, respectively, in the missing bacteria, instead of 0% resistance as assumed in the main SRL."),
             p("Note this recommendation should be interpreted with caution and used only on the advice of clinical support with the full awareness of the limitations of the 
                inputted data. For example consideration should be taken of"),
             p("(a) the amount of missing resistance data (shown by the Miss columns in the above table). It may be that a certain bacteria is believed to cause
             a proportion of the syndrome but there is no data on the level of resistance to antibiotics within the therapy"),
             p("(b) the aetiology of the syndrome may be unknown or uncertain in this setting - small changes in the proportion of bacteria causing a syndrome would have knock-on effects on the 
               proportion resistant and hence the recommendation for therapy."),
             p("(c) the method used: a sensitivity analysis is shown in the med,high SRL columns above which takes into account the assumption made about the sensitivity of the bacteria with missing 
               data to antibiotics within the therapy - baseline assumptions take them to be totally susceptible."), 
             p("(d) the coverage of sampling: the analysis below explores the threshold for sampling to attempt to explore the impact of bias towards sampling patients for whom
               local empiric therapy is failing and hence the bias towards more samples that are resistant being in the data."),
             p("For further limitations and discussion please check our",
               a("publication.", 
                      href = "https://wellcomeopenresearch.org/articles/4-140")), p("and",
                a("preprint.", 
                      href = "https://wellcomeopenresearch.org/articles/4-140")),
             br(), 
             h4("Threshold for sampling"),
             tableOutput("sample_prop"),
             p("The threshold for sampling is the maximum percentage of this syndrome that is sampled in this dataset in order to push the overall resistance below the inputted resistance threshold assuming all other bacteria causing this syndrome 
                         are totally susceptible. It is NA if the resistance prevalence is already below the threshold."), 
             p("e.g. if resistance = 50% in the proportion sampled and the threshold cutoff is 15%, then to recommend use of this therapy a maximum of 30% of patients with this syndrome should be sampled and in the data. The other 70% of patients with this syndrome need to “not be in the data and have a syndrome caused by a bacteria that is totally susceptible” for resistance to be below the inputted cutoff threshold for changing antibiotic.")
    )
    
    
    
  )
)
)

###********************************************************************************************************************************************************************#####
#### FUNCTIONS TO POWER SHINY ####
###********************************************************************************************************************************************************************#####

server <- function(input, output) {
  
  ### ****************************************************************************************************************************************###
  # MODEL - takes in original data and does many of the basic manipulations to get the data in the correct format for the rest of the analysis
  ### ****************************************************************************************************************************************###
  
  # Reactive dependencies - if these change then MODEL will run again and update values
  xxchange <- reactive({
    paste(input$variable, input$age, input$checkGroup, input$checkbox, input$checkbox_file_cost, input$checkbox_file_res, 
          input$range1, input$range2, input$range3, input$range4, 
          input$range5, input$range6, input$range7, input$range8,
          input$range9, input$range10, input$range11, input$range12,
          input$range13, input$range14, input$range15, input$range16,
          input$range17, input$range18,
          input$drug1.1,input$drug1.2,input$drug1.3,input$drug2.1,input$drug2.2,input$drug2.3,
          input$drug3.1,input$drug3.2,input$drug3.3,input$drug4.1,input$drug4.2,input$drug4.3)
  })
  
  ## Input data
  data_res <- reactive({
    if(!input$checkbox_file_res){inFile <- input$file_res
    if(is.null(inFile)) { return(NULL) }    
    res_levels <- read_excel_allsheets(inFile$datapath)}else{
      res_levels <- res_levels_Zambia}
    res_levels <- res_levels %>% pivot_longer(cols = colnames(res_levels)[4]:last(colnames(res_levels)), values_to = "res_prop") %>% dplyr::rename(drug = name)
    #res_levels[is.na(res_levels$res_prop),"res_prop"] <- 0 # Set to 0 if not data: TO CHECK IF KEEP as assumes no data = no resistance
    return(res_levels)
  })
  
  
  data_cost <- reactive({
    if(!input$checkbox_file_cost){inFile <- input$file_cost
    if (is.null(inFile)) { return(NULL) }    
    dataFile <- read_excel_allsheets(inFile$datapath)}else{
      dataFile <- cost_levels_Zambia}
    #dataFile<-data.frame(EndoPaste(dataFile)[1],stringsAsFactors=FALSE)
  })
  
  ### ****************************************************************************************************************************************###
  # TREAT - formulates the treatment decisions 
  ### ****************************************************************************************************************************************###
  
  # Reactive dependencies - if these change then TREAT will run again 
  xxchanget <- reactive({
    paste(model()$datam_map, model()$datam_c, model()$d11,model()$d12, model()$d21, model()$d22, model()$d3, input$res_cut,
          input$range1, input$range2, input$range3, input$range4, 
          input$range5, input$range6, input$range7, input$range8,
          input$range9, input$range10, input$range11, input$range12,
          input$range13, input$range14, input$range15, input$range16,
          input$range17, input$range18)
  })
  
  # Treatment data subset
  treat <- eventReactive(xxchanget(), {
    
    ### Data
    datam_map_new <- model()$datam_c
    
    ###### THERAPY DECISIONS #########
    ### Resistance cut off
    res_cut <- input$res_cut
    
    ### Column for each therapy 
    datam_map_new$first <- NA
    datam_map_new$second <- NA
    datam_map_new$third <- NA
    
    ######## where should use first line? ###########################
    # first line therapy for this synd = d11 / d12 combination 
    if(model()$p1 == 1||model()$p2 == 1){ # if data
      # first line therapy for this synd = d11 / d12 combination
      
      d11 <- model()$d11
      d12 <- model()$d12
      
      # Combination therapy?
      if(d12 != ""){
        ## Subset data to be just those with both drugs
        dd <- datam_map_new
        dd <- dd[which(dd$variable %in% c(d11,d12)),]
        dd <- dd %>% dplyr::group_by(Country) %>% dplyr::summarise(nn = sum(res_perc < res_cut), 
                                                                   count = n(), cisol1 = sum(n), nh = sum(res_perc > res_cut)) 
        if(dim(dd)[1]>0){
          datam_map_new <- merge(dd,datam_map_new, by = "Country",all.y=TRUE)}
        
        # If both lower than threshold then... use first line therapy
        w<-which(datam_map_new$nn == 2)
        datam_map_new[w,"first"] <- 1
        
        # If data on both drugs and not low resistance then don't use first line
        w2<-setdiff(which(datam_map_new$count == 2),w)
        datam_map_new[w2,"first"] <- 0
        
        # If only data for one or the other then leave as NA
        # UNLESS one drug > res_cut: then can't use first
        w3 <- which(datam_map_new$nh > 0)
        datam_map_new[w3,"first"] <- 0
        
      } else { # if only one drug
        dd <- datam_map_new
        dd <- dd[which(dd$variable %in% c(d11)),]
        dd <- dd %>% dplyr::group_by(Country) %>% dplyr::summarise(nn = sum(res_perc < res_cut), count = n(), cisol1 = sum(n), nh = sum(res_perc > res_cut)) 
        if(dim(dd)[1]>0){
          datam_map_new <- merge(dd,datam_map_new, by = "Country",all.y=TRUE)}
        
        ### Where resistance high? 
        wr <- which(datam_map_new$res_perc >  res_cut) # resistance higher than res_cut input (20%)
        wd1 <- which(datam_map_new$variable == d11)
        w1n <- intersect(wd1, wr) # where information and resistance to d1 > res_cut
        w1y <- setdiff(wd1, wr) # where information and resistance to d1 < res_cut
        
        datam_map_new[w1y,"first"] <- 1 # Use first line, resistance low
        datam_map_new[w1n,"first"] <- 0 # Don't use first line 
      }
      
      ## remove nn and count
      if("nn" %in% colnames(datam_map_new)){
        w<-which(colnames(datam_map_new) == "nn"); w2<-which(colnames(datam_map_new) == "count"); w3<-which(colnames(datam_map_new) == "nh")
        datam_map_new <- datam_map_new[,-c(w,w2,w3)]}
      
      ######## where should use second line? ###########################
      # where should use second line, if available?
      # second line therapy for this synd = d2 ### NOT DONE FOR COMBINATION THERAPY
      d21 <- model()$d21 #as.character(drg_bkdwn[intersect(which(drg_bkdwn$syndrome == synd),which(drg_bkdwn$Age == age)),8])
      d22 <- model()$d22 #as.character(drg_bkdwn[intersect(which(drg_bkdwn$syndrome == synd),which(drg_bkdwn$Age == age)),8])
      
      # Combination therapy?
      if(d22 != ""){
        ## Subset data to be just those with both drugs
        dd <- datam_map_new
        dd <- dd[which(dd$variable %in% c(d21,d22)),]
        dd <- dd %>% dplyr::group_by(Country) %>% dplyr::summarise(nn = sum(res_perc < res_cut), count = n(), cisol2 = sum(n), nh = sum(res_perc > res_cut)) 
        if(dim(dd)[1]>0){
          datam_map_new <- merge(dd,datam_map_new, by = "Country",all.y=TRUE)}
        w<-which(datam_map_new$nn == 2) # which countries have both res less than res_cut
        datam_map_new[w,"second"] <- 1
        
        # If data on both drugs and not low resistance then don't use
        # first says which has data on both, second which has resistance < res_cut
        # setdiff takes whats in first and not in second
        w2<-setdiff(which(datam_map_new$count == 2),w) 
        datam_map_new[w2,"second"] <- 0
        
        # UNLESS one drug > res_cut: then can't use second
        w3 <- which(datam_map_new$nh > 0)
        datam_map_new[w3,"second"] <- 0
        
      } else { # if only one drug
        dd <- datam_map_new
        dd <- dd[which(dd$variable %in% c(d21)),]
        dd <- dd %>% dplyr::group_by(Country) %>% dplyr::summarise(nn = sum(res_perc < res_cut), count = n(), cisol2 = sum(n), nh = sum(res_perc > res_cut)) 
        if(dim(dd)[1]>0){
          datam_map_new <- merge(dd,datam_map_new, by = "Country",all.y=TRUE)}
        
        ### Where resistance high? 
        wr <- which(datam_map_new$res_perc >  res_cut) # resistance higher than res_cut input (20%)
        wd2 <- which(datam_map_new$variable == d21)
        w2n <- intersect(wd2, wr) # where information and resistance to d2 > res_cut
        w2y <- setdiff(wd2, wr) # where information and resistance to d2 < res_cut
        
        datam_map_new[w2y,"second"] <- 1 # Could use second line, resistance low
        datam_map_new[w2n,"second"] <- 0 # Can't use second line, resistance to d2 too high
      }
      
      ## remove nn and count
      if("nn" %in% colnames(datam_map_new)){
        w<-which(colnames(datam_map_new) == "nn"); w2<-which(colnames(datam_map_new) == "count"); w3<-which(colnames(datam_map_new) == "nh")
        datam_map_new <- datam_map_new[,-c(w,w2,w3)]}
    }
    
    if(!("cisol1" %in% colnames(datam_map_new))){
      datam_map_new$cisol1 <- NA
    } 
    
    if(!("cisol2" %in% colnames(datam_map_new))){
      datam_map_new$cisol2 <- NA
    }
    
    ######## where should use third line? ###########################
    # where should use third line, if available?
    # third line therapy for this synd = d3 ### NOT DONE FOR COMBINATION THERAPY
    d3 <- model()$d3 #as.character(drg_bkdwn[intersect(which(drg_bkdwn$syndrome == synd),which(drg_bkdwn$Age == age)),8])
    
    dd <- datam_map_new # just do to get cisol... 
    dd <- dd[which(dd$variable %in% c(d3)),]
    dd <- dd %>% dplyr::group_by(Country) %>% dplyr::summarise(nn = sum(res_perc < res_cut), 
                                                               count = n(), cisol3 = sum(n), nh = sum(res_perc > res_cut)) 
    if(dim(dd)[1]>0){
      datam_map_new <- merge(dd,datam_map_new, by = "Country",all.y=TRUE)}
    
    ### Where resistance high? 
    wr <- which(datam_map_new$res_perc >  res_cut) # resistance higher than res_cut input (20%)
    wd3 <- which(datam_map_new$variable == d3)
    w3n <- intersect(wd3, wr) # where information and resistance to d3 > res_cut
    w3y <- setdiff(wd3, wr) # where information and resistance to d3 < res_cut
    
    datam_map_new[w3y,"third"] <- 1 # Could use third line, resistance low
    datam_map_new[w3n,"third"] <- 0 # Can't use third line, resistance to d3 too high
    
    if(!("cisol3" %in% colnames(datam_map_new))){
      datam_map_new$cisol3 <- NA
    } 
    
    ### Need to group the above information by country
    suma = function(x) if (all(is.na(x))) x[NA_integer_] else max(x, na.rm = TRUE)
    data_treat <- ddply(datam_map_new, .(Country), summarise, 
                        first_c = suma(first),  
                        second_c = suma(second),  
                        third_c = suma(third),
                        nisol = ifelse(sum(is.na(cisol1),is.na(cisol2),is.na(cisol3))< 3, min(cisol1,cisol2,cisol3,na.rm = TRUE) , 11))# Only want to say if result and < 10. #min(cisol1, cisol2, cisol3, na.rm = TRUE))
    
    ### Need at least 10 isolates
    data_treat$"Number of isolates" <- ""
    data_treat[which(data_treat$nisol < 10),"Number of isolates"] <- "< 10"
    
    # Sum over all the information given above
    suma = function(x) if (all(is.na(x))) x[NA_integer_] else max(x, na.rm = TRUE)
    data_treat$sum_t <- apply(data_treat[,c("first_c","second_c","third_c")],1,suma)
    
    #### New column for reccomendation 
    data_treat$recc <- 0
    ## Look at sum_t
    # if NA then no data on any of the three
    w<-which(!is.na(data_treat$sum_t)) # Those that are not NA
    
    # if 1 then one of columns had data 
    w11 <- which(data_treat$first_c >= 1) # Here can use first line ... 
    data_treat[w11,"recc"] <- 1 # so use it! 
    
    # These have information but can't use first 
    w10 <- which(data_treat$first_c == 0) # Here can't use first line ... 
    data_treat[w10,"recc"] <- 2.1 # so recommend second, based on first
    
    # But some will have data on second line
    w21 <- intersect(w10, which(data_treat$second_c >= 1)) # Here can't use first line ... 
    data_treat[w21,"recc"] <- 2 # and can use second so use it
    
    w20 <- intersect(w10, which(data_treat$second_c == 0)) # Here can't use first or second line ... 
    data_treat[w20,"recc"] <- 3.12 # so recommend third
    
    # But some will have data on third line
    w31 <- intersect(w20, which(data_treat$third_c >= 1)) # Here can't use first or second line ... 
    data_treat[w31,"recc"] <- 3 # and can use third so use it
    
    w30 <- intersect(w20, which(data_treat$third_c == 0)) # Here can't use first, second or third line ... 
    data_treat[w30,"recc"] <- 4.123 # so recommend fourth
    
    ### FOR NOW - IF NO DATA ON FIRST THEN CAN'T SAY DON'T USE
    suma = function(x) if (all(is.na(x))) x[NA_integer_] else max(x, na.rm = TRUE)
    ## But label where have data on others 
    data_treat$nofirstd <- as.numeric(apply(data_treat[,c("first_c","second_c","third_c")],1,suma))
    w0<-which(data_treat$recc == 0)
    wd<-which(!is.na(data_treat$nofirstd))
    data_treat[intersect(w0,wd),"recc"] <- 0.6 # where currently recc = 0, but data on 2nd or 3rd... round up to 1
    
    if(any(is.na(data_treat[w,"recc"]))){print("STOP")} # if any not assigned
    
    # Remove those whose recc is zero - don't want to plot
    wrc<-which(data_treat$recc == 0)
    #print(data_treat[wrc,]) # those with no data on the drugs used
    if(length(wrc) > 0 ){data_treat <- data_treat[-wrc,]} # remove so they are grey in the map
    
    # return all object as a list
    list(data_treat = data_treat)
  }
  )
  
  
  ### ****************************************************************************************************************************************###
  # OUTPUT$SLIDERS - generates sliders for contributing pathogen distribution 
  ### ****************************************************************************************************************************************###
  
  output$sliders <- renderUI({
    
    synd <- input$variable
    pvars <- as.character(sp_all[,1]) #as.character(unique(datam_map$Species)) # all species in data used 
    
    if(input$checkbox){sp_bkdwn <- sp_bkdwn_zambia}else{sp_bkdwn <- sp_bkdwn_lit} # use literature or Zambian?
    #if(!is.null(data_bacteria)){sp_bkdwn <- data_bacteria} # overwrite above 
    
    sp_b <- melt(sp_bkdwn[which(sp_bkdwn[,1] == synd),],id.vars = "syndrome")
    sp_b$variable <- gsub(".", ' ', sp_b$variable, fixed = T)
    ## Need "Streptococcus, viridans group"
    g <- grep("viri",sp_b$variable)
    sp_b[g,"variable"] <- "Streptococcus, viridans group"
    
    values_slid <- filter(sp_b, variable %in% pvars)
    #values_in_syn <- c(100*unique(datam_map2$Prop_Syn), rep(0,length(pvars_diff)))
    
    lapply(seq(values_slid$variable), function(i) {
      sliderInput(inputId = paste0("range",i),
                  label = em(values_slid$variable[i]),
                  min = 0, max = 100, value = values_slid$value[i])
    })
  })
  
  ### ****************************************************************************************************************************************###
  # OUTPUT$SLIDERS_TOTAL - generates total of inputted sliders for contributing pathogen distribution to check sums to 100
  ### ****************************************************************************************************************************************###
  
  output$sliders_total <- renderText({
    
    synd <- input$variable
    pvars <- as.character(sp_all[,1]) #as.character(unique(datam_map$Species)) # all species in data used 
    
    if(input$checkbox){sp_bkdwn <- sp_bkdwn_zambia}else{sp_bkdwn <- sp_bkdwn_lit} # use literature or Zambian?
    #if(!is.null(data_bacteria)){sp_bkdwn <- data_bacteria} # overwrite above 
    
    sp_b <- melt(sp_bkdwn[which(sp_bkdwn[,1] == synd),],id.vars = "syndrome")
    sp_b$variable <- gsub(".", ' ', sp_b$variable, fixed = T)
    values_slid <- filter(sp_b, variable %in% pvars)
    
    pvars_use <- pvars
    names <- c()
    for(i in 1:length(pvars_use)){names <- c(names,paste0("range", i))}
    
    values <- c()
    for(i in 1:length(pvars_use)){values <- c(values,input[[names[i]]])}
    
    if(sum(values)<101){paste0("Current total is: ", sum(values))
    }else{paste0("Current total is: ", sum(values), ".\n This is not 100. PLEASE ADJUST")
    }
  })
  
  ### ****************************************************************************************************************************************###
  # OUTPUT$INPUTPLOT - creates barchart for left panel of inputted (or baseline) contributing bacterial species
  ### ****************************************************************************************************************************************###
  
  output$inputplot <- renderPlot({
    
    cols <- colorRampPalette(brewer.pal(6,"Reds"), bias = 2)(13)
    
    synd <- input$variable
    pvars <- as.character(sp_all[,1]) #as.character(unique(datam_map$Species)) # all species in data used 
    
    if(input$checkbox){sp_bkdwn <- sp_bkdwn_zambia}else{sp_bkdwn <- sp_bkdwn_lit} # use literature or Zambian?
    #if(!is.null(data_bacteria)){sp_bkdwn <- data_bacteria} # overwrite above 
    
    sp_b <- melt(sp_bkdwn[which(sp_bkdwn[,1] == synd),],id.vars = "syndrome")
    sp_b$variable <- gsub(".", ' ', sp_b$variable, fixed = T)
    ## Need "Streptococcus, viridans group"
    g <- grep("viri",sp_b$variable)
    sp_b[g,"variable"] <- "Streptococcus, viridans group"
    
    values_slid <- filter(sp_b, variable %in% pvars)
    
    pvars_use <- pvars
    names <- c()
    for(i in 1:length(pvars_use)){names <- c(names,paste0("range", i))}
    
    values <- c()
    for(i in 1:length(pvars_use)){values <- c(values,input[[names[i]]])}
    
    bar_plot_data <- as.data.frame(cbind(rep(1, length(values)),values))
    colnames(bar_plot_data) <- c("x","value")
    bar_plot_data$Bacteria <- values_slid$variable
    
    p = ifelse(sum(bar_plot_data$value)!=100, 
               'atop(bold("Proportions do not equal 100,\nplease adjust"))', "")
    
    ggplot(bar_plot_data,aes(x = x, y = value, fill = Bacteria) ) + 
      scale_y_continuous(lim = c(0,100),"Percentage of syndrome (%)")+ 
      geom_bar(stat="identity") +  coord_flip() +
      guides(fill=guide_legend(nrow=length(pvars),byrow=TRUE)) +
      guides(fill=guide_legend(ncol=2)) + 
      theme(legend.text = element_text(face = rep("italic", length(pvars_use)))) +
      theme(legend.position = "top") + 
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) + 
      annotate("text", x = 0.5, y = 50, size = 5, label = p, colour = "black", parse = TRUE)
    
  })
  
  ### ****************************************************************************************************************************************###
  # OUTPUT$RECCTABLE - recommendation table
  ### ****************************************************************************************************************************************###
  
  output$recctable <- renderTable({
    
    # input syndrome
    synd <- input$variable
    pvars <- as.character(sp_all[,1]) #as.character(unique(datam_map$Species)) # all species in data used 
    
    if(input$checkbox){sp_bkdwn <- sp_bkdwn_zambia}else{sp_bkdwn <- sp_bkdwn_lit} # use literature or Zambian?
    # if(!is.null(data_bacteria)){sp_bkdwn <- data_bacteria} # overwrite above 
    
    sp_b <- melt(sp_bkdwn[which(sp_bkdwn[,1] == synd),],id.vars = "syndrome")
    sp_b$variable <- gsub(".", ' ', sp_b$variable, fixed = T)
    ## Need "Streptococcus, viridans group"
    g <- grep("viri",sp_b$variable)
    sp_b[g,"variable"] <- "Streptococcus, viridans group"
    
    values_slid <- filter(sp_b, variable %in% pvars)
    
    pvars_use <- pvars
    names <- c()
    for(i in 1:length(pvars_use)){names <- c(names,paste0("range", i))}
    
    values <- c()
    for(i in 1:length(pvars_use)){values <- c(values,input[[names[i]]])}
    
    # bar_plot data
    bar_plot_data <- as.data.frame(cbind(rep(1, length(values)),values))
    colnames(bar_plot_data) <- c("x","value")
    bar_plot_data$Bacteria <- values_slid$variable
    
    res_levels <- data_res() 
    cost <- data_cost() 
    
    #res_levels
    
    ## What were the inputted therapy drugs?
    therapy_choices <- as.data.frame(cbind(c(1.1, 1.2, 1.3, 2.1, 2.2, 2.3, 3.1, 3.2, 3.3, 4.1, 4.2, 4.3),
                                           c(input$drug1.1,
                                             input$drug1.2,
                                             input$drug1.3,
                                             input$drug2.1,
                                             input$drug2.2,
                                             input$drug2.3,
                                             input$drug3.1,
                                             input$drug3.2,
                                             input$drug3.3,
                                             input$drug4.1,
                                             input$drug4.2,
                                             input$drug4.3))) %>%
      set_colnames(c("line","drug")) %>%
      filter(drug != "blank")
    
    # therapy_choices <- as.data.frame(cbind(
    #   c(1.1, 1.2, 1.3, 2.1, 2.2, 2.3, 3.1, 3.2, 3.3, 4.1,4.2, 4.3),
    #   c("Ampicillin","blank", "blank","Cefazolin","blank","blank","Cefotaxime","blank","blank","Cephalothin","blank","blank"))) %>%
    #   set_colnames(c("line","drug")) %>%
    #   filter(drug != "blank")
    
    ### Add in upper and medium resistance levels if data missing 
    res_levels <- res_levels %>% mutate(res_prop_high = res_prop,res_prop_med = res_prop)
    res_levels[is.na(res_levels$res_prop_high),"res_prop_high"] <- 1
    res_levels[is.na(res_levels$res_prop_med),"res_prop_med"] <- 0.5
    
    table_output <- res_levels %>% filter(Sheet == input$variable) %>%
      right_join(therapy_choices) %>%
      left_join(bar_plot_data) %>%
      mutate(level_res_bug = res_prop * value, level_res_bug_high = res_prop_high * value,level_res_bug_med = res_prop_med * value) %>% 
      group_by(drug, line) %>%
      dplyr::summarise(synd_res_level = sum(level_res_bug, na.rm = TRUE),
                       synd_res_level_high = sum(level_res_bug_high, na.rm = TRUE),
                       synd_res_level_med = sum(level_res_bug_med, na.rm = TRUE),
                       missing = sum(is.na(res_prop)*value)) %>%
      mutate(use = ifelse(synd_res_level < input$res_cut,1,0)) %>% #))
      mutate(line_lev = sub("\\..*", "", line),
             combination2 = sub("*..", "", line)) %>%
      group_by(line_lev) %>%
      mutate(use_comb = if_else(any(use == 0),"Not recommended","Can use: resistance less than cutoff")) %>%
      pivot_wider(names_from = combination2, values_from = drug) %>%
      group_by(line_lev) %>%
      dplyr::rowwise() %>%
      mutate(`2` = ifelse("2" %in% names(.),`2`, NA)) %>% # Add in column if no dual therapies
      mutate(`3` = ifelse("3" %in% names(.),`3`, NA)) %>% # Add in column if no triple therapies
      mutate(antibiotic = ifelse(!is.na(`1`),`1`,NA)) %>%
      left_join(cost) %>%
      select(-c("antibiotic","Sheet")) %>%
      dplyr::rename(cost1 = cost) %>%
      mutate(antibiotic = ifelse(!is.na(`2`),`2`,NA)) %>%
      left_join(cost) %>%
      select(-c("antibiotic","Sheet")) %>%
      dplyr::rename(cost2 = cost) %>%
      mutate(antibiotic = ifelse(!is.na(`3`),`3`,NA)) %>%
      left_join(cost) %>%
      select(-c("antibiotic","Sheet")) %>%
      dplyr::rename(cost3 = cost) %>%
      ungroup() %>%
      mutate(synd_1 = ifelse(!is.na(`1`),synd_res_level,NA),
             synd_2 = ifelse(!is.na(`2`),synd_res_level,NA),
             synd_3 = ifelse(!is.na(`3`),synd_res_level,NA),
             synd_1_high = ifelse(!is.na(`1`),synd_res_level_high,NA), 
             synd_2_high = ifelse(!is.na(`2`),synd_res_level_high,NA),
             synd_3_high = ifelse(!is.na(`3`),synd_res_level_high,NA),
             synd_1_med = ifelse(!is.na(`1`),synd_res_level_med,NA), 
             synd_2_med = ifelse(!is.na(`2`),synd_res_level_med,NA),
             synd_3_med = ifelse(!is.na(`3`),synd_res_level_med,NA)) %>%
      dplyr::rowwise() %>%
      mutate(miss_1 = ifelse(!is.na(synd_1),missing,NA),miss_2 = ifelse(!is.na(synd_2),missing,NA),miss_3 = ifelse(!is.na(synd_3),missing,NA)) %>%
      dplyr::group_by(line_lev) %>%
      dplyr::summarize(across(everything(), ~ first(na.omit(.)))) %>%
      dplyr::rowwise() %>%
      mutate(total_cost = sum(cost1, cost2, cost3, na.rm = TRUE)) %>%
      dplyr::select(where(not_all_na))
    
    
    #table_output
    
    if("3" %in% colnames(table_output)){
      table_output_sh <- table_output %>%
        mutate(limits1 = ifelse(!is.na(synd_1_med),paste0("(",round(synd_1_med,2),",", round(synd_1_high,2),")"),NA),
               limits2 = ifelse(!is.na(synd_2_med),paste0("(",round(synd_2_med,2),",", round(synd_2_high,2),")"),NA),
               limits3 = ifelse(!is.na(synd_3_med),paste0("(",round(synd_3_med,2),",", round(synd_3_high,2),")"),NA)) %>%
        select(c("line_lev","1","2","3","use_comb","total_cost","synd_1","synd_2","synd_3","miss_1", "miss_2","miss_3","limits1", "limits2", "limits3"))
      colnames(table_output_sh) <- c("Therapy line","Drug (1st)","Drug (2nd)","Drug (3rd)","Recommendation","Total cost","SRL 1st","SRL 2nd","SRL 3rd",
                                     "Miss 1st (%)","Miss 2nd (%)","Miss 3rd (%)",
                                     "(Med, High SRL 1st)","(Med, High SRL 2nd)","(Med, High SRL 3rd)")}
    
    if(!"3" %in% colnames(table_output)){
      if("2" %in% colnames(table_output)){table_output_sh <- table_output %>%
        mutate(limits1 = ifelse(!is.na(synd_1_med),paste0("(",round(synd_1_med,2),",", round(synd_1_high,2),")"),NA),
               limits2 = ifelse(!is.na(synd_2_med),paste0("(",round(synd_2_med,2),",", round(synd_2_high,2),")"),NA)) %>%
        select(c("line_lev","1","2","use_comb","total_cost","synd_1","synd_2","miss_1","miss_2","limits1","limits2"))
      colnames(table_output_sh) <- c("Therapy line","Drug (1st)","Drug (2nd)","Recommendation","Total cost","SRL 1st","SRL 2nd",
                                     "Miss 1st (%)","Miss 2nd (%)",
                                     "(Med, High SRL 1st)","(Med, High SRL 2nd)")}else{
                                       table_output_sh <- table_output %>%
                                         mutate(limits1 = ifelse(!is.na(synd_1_med),paste0("(",round(synd_1_med,2),",", round(synd_1_high,2),")"),NA)) %>%
                                         select(c("line_lev","1","use_comb","total_cost","synd_1","miss_1", "limits1"))
                                       colnames(table_output_sh) <- c("Therapy line","Drug (1st)","Recommendation","Total cost","SRL 1st",
                                                                      "Miss 1st (%)","(Med, High SRL 1st)")
                                     }
    }
    
    
    table_output_sh
    #res_levels  %>% filter(Sheet == input$variable)
    
  })
  
  ### ****************************************************************************************************************************************###
  # OUTPUT$sample_prop - How many to not sample to be under the threshold? 
  ### ****************************************************************************************************************************************###
  
  output$sample_prop <-  renderTable({
    
    # input syndrome
    synd <- input$variable
    pvars <- as.character(sp_all[,1]) #as.character(unique(datam_map$Species)) # all species in data used 
    
    if(input$checkbox){sp_bkdwn <- sp_bkdwn_zambia}else{sp_bkdwn <- sp_bkdwn_lit} # use literature or Zambian?
    # if(!is.null(data_bacteria)){sp_bkdwn <- data_bacteria} # overwrite above 
    
    sp_b <- melt(sp_bkdwn[which(sp_bkdwn[,1] == synd),],id.vars = "syndrome")
    sp_b$variable <- gsub(".", ' ', sp_b$variable, fixed = T)
    ## Need "Streptococcus, viridans group"
    g <- grep("viri",sp_b$variable)
    sp_b[g,"variable"] <- "Streptococcus, viridans group"
    
    values_slid <- filter(sp_b, variable %in% pvars)
    
    pvars_use <- pvars
    names <- c()
    for(i in 1:length(pvars_use)){names <- c(names,paste0("range", i))}
    
    values <- c()
    for(i in 1:length(pvars_use)){values <- c(values,input[[names[i]]])}
    
    # bar_plot data
    bar_plot_data <- as.data.frame(cbind(rep(1, length(values)),values))
    colnames(bar_plot_data) <- c("x","value")
    bar_plot_data$Bacteria <- values_slid$variable
    
    res_levels <- data_res() 
    cost <- data_cost() 
    
    #res_levels
    
    ## What were the inputted therapy drugs?
    therapy_choices <- as.data.frame(cbind(c(1.1, 1.2, 1.3, 2.1, 2.2, 2.3, 3.1, 3.2, 3.3, 4.1, 4.2, 4.3),
                                           c(input$drug1.1,
                                             input$drug1.2,
                                             input$drug1.3,
                                             input$drug2.1,
                                             input$drug2.2,
                                             input$drug2.3,
                                             input$drug3.1,
                                             input$drug3.2,
                                             input$drug3.3,
                                             input$drug4.1,
                                             input$drug4.2,
                                             input$drug4.3))) %>%
      set_colnames(c("line","drug")) %>%
      filter(drug != "blank")
    
    # therapy_choices <- as.data.frame(cbind(
    #   c(1.1, 1.2, 1.3, 2.1, 2.2, 2.3, 3.1, 3.2, 3.3, 4.1,4.2, 4.3),
    #   c("Ampicillin","blank", "blank","Cefazolin","blank","blank","Cefotaxime","blank","blank","Cephalothin","blank","blank"))) %>%
    #   set_colnames(c("line","drug")) %>%
    #   filter(drug != "blank")
    
    ### Add in upper and medium resistance levels if data missing 
    res_levels <- res_levels %>% mutate(res_prop_high = res_prop,res_prop_med = res_prop)
    res_levels[is.na(res_levels$res_prop_high),"res_prop_high"] <- 1
    res_levels[is.na(res_levels$res_prop_med),"res_prop_med"] <- 0.5
    
    table_output <- res_levels %>% filter(Sheet == input$variable) %>%
      right_join(therapy_choices) %>%
      left_join(bar_plot_data) %>%
      mutate(level_res_bug = res_prop * value, level_res_bug_high = res_prop_high * value,level_res_bug_med = res_prop_med * value) %>% 
      group_by(drug, line) %>%
      dplyr::summarise(synd_res_level = sum(level_res_bug, na.rm = TRUE),
                       synd_res_level_high = sum(level_res_bug_high, na.rm = TRUE),
                       synd_res_level_med = sum(level_res_bug_med, na.rm = TRUE),
                       missing = sum(is.na(res_prop)*value)) %>%
      mutate(use = ifelse(synd_res_level < input$res_cut,1,0)) %>% #))
      mutate(line_lev = sub("\\..*", "", line),
             combination2 = sub("*..", "", line)) %>%
      group_by(line_lev) %>%
      mutate(use_comb = if_else(any(use == 0),"Not recommended","Can use: resistance less than cutoff")) %>%
      pivot_wider(names_from = combination2, values_from = drug) %>%
      group_by(line_lev) %>%
      dplyr::rowwise() %>%
      mutate(`2` = ifelse("2" %in% names(.),`2`, NA)) %>% # Add in column if no dual therapies
      mutate(`3` = ifelse("3" %in% names(.),`3`, NA)) %>% # Add in column if no triple therapies
      mutate(antibiotic = ifelse(!is.na(`1`),`1`,NA)) %>%
      left_join(cost) %>%
      select(-c("antibiotic","Sheet")) %>%
      dplyr::rename(cost1 = cost) %>%
      mutate(antibiotic = ifelse(!is.na(`2`),`2`,NA)) %>%
      left_join(cost) %>%
      select(-c("antibiotic","Sheet")) %>%
      dplyr::rename(cost2 = cost) %>%
      mutate(antibiotic = ifelse(!is.na(`3`),`3`,NA)) %>%
      left_join(cost) %>%
      select(-c("antibiotic","Sheet")) %>%
      dplyr::rename(cost3 = cost) %>%
      ungroup() %>%
      mutate(synd_1 = ifelse(!is.na(`1`),synd_res_level,NA),
             synd_2 = ifelse(!is.na(`2`),synd_res_level,NA),
             synd_3 = ifelse(!is.na(`3`),synd_res_level,NA),
             synd_1_high = ifelse(!is.na(`1`),synd_res_level_high,NA), 
             synd_2_high = ifelse(!is.na(`2`),synd_res_level_high,NA),
             synd_3_high = ifelse(!is.na(`3`),synd_res_level_high,NA),
             synd_1_med = ifelse(!is.na(`1`),synd_res_level_med,NA), 
             synd_2_med = ifelse(!is.na(`2`),synd_res_level_med,NA),
             synd_3_med = ifelse(!is.na(`3`),synd_res_level_med,NA)) %>%
      dplyr::rowwise() %>%
      mutate(miss_1 = ifelse(!is.na(synd_1),missing,NA),miss_2 = ifelse(!is.na(synd_2),missing,NA),miss_3 = ifelse(!is.na(synd_3),missing,NA)) %>%
      dplyr::group_by(line_lev) %>%
      dplyr::summarize(across(everything(), ~ first(na.omit(.)))) %>%
      dplyr::rowwise() %>%
      mutate(total_cost = sum(cost1, cost2, cost3, na.rm = TRUE)) %>%
      dplyr::select(where(not_all_na))
    
    if("3" %in% colnames(table_output)){
      prop_table <- table_output %>% select(`1`, `2`, `3`, synd_1, synd_2, synd_3)  %>% 
        mutate(max_for_regimen = pmax(synd_1, synd_2, synd_3, na.rm=TRUE)) %>%
        mutate(prop_sample = ifelse(max_for_regimen > input$res_cut, 100 * input$res_cut / max_for_regimen, NA)) %>% # less than this
        select(`1` ,`2` ,`3`, max_for_regimen, prop_sample)
      colnames(prop_table) <- c("Drug (1st)", "Drug (2nd)", "Drug (3rd)","Max SRL for regimen","Threshold for sampling (%)")}
    
    
    if(!"3" %in% colnames(table_output)){
      if("2" %in% colnames(table_output)){
        prop_table <- table_output %>% select(`1`, `2`, synd_1, synd_2) %>% 
          mutate(max_for_regimen = pmax(synd_1, synd_2, na.rm=TRUE)) %>%
          mutate(prop_sample = ifelse(max_for_regimen > input$res_cut, 100 * input$res_cut / max_for_regimen, NA)) %>% # less than this
          select(`1` ,`2` , max_for_regimen, prop_sample)
        
        colnames(prop_table) <- c("Drug (1st)", "Drug (2nd)",  "Max SRL for regimen","Threshold for sampling (%)")}else{
          prop_table <- table_output %>% select(`1`, synd_1) %>% 
            mutate(max_for_regimen = synd_1) %>%
            mutate(prop_sample = ifelse(max_for_regimen > input$res_cut, 100 * input$res_cut / max_for_regimen, NA)) %>% # less than this
            select(`1`, max_for_regimen, prop_sample)
          
          colnames(prop_table) <- c("Drug (1st)", "Max SRL for regimen","Threshold for sampling (%)")
        }
    }
    
    
    
    
    prop_table
    
    
  })
  
  
  ### ****************************************************************************************************************************************###
  # OUTPUT$DATAPLOT - Maps resistance levels
  ### ****************************************************************************************************************************************###
  
  output$dataplot <- renderPlot({
    
    theme_set(theme_bw(base_size = 11))
    # 
    # data_res() %>% filter(!is.na(res_prop), Sheet == input$variable) %>% ungroup() %>% 
    #   ggplot() +
    #   geom_bar(aes(x=reorder_within(drug, res_prop, Bacteria), y = res_prop, fill = drug),stat = "identity") + 
    #   facet_wrap(~Bacteria,scales="free_x") + 
    #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.text = element_text(size = 3)) + 
    #   scale_y_continuous("Resistance proportion") 
    
    data_res() %>% filter(!is.na(res_prop), res_prop > 0.01, Sheet == input$variable) %>% ungroup() %>% 
      ggplot() +
      geom_bar(aes(x=reorder_within(drug, res_prop, Bacteria), y = res_prop, fill = drug),stat = "identity") + 
      facet_wrap(~Bacteria,scales="free_x") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.text = element_text(size = 5)) + 
      scale_y_continuous("Resistance proportion") + 
      scale_x_reordered("Antibiotic") + 
      scale_fill_discrete("Antibiotic")
    
  }, height = 3000, width = 1500)
  
  output$dataplot2 <- renderPlot({
    
    ggplot(data_res(), aes(x = Bacteria, y = drug, fill = res_prop)) + geom_tile() +
      scale_fill_gradient("Proportion resistant", low="blue", high="red", limits = c(0,1)) + 
      facet_wrap(~Syndrome) + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      scale_y_discrete("Antibiotic")
    
  }, height = 1000, width = 1500)
  
  ### ****************************************************************************************************************************************###
  # OUTPUT$RECC - Recommendations table 
  ### ****************************************************************************************************************************************###
  
  output$recc <- renderTable({
    
    if(model()$p3 == 1 & dim(treat()$data_treat)[1]>0){
      # Data
      drec <- ""
      drec <- treat()$data_treat
      
      if(length(input$checkGroup) == 1 && input$checkGroup == "ECDC"){
        drec <- subset(drec, Country%in%ecdc_countries)}
      
      # Relabel columns
      t1 <- paste0("First line: ",model()$d11) 
      if(model()$d12 !=""){t1 <- paste0("First line: ",model()$d11," & ",model()$d12)} 
      colnames(drec)[colnames(drec) == "first_c"] <- t1
      t2 <- paste0("Second line: ",model()$d21)
      if(model()$d22 !=""){t2 <- paste0("Second line: ",model()$d21," & ",model()$d22)}
      colnames(drec)[colnames(drec) == "second_c"] <- t2
      colnames(drec)[colnames(drec) == "third_c"] <- paste0("Third line: ",model()$d3)
      
      # Recode rows
      w1<-which(drec$recc == 1)
      w2<-which(drec$recc == 2)
      w3<-which(drec$recc == 3)
      w4<-which(drec$recc == 2.1)
      w5<-which(drec$recc == 3.12)
      w6<-which(drec$recc == 0.6)
      w7<-which(drec$recc == 4.123)
      
      drec$Recommendation = ""
      drec[w1,"Recommendation"] = "Use first line"
      drec[w2,"Recommendation"] = "Use second line"
      drec[w3,"Recommendation"] = "Use third line (if exists)"
      drec[w4,"Recommendation"] = "Use second line, as resistance to first (but no data on resistance to second)"
      drec[w5,"Recommendation"] = "Use third line, as resistance to both first and second (but no data on resistance to third)"
      drec[w6,"Recommendation"] = "Use first line, but no data to inform - consider second or third if data"
      drec[w7,"Recommendation"] = "Consider alternatives! Resistance to all recommended therapies seen"
      
      #### Add in proportion of syndrome allocated
      # drugs for this syndrome
      drug_list_this_synd <- c(model()$d11,model()$d12,model()$d21,model()$d22,model()$d3)
      
      w<-which(drug_list_this_synd == "")
      if(length(w) > 0){drug_list_this_synd <- drug_list_this_synd[-w]} 
      
      # Averages across the data for all checkgroups
      data2 <- datam_map_all[datam_map_all$Dataset %in% input$checkGroup,] # those in this group
      datam_map <- data2 %>%dplyr::group_by(Country,syndrome,Species,variable) %>%
        dplyr::summarise(rate_r=mean(rate_r), Prop_Syn = mean(Prop_Syn),n = sum(n)) # mean over all datasets
      
      # sums up coverage by syndrome and drug for each country. Only those drugs in this syndrome's treatment
      dd <- datam_map %>% group_by(Country, syndrome, variable) %>% dplyr::summarise(total_prop = sum(Prop_Syn)) %>%
        subset(variable %in% drug_list_this_synd) 
      
      dd2 <- subset(dd,syndrome == input$variable) %>% group_by(Country) %>% dplyr::summarise(mm = mean(total_prop)) # Takes mean across the variables in this syndrome
      drec2 <- merge(drec, dd2, by = "Country")
      colnames(drec2)[colnames(drec2) == "mm"] <- "Mean syndrome coverage"
      
      drec2[,c("Country",t1,t2,
               paste0("Third line: ",model()$d3),"Recommendation","Number of isolates","Mean syndrome coverage")]
      
      
    }
  },
  caption = "For the different therapy options (First, Second or Third line), the values in the table 
             signify if data is available for all drugs in the therapy (NA if not) and that the therapy 
             can be used (1) or not (0). The number of isolates is blank unless there were fewer than 
              10 isolates to inform the decision, where a '< 10' is indicated. The final column gives the mean syndrome coverage
            across the relevant drugs for this syndrome for this country. A value of 0.5 means that only 50% of the contributing pathogens
            were available from sources linked to this syndrome and had been tested for the relevant antibiotics.",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL)
  )
  
  
}

### ****************************************************************************************************************************************###
# RUN SHINY APP
### ****************************************************************************************************************************************###

shinyApp(ui = ui, server = server)
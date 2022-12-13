# -----------------------------------------------------------------------------
# script setup
# -----------------------------------------------------------------------------

# load libraries
library(shiny)
library(arrow)
library(DT)
library(stringr) 
library(shinyWidgets)
library(shinycssloaders)
library(arrow)
library(tidyverse)
library(gridExtra)
library(ggborderline)
library(ggrepel)
library(ggtips)
library(ggh4x)

# set number of SOWs
nSOWS <- 1 + 159 + 500

# hide warnings to keep console clean
options(dplyr.summarise.inform = FALSE)

# color palette
blues <- c("#00429d", "#3761ab", "#5681b9", "#73a2c6", "#93c4d2", "#b9e5dd")
getBluePal <- colorRampPalette(blues)
yellow <- "#EECC66"
reds <- rev(c("#ffd3bf", "#ffa59e", "#f4777f", "#dd4c65", "#be214d", "#93003a"))
getRedPal <- colorRampPalette(reds)
fullPal <- c(blues, yellow, rev(reds))
getPal <- colorRampPalette(fullPal)

# -----------------------------------------------------------------------------
# file directory setup and loading
# -----------------------------------------------------------------------------

# set names and paths to output files
filelist <- data.frame(stringsAsFactors = FALSE,
                       "varname" = c("annualObjs", "hydroStats", "h1", "h2", "h3", "h4", "h6", "h7", "h14", "impactZones", "dynamicRob", "dynamicNorm", "staticRob", "staticNorm", "factorRank", "exoHydro"),
                       "dirname" = c("annualObjectives/annualValues.feather", "hydroStatistics/hydroStatistics.feather", "hCriteria/h1.feather", "hCriteria/h2.feather", "hCriteria/h3.feather", "hCriteria/h4.feather", "hCriteria/h6.feather", "hCriteria/h7.feather", "hCriteria/h14.feather", "impactZones/impactZoneExceedances.feather", "scenarioDiscovery/dynamicRobustness.feather", "scenarioDiscovery/dynamicNormalized.feather", "scenarioDiscovery/staticRobustness.feather", "scenarioDiscovery/staticNormalized.feather", "scenarioDiscovery/dynamicFactorRanking.feather", "scenarioDiscovery/exogenousHydro.feather"),
                       "polCol" = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0))

# impact zone description and thresholds (m)
impactZonesContext <- read.csv("data/input/impactZones.csv", check.names = FALSE, stringsAsFactors = FALSE)

# app descriptive text - blank for now
# description <- read_file("description.txt")

# -----------------------------------------------------------------------------
# pi and variable setup
# -----------------------------------------------------------------------------

print("... setting pi names ...")

# names of objectives
pis <- c("Coastal Impacts: Upstream Buildings Impacted (#)", "Coastal Impacts: Downstream Buildings Impacted (#)", "Commercial Navigation: Ontario + Seaway + Montreal Transportation Costs ($)", "Hydropower: Moses-Saunders + Niagara Energy Value ($)", "Meadow Marsh: Area (ha)", "Recreational Boating: Impact Costs ($)")

# pis to maximize
maxPIs <- c("Hydropower: Moses-Saunders + Niagara Energy Value ($)", "Meadow Marsh: Area (ha)")

# pis to minimize
minPIs <- c("Coastal Impacts: Upstream Buildings Impacted (#)", "Coastal Impacts: Downstream Buildings Impacted (#)", "Commercial Navigation: Ontario + Seaway + Montreal Transportation Costs ($)", "Recreational Boating: Impact Costs ($)")

# pi levels
piLevels <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
                       "PI" = c("upstreamCoastal", "ontarioCoastal", "alexbayCoastal", "cardinalCoastal", "downstreamCoastal", "lerybeauharnoisCoastal", "ptclaireCoastal", "sorelCoastal", "lacstpierreCoastal", "maskinongeCoastal", "troisrivieresCoastal", "totalCommercialNavigation", "ontarioCommercialNavigation", "seawayCommercialNavigation", "montrealCommercialNavigation", "totalEnergyValue", "nypaMosesSaundersEnergyValue", "opgMosesSaundersEnergyValue", "peakingMosesSaundersValue", "nypaNiagaraEnergyValue", "opgNiagaraEnergyValue", "totalRecBoating", "ontarioRecBoating", "alexbayRecBoating", "brockvilleRecBoating", "ogdensburgRecBoating", "longsaultRecBoating", "ptclaireRecBoating", "varennesRecBoating", "sorelRecBoating", "mmArea", "mmLowSupply"),
                       "PI Name" = c("Upstream Coastal Impacts", "Lake Ontario Coastal Impacts", "Alexandria Bay Coastal Impacts", "Cardinal Coastal Impacts", "Downstream Coastal Impacts", "Lery Beauharnois Coastal Impacts", "Pointe-Claire Coastal Impacts", "Sorel Coastal Impacts", "Lac St. Pierre Coastal Impacts", "Maskinonge Coastal Impacts", "Trois Rivieres Coastal Impacts", "Total Commercial Navigation", "Lake Ontario Commercial Navigation", "Seaway Commercial Navigation", "Montreal Commercial Navigation", "Total Energy Value", "NYPA @ Moses-Saunders Hydropower Energy Value", "OPG @ Moses-Saunders Hydropower Energy Value", "Moses-Saunders Hydropower Peaking Value", "NYPA @ Niagara Hydropower Energy Value", "OPG @ Niagara Hydropower Energy Value", "Total Recreational Boating", "Lake Ontario Recreational Boating", "Alexandria Bay Recreational Boating", "Brockville Recreational Boating", "Ogdensburg Recreational Boating", "Long Sault Recreational Boating", "Pointe-Claire Recreational Boating", "Varennes Recreational Boating", "Sorel Recreational Boating", "Meadow Marsh Acreage", "Meadow Marsh Low Supply Year"),
                       "PI Location" = c("Aggregate", "Lake Ontario", "Alexandria Bay", "Cardinal", "Aggregate", "Lery Beauharnois", "Pointe-Claire", "Sorel", "Lac St. Pierre", "Maskinonge", "Trois Rivieres", "Aggregate", "Lake Ontario", "Seaway", "Montreal", "Aggregate", "NYPA @ Moses-Saunders Energy Value", "OPG @ Moses-Saunders Energy Value", "Moses-Saunders Peaking Value", "NYPA @ Niagara Energy Value", "OPG @ Niagara Energy Value", "Aggregate", "Lake Ontario", "Alexandria Bay", "Brockville", "Ogdensburg", "Long Sault", "Pointe-Claire", "Varennes", "Sorel", "Meadow Marsh Acreage", "Meadow Marsh Low Supply Year"),
                       "PI Group" = c(rep("Coastal Impacts", 11), rep("Commercial Navigation", 4), rep("Hydropower", 6), rep("Recreational Boating", 9), rep("Wetland Health & Services", 2)),
                       "Individual Group" = c(rep("Upstream Coastal Impacts", 4), rep("Downstream Coastal Impacts", 7), rep("Commercial Navigation", 4), rep("Hydropower", 6), rep("Recreational Boating", 9), rep("Wetland Health & Services", 2))) %>%
  mutate(`PI Name` = factor(`PI Name`, levels = c(unique(`PI Name`))),
         `PI Location` = factor(`PI Location`, levels = c(unique(`PI Location`))),# levels = c("Aggregate", "Lake Ontario", "Alexandria Bay", "Brockville", "Ogdensburg", "Cardinal", "Long Sault", "Lery Beauharnois", "Pointe-Claire", "Varennes", "Sorel", "Maskinonge", "Lac St. Pierre", "Trois Rivieres","Seaway", "Montreal", "NYPA @ Moses-Saunders Energy Value", "OPG @ Moses-Saunders Energy Value", "Moses-Saunders Peaking Value", "NYPA @ Niagara Energy Value", "OPG @ Niagara Energy Value", "Meadow Marsh Acreage", "Meadow Marsh Low Supply Year")),
         `PI Group` = factor(`PI Group`, levels = c(unique(`PI Group`))),# levels = c("Coastal Impacts", "Commercial Navigation", "Hydropower", "Recreational Boating", "Wetland Health & Services")),
         `Individual Group` = factor(`Individual Group`, levels = c(unique(`Individual Group`))))# factor(`Individual Group`, levels = c("Upstream Coastal Impacts", "Downstream Coastal Impacts", "Commercial Navigation", "Hydropower", "Recreational Boating", "Wetland Health & Services")))

# sets geographic order of variables from lake ontario down to batiscan
hydroLevels <- data.frame(
  "Variable" = c("ontLevel", "ontFlow", "stlouisFlow", "kingstonLevel", "alexbayLevel", "brockvilleLevel", "ogdensburgLevel", "cardinalLevel", "iroquoishwLevel", "iroquoistwLevel", "morrisburgLevel", "longsaultLevel", "saundershwLevel", "saunderstwLevel", "cornwallLevel", "summerstownLevel", "lerybeauharnoisLevel", "ptclaireLevel", "jetty1Level", "stlambertLevel", "varennesLevel", "sorelLevel", "lacstpierreLevel", "maskinongeLevel", "troisrivieresLevel", "batiscanLevel"), 
  "Name" = c("Lake Ontario Level", "Lake Ontario Flow", "Lac St. Louis Flow", "Kingston Level", "Alexandria Bay Level", "Brockville Level", "Ogdensburg Level", "Cardinal Level", "Iroquois Headwaters Level", "Iroquois Tailwaters Level", "Morrisburg Level", "Long Sault Level", "Saunders Headwaters Level", "Saunders Tailwaters Level", "Cornwall Level", "Summerstown Level", "Lery Beauharnois Level", "Pointe-Claire Level", "Jetty1 Level", "St. Lambert Level", "Varennes Level", "Sorel Level", "Lac St. Pierre Level", "Maskinonge Level", "Trois Rivieres Level", "Batiscan Level")) %>%
  mutate(Location = trimws(str_remove(Name, "Level|Flow")),
         Hydro = trimws(str_remove(Name, Location)),
         Hydro = ifelse(Hydro == "Level", "Level (m)", "Flow (cms)"),
         Units = paste(Location, Hydro),
         Location = factor(Location, levels = c("Lake Ontario", "Kingston", "Alexandria Bay", "Brockville", "Ogdensburg", "Cardinal", "Iroquois Headwaters", "Iroquois Tailwaters", "Morrisburg", "Long Sault", "Saunders Headwaters", "Saunders Tailwaters", "Cornwall", "Summerstown", "Lery Beauharnois", "Lac St. Louis", "Pointe-Claire", "Jetty1", "St. Lambert", "Varennes", "Sorel", "Lac St. Pierre", "Maskinonge", "Trois Rivieres", "Batiscan")))

# satisficing criteria agreed upon by glam and the board 
piFilter <- data.frame("Group" = c("Upstream Coastal Impacts", "Downstream Coastal Impacts", "Commercial Navigation", "Hydropower", "Wetland Health & Services", "Recreational Boating"),
                       "PI" = pis,
                       "lowerBounds" = c(0, 0, 0, -0.5, -5, -10),
                       "roundDecimal" = c(0, 0, 2, 2, 0, 0))

# -----------------------------------------------------------------------------
# pareto front data
# -----------------------------------------------------------------------------

print("... loading pareto front data ...")

# load in baseline objective performance
paretoBaseline <- read.csv("data/baseline/NonDominatedPolicies.txt", sep = "\t", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE) %>%
  dplyr::select(Experiment, `Lead-Time`, Skill, Policy, all_of(pis))

# create levels for the baseline performance and optimized seeds
baselinePolicies <- paretoBaseline$Policy

# set lead-time and skill options
leadtimeOptions <- c("1-month", "3-month", "6-month", "12-month")
skillOptions <- unique(paretoBaseline$Skill)[!is.na(unique(paretoBaseline$Skill))]

# set up handles to database tables on app start
runInfo <- data.frame("fn" = grep("_", list.files("data/runs/"), value = TRUE)) %>%
  mutate(lt = unlist(lapply(str_split(fn, "_"), "[[", 1)),
         lt_pretty = paste0(unlist(strsplit(lt, "month")), "-month"),
         sk = unlist(lapply(str_split(fn, "_"), "[[", 2)),
         sk_pretty = case_when(sk == "sqAR" ~ "Status Quo (AR)", sk == "sqLM" ~ "Status Quo (LM)", sk == "0" ~ "Perfect", TRUE ~ as.character(sk)), 
         Policy = paste(lt_pretty, sk_pretty))

# load pareto front for each experiment
paretoByForecast <- list()

for (j in 1:nrow(runInfo)) {

  tmp <- data.table::fread(paste0("data/runs/", runInfo[j, "fn"], "/NonDominatedPolicies.txt")) %>%
    mutate(Policy = paste0("Seed", Policy, "_Policy", ID)) %>%
    dplyr::select(Experiment, `Lead-Time`, Skill, Policy, all_of(pis))
  
  paretoByForecast[[runInfo[j, "fn"]]] <- tmp
  
}

paretoByForecast <- bind_rows(paretoByForecast) %>%
  mutate(.before = 1, searchID = 1:nrow(.))

# load overall pareto front across all experiments=
paretoOverall <- read.delim("data/NonDominatedPolicies.txt", sep = "\t", header = TRUE, check.names = FALSE) %>%
  mutate(Policy = paste0("Seed", Policy, "_Policy", ID)) %>%
  select(Experiment, `Lead-Time`, Skill, Policy, all_of(pis))

# match identifier of individual pareto fronts to overall pareto front
matchID <- paretoByForecast %>% 
  select(searchID, Experiment, Policy)

paretoOverall <- paretoOverall %>%
  left_join(matchID, by = c("Experiment", "Policy")) %>%
  select(searchID, everything())

# -----------------------------------------------------------------------------
# baseline data
# -----------------------------------------------------------------------------
# 
# print("... loading baseline data ...")
# 
# for (i in 1:nrow(filelist)) {
#   
#   tmp <- data.table::fread(paste0("../data/baseline/postAnalysis/", filelist[i, "dirname"])) %>%
#     mutate(Policy = str_replace_all(Policy, "_", " ")) %>%
#     mutate(.before = 2, 
#            `Lead-Time` = case_when(Policy == "Plan 2014 Baseline" ~ "12-month", TRUE ~ str_split(Policy, " ")[[1]][1]),
#            Skill = case_when(Policy == "Plan 2014 Baseline" ~ "Status Quo (AR)", TRUE ~ str_split(Policy, " ")[[1]][2]))
#   
#   if (filelist[i, "polCol"] == 1) {
#     
#     tmp <- tmp %>%
#       select(-Experiment, -ID)
#     
#   }
#   
#   assign(paste0(filelist[i, "varname"], "Baseline"), tmp)
#   
#   print(paste(filelist[i, "varname"], "loaded"))
#   
# }

# -----------------------------------------------------------------------------
# post-processing results
# -----------------------------------------------------------------------------

print("... loading post-processing data ...")
# print(runInfo)

st <- Sys.time()

for (i in 1:nrow(filelist)) {
  
  # output <- list()
  # count <- 1
  # 
  # for (j in 1:nrow(runInfo)) {
  #   
  #   tmpFile <- paste0("../data/", runInfo[j, "fn"], "/postAnalysis/", filelist[i, "dirname"])
  #   
  #   if (file.exists(tmpFile)) {
  #     
  #     tmp <- data.table::fread(tmpFile) %>%
  #       mutate(.before = 2, `Lead-Time` = runInfo[j, "lt_pretty"], Skill = runInfo[j, "sk_pretty"])
  #     
  #     if (filelist[i, "polCol"] == 1) {
  #       
  #       tmp <- tmp %>%
  #         mutate(Policy = paste0("Seed", Policy, "_Policy", ID)) %>%
  #         select(-Experiment, -ID)
  #       
  #     }
  #     
  #     output[[count]] <- tmp
  #     count <- count + 1
  #     
  #   }
  #   
  # }
  # 
  # # join with baseline data
  # tmp <- bind_rows(output) %>%
  #   bind_rows(get(paste0(filelist[i, "varname"], "Baseline")))
  # 
  # assign(filelist[i, "varname"], tmp)
  # print(paste(filelist[i, "varname"], "loaded"))
  
  # tmp <- readRDS(paste0("data", filelist[i, "dirname"]))
  print(paste("... loading", filelist[i, "varname"]))
  tmp <- arrow::read_feather(paste0("data/", filelist[i, "dirname"]))
  assign(filelist[i, "varname"], tmp)
  
}

et <- Sys.time()
print(et - st)

# -----------------------------------------------------------------------------
# ui
# -----------------------------------------------------------------------------

print("starting ui")

totalwidth <- 12
sidebarWidth <- 3
mainbarWidth <- totalwidth - sidebarWidth

ui <- fluidPage(
  
  # set theme for overall web app
  theme = shinythemes::shinytheme("yeti"),
  
  # create the main tabset
  navbarPage(
    
    # app title
    "Policy Analysis",
    
    # first tab in main tab set
    tabPanel(
      
      # first tab title
      "Exploration",
      
      sidebarPanel(
        
        # side panel parameters
        style = "height: 95vh; overflow-y: auto; background-color: #F5F5F5; border-radius: 8px; border-width: 0px", 
        width = sidebarWidth,
        
        # side panel visuals
        h4("Policy Infomation"),
        selectInput(inputId = "mode", label = "Analysis", choices = c("Forecast-Specific Pareto Front", "Pareto Front Across All Forecasts"), multiple = FALSE, selected = "Pareto Front Across All Forecasts"),
        conditionalPanel(
          "input.mode == 'Pareto Front Across All Forecasts'",
          selectInput(inputId = "basePol", label = "Policy for Normalization", multiple = FALSE, choices = baselinePolicies, selected = "Plan 2014 Baseline"),
        ),
        conditionalPanel(
          "input.mode == 'Forecast-Specific Pareto Front'",
          selectInput(inputId = "leadtime", label = "Forecast Lead-Time", choices = unique(runInfo$lt_pretty), multiple = FALSE, selected = "12-Month"),
          selectInput(inputId = "skill", label = "Forecast Skill", choices = unique(runInfo$sk_pretty), multiple = FALSE, selected = "Status Quo (LM)"),
          selectInput(inputId = "basePol", label = "Policy for Normalization", multiple = FALSE, choices = baselinePolicies, selected = "Plan 2014 Baseline"),
        ),
        br(),
        h4("Policy Filtering"),
        selectInput(inputId = "filterMethod", label = "Filtering Method", multiple = FALSE, choices = c("Satisficing Criteria", "Manual Filtering", "Improvements Across All PIs"), selected = "Satisficing Criteria"),
        conditionalPanel(
          "input.filterMethod == 'Satisficing Criteria'",
          numericRangeInput(inputId = "cuNumeric", label = h5("Coastal Impacts: Upstream Buildings Impacted (#)"), value = c(0, 100)),
          numericRangeInput(inputId = "cdNumeric", label = h5("Coastal Impacts: Downstream Buildings Impacted (#)"), value = c(0, 100)),
          numericRangeInput(inputId = "cnNumeric", label = h5("Commercial Navigation: Ontario + Seaway + Montreal Transportation Costs ($)"), value = c(0, 100)),
          numericRangeInput(inputId = "hpNumeric", label = h5("Hydropower: Moses-Saunders + Niagara Energy Value ($)"), value = c(-0.5, 100)),
          numericRangeInput(inputId = "mmNumeric", label = h5("Meadow Marsh: Area (ha)"), value = c(-5, 100)),
          numericRangeInput(inputId = "rbNumeric", label = h5("Recreational Boating: Impact Costs ($)"), value = c(-10, 100))
        ),
        conditionalPanel(
          "input.filterMethod == 'Manual Filtering'",
          sliderInput(inputId = "cuSlider", label = "Coastal Impacts: Upstream Buildings Impacted (#)", min = -100, max = 100, value = c(-100, 100), step = 1, dragRange = TRUE),
          sliderInput("cdSlider", "Coastal Impacts: Downstream Buildings Impacted (#)", -100, 100, c(-100, 100), step = 1, dragRange = TRUE),
          sliderInput("cnSlider", "Commercial Navigation: Ontario + Seaway + Montreal Transportation Costs ($)", -100, 100, c(-100, 100), step = 0.1, dragRange = TRUE),
          sliderInput("hpSlider", "Hydropower: Moses-Saunders + Niagara Energy Value ($)", -100, 100, c(-100, 100), step = 0.1, dragRange = TRUE),
          sliderInput("mmSlider", "Meadow Marsh: Area (ha)", -100, 100, c(-100, 100), step = 1, dragRange = TRUE),
          sliderInput("rbSlider", "Recreational Boating: Impact Costs ($)", -100, 100, c(-100, 100), step = 1, dragRange = TRUE)
        )
        
      ),
      
      mainPanel(
        
        # main panel parameters
        width = mainbarWidth,
        
        # main panel visuals 
        fluidRow(
          column(12,
                 column(11,h3(textOutput("plotTitle"))),
                 column(1, dropdownButton(
                   br(),
                   selectInput(inputId = "plotPol", label = "Policies to Display", multiple = TRUE, choices = baselinePolicies, selected = c("Plan 2014 Baseline")), # selected = c("12-month Status Quo (AR)")),
                   selectInput(inputId = "labelUnits", label = "Plot Label Units", multiple = FALSE, choices = c("Percent Change from Baseline", "Original PI Units"), selected = "Original PI Units"),
                   selectInput(inputId = "filterTable", label = "Table Units", multiple = FALSE, choices = c("Percent Change from Baseline", "Original PI Units"), selected = "Percent Change from Baseline"),
                   circle = TRUE, icon = icon("gear"), width = "300px", right = TRUE, margin = "20px",
                   tooltip = tooltipOptions(title = "Click to See Display Options", placement = "left")))
          )
        ),
        plotOutput("filterPlot", height = 725, width = 1000, brush = brushOpts(id = "plotBrush")),
        # uiOutput("ui_plot"),
        br(),
        h4("Table of Pareto Optimal Policy Performance"),
        DT::dataTableOutput("filteredTable"),
        br(),
        
      )
      
    ),
    
    # second tab in main tab set
    tabPanel(
      
      # second tab title
      "Reevaluation",
      
      sidebarPanel(
        
        # side panel parameters
        style = "overflow-y: auto; background-color: #F5F5F5; border-radius: 8px; border-width: 0px", 
        width = sidebarWidth,
        
        # side panel visuals
        h4("Policy Infomation"),
        selectInput(inputId = "runData", label = "Reassessment Dataset", choices = c("All SOW Traces", "Historic", "Stochastic", "Climate Scenarios"), multiple = FALSE, selected = "All SOW Traces"),
        selectInput(inputId = "policySelection", label = "Policy Selection", choices = c("Select from Table", "Select by searchID"), multiple = FALSE, selected = "Select from Table"),
        conditionalPanel(
          "input.policySelection == 'Select by searchID'",
          searchInput(inputId = "evalPoliciesManual", label = "Enter policies by searchID (separated with a comma):", placeholder = NULL, btnSearch = icon("magnifying-glass"), width = "100%")
        ),
        conditionalPanel(
          "input.policySelection == 'Select from Table'",
          pickerInput(inputId = "evalPolicies", label = "Policies to Evaluate", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)))
        
      ),
      
      mainPanel(
        
        # main panel parameters
        width = mainbarWidth,
        
        # tabset in main panel
        tabsetPanel(
          
          # first panel in tabset
          tabPanel(
            "Robustness",
            br(),
            h4(textOutput("robustTitle")),
            br(),
            DT::dataTableOutput("robustnessTable") %>% withSpinner(color = blues[3], proxy.height = 200),
            br(),
            tabsetPanel(
              tabPanel("Dynamic Robustness",
                       br(),
                       plotOutput("dynamicRobustPlot", width = 1000, height = 1100) %>% withSpinner(color = blues[3], proxy.height = 200)
              ),
              tabPanel("Dynamic Factor Mapping",
                       br(),
                       plotOutput("factorMapPlot", width = 1000, height = 1200) %>% withSpinner(color = blues[3], proxy.height = 200),
              ),
              tabPanel("Static Robustness",
                       br(),
                       plotOutput("staticRobustPlot", width = 1000, height = 1100) %>% withSpinner(color = blues[3], proxy.height = 200)
              ),
            ),
            br(),
          ),
          
          tabPanel(
            "Spatial Disaggregation",
            br(),
            fluidRow(
              column(12,
                     column(11, h4("Performance Indicator by Interest/Use Group")),
                     column(1, 
                            dropdownButton(
                              br(),
                              selectInput(inputId = "boxplot", label = "Annual Statistic", choices = c("Net Annual Average", "Net Annual Minimum", "Net Annual Maximum", "Net Annual Total"), multiple = FALSE, selected = "Net Annual Average"),
                              selectInput(inputId = "mmboxplot", label = "Meadow Marsh Supply Type", choices = c("All Years", "Low Supply Years"), multiple = FALSE, selected = "All Years"),
                              circle = TRUE, icon = icon("gear"), width = "300px", right = TRUE, margin = "20px",
                              tooltip = tooltipOptions(title = "Click to See Display Options", placement = "left")))
              )
            ),
            br(),
            plotOutput("candidatePolicyPlots", width = 1000, height = 2000) %>% withSpinner(color = blues[3], proxy.height = 200),
            br(),
            h4("Historic Performance"),
            DT::dataTableOutput("candidatePolicyTable") %>% withSpinner(color = blues[3], proxy.height = 200),
            br(),
            h4("Stochastic Performance"),
            DT::dataTableOutput("reevaluationTable") %>% withSpinner(color = blues[3], proxy.height = 200),
          ),
          
          # second panel in tabset
          tabPanel(
            "Impact Zones",
            br(),
            h4("Simulated Frequency in Each Impact Zone"),
            plotOutput("impactzonePlot", width = 1000, height = 2000) %>% withSpinner(color = blues[3], proxy.height = 200),
            br(),
            h4("Impact Zone Descriptions"),
            DT::dataTableOutput("impactZoneDescription") %>% withSpinner(color = blues[3], proxy.height = 200),
            br(),
            h4("Impact Zone Categories"),
            DT::dataTableOutput("impactZoneTable") %>% withSpinner(color = blues[3], proxy.height = 200),
          ),
          
          # third panel in tabset
          tabPanel(
            "H1-H7 Criteria",
            br(),
            DT::dataTableOutput("hTable") %>% withSpinner(color = blues[3], proxy.height = 200),
            br(),
            tabsetPanel(
              tabPanel("H1",
                       h5("The regulated outflow from Lake Ontario shall be such as not to increase the frequency of low levels or reduce the minimum level of Montreal Harbour below those which would have occurred with the supplies of the past as adjusted."),
                       DT::dataTableOutput("h1Table") %>% withSpinner(color = blues[3], proxy.height = 200),
              ),
              tabPanel("H2",
                       h5("The regulated outflow from Lake Ontario shall be such as not to increase the frequency of low levels or reduce the minimum level of Lake St. Louis below those listed in the table below which would have occurred with the supplies of the past as adjusted."),
                       DT::dataTableOutput("h2Table") %>% withSpinner(color = blues[3], proxy.height = 200),
              ),
              tabPanel("H3",
                       h5("The regulated outflow from Lake Ontario shall be such that the frequencies of occurrence of high water levels on Lake St. Louis as measured at the Pointe Claire gauge are not greater than those listed below with supplies of the past as adjusted."),
                       DT::dataTableOutput("h3Table") %>% withSpinner(color = blues[3], proxy.height = 200),
              ),
              tabPanel("H4",
                       h5("The regulated monthly mean level of Lake Ontario shall not exceed elevations (IGLD85) in the corresponding months with the supplies of the past as adjusted."),
                       DT::dataTableOutput("h4Table") %>% withSpinner(color = blues[3], proxy.height = 200),
              ),
              tabPanel("H6",
                       h5("Under regulation, the frequency of occurrences of monthly mean elevations of approximately 75.07 meters (m), 246.3 feet (ft) IGLD 1985 and higher on Lake Ontario shall not be greater than would have occurred with supplies of the past as adjusted and with pre-project conditions."),
                       DT::dataTableOutput("h6Table") %>% withSpinner(color = blues[3], proxy.height = 200),
              ),
              tabPanel("H7",
                       h5("The regulated monthly mean water levels of Lake Ontario, with supplies of the past as adjusted shall not be less than the following elevations (IGLD 1985) in the corresponding months."),
                       DT::dataTableOutput("h7Table") %>% withSpinner(color = blues[3], proxy.height = 200),
              ),
            ),
            br(),
          ),
          
          # third panel in tabset
          tabPanel(
            "H14 Criteria",
            br(),
            h5("In the event that Lake Ontario water levels reach or exceed high levels, the works in the International Rapids Section shall be operated to provide all possible relief to the riparian owners upstream and downstream. In the event that Lake Ontario levels reach or fall below low levels the works in the International Rapids Section shall be operated to provide all possible relief to municipal water intakes, navigation and power purposes, upstream and downstream. The high and low water levels at which this criterion applies, and any revisions to these levels, shall be subject to the concurrence of Canada and the United States and shall be set out in a Commission directive to the Board. "),
            plotOutput("h14Plot", width = 1000, height = 1000) %>% withSpinner(color = blues[3], proxy.height = 200),
            br(),
          ),
          
          # fourth panel in tabset
          tabPanel(
            "Water Level Statistics",
            br(),
            fluidRow(
              column(12,
                     column(11, h4("Water Level Statistics by Month and Location")),
                     column(1, 
                            dropdownButton(
                              br(),
                              selectInput(inputId = "summaryStatistic", label = "Select Summary Statistic", choices = c("Monthly Mean", "Monthly Maximum", "Monthly Minimum"), multiple = FALSE, selected = "Mean"), # choices = c("Minimum", "Mean", "Median", "Maximum", "Variance"), multiple = FALSE, selected = "Mean"),
                              circle = TRUE, icon = icon("gear"), width = "300px", right = TRUE, margin = "20px",
                              tooltip = tooltipOptions(title = "Click to See Display Options", placement = "left")))
              )
            ),
            br(),
            plotOutput("waterLevelStatsPlot", width = 1000, height = 1000) %>% withSpinner(color = blues[3], proxy.height = 200),
            br(),
          )
          
        )
        
      )
      
    ),
    
    # # third tab in main tab set
    # tabPanel(
    #   
    #   "Custom Figure",
    #   
    #   sidebarPanel(
    #     
    #     # side panel parameters
    #     width = sidebarWidth,
    #     
    #     # side panel visuals
    #     selectInput(inputId = "normPolicy", label = "Policy for Normalization", multiple = FALSE, choices = baselinePolicies, selected = "Plan 2014 Baseline"),
    #     selectInput(inputId = "examplePolicies", label = "Policies to Display", multiple = TRUE, choices = baselinePolicies, selected = "Plan 2014 Baseline"),
    #     selectInput(inputId = "exampleDisplay", label = "Display Units", multiple = FALSE, choices = c("Percent Change from Baseline", "Original PI Units"), selected = "Percent Change from Baseline")
    #     
    #   ),
    #   
    #   mainPanel(
    #     
    #     # main panel parameters
    #     width = mainbarWidth,
    #     
    #     # main panel visuals
    #     textOutput(description),
    #     plotOutput("customPlot", width = 1000, height = 700)
    #     
    #   )
    
    # )
    
  )
  
)

print("done ui")

# -----------------------------------------------------------------------------
# server
# -----------------------------------------------------------------------------

print("starting server")

server <- function(input, output, session) {
  
  mode <- reactive({input$mode})
  
  # forecast leadtime and skill for input data selection
  leadtime <- reactive({
    input$leadtime
  })
  
  # forecast leadtime and skill for input data selection
  skill <- reactive({
    input$skill
  })
  
  # update forecast skill options based on lead-times
  observeEvent(leadtime(), {
    
    new_skills <- runInfo %>% 
      filter(lt_pretty == leadtime()) %>%
      # filter(lt_pretty == lt_pretty) %>%
      arrange(factor(Policy, levels = baselinePolicies)) %>%
      select(sk_pretty) %>%
      unique() %>%
      deframe()
    
    updateSelectInput(session, "skill", choices = new_skills, selected = last(new_skills))
    # updateSelectInput(session, "plotPol", selected = c("Plan 2014 Baseline", baselinePolicies[str_detect(baselinePolicies, input$leadtime)]))
    
  })
  
  # policies for normalization and plotting
  basePol <- reactive({input$basePol})
  plotPol <- reactive({input$plotPol})
  
  fm <- reactive({input$filterMethod})
  
  cuUpper <- reactive({
    if (fm() == "Improvements Across All PIs") return(input$cuNumeric[2])
    if (fm() == "Satisficing Criteria") return(input$cuNumeric[2])
    if (fm() == "Manual Filtering") return(input$cuSlider[2])
  })
  
  cuLower <- reactive({
    if (fm() == "Improvements Across All PIs") return(0)
    if (fm() == "Satisficing Criteria") return(input$cuNumeric[1])
    if (fm() == "Manual Filtering") return(input$cuSlider[1])
  })
  
  cdUpper <- reactive({
    if (fm() == "Improvements Across All PIs") return(input$cdNumeric[2])
    if (fm() == "Satisficing Criteria") return(input$cdNumeric[2])
    if (fm() == "Manual Filtering") return(input$cdSlider[2])
  })
  
  cdLower <- reactive({
    if (fm() == "Improvements Across All PIs") return(0)
    if (fm() == "Satisficing Criteria") return(input$cdNumeric[1])
    if (fm() == "Manual Filtering") return(input$cdSlider[1])
  })
  
  cnUpper <- reactive({
    if (fm() == "Improvements Across All PIs") return(input$cnNumeric[2])
    if (fm() == "Satisficing Criteria") return(input$cnNumeric[2])
    if (fm() == "Manual Filtering") return(input$cnSlider[2])
  })
  
  cnLower <- reactive({
    if (fm() == "Improvements Across All PIs") return(0)
    if (fm() == "Satisficing Criteria") return(input$cnNumeric[1])
    if (fm() == "Manual Filtering") return(input$cnSlider[1])
  })
  
  hpUpper <- reactive({
    if (fm() == "Improvements Across All PIs") return(input$hpNumeric[2])
    if (fm() == "Satisficing Criteria") return(input$hpNumeric[2])
    if (fm() == "Manual Filtering") return(input$hpSlider[2])
  })
  
  hpLower <- reactive({
    if (fm() == "Improvements Across All PIs") return(0)
    if (fm() == "Satisficing Criteria") return(input$hpNumeric[1])
    if (fm() == "Manual Filtering") return(input$hpSlider[1])
  })
  
  mmUpper <- reactive({
    if (fm() == "Improvements Across All PIs") return(input$mmNumeric[2])
    if (fm() == "Satisficing Criteria") return(input$mmNumeric[2])
    if (fm() == "Manual Filtering") return(input$mmSlider[2])
  })
  
  mmLower <- reactive({
    if (fm() == "Improvements Across All PIs") return(0)
    if (fm() == "Satisficing Criteria") return(input$mmNumeric[1])
    if (fm() == "Manual Filtering") return(input$mmSlider[1])
  })
  
  rbUpper <- reactive({
    if (fm() == "Improvements Across All PIs") return(input$rbNumeric[2])
    if (fm() == "Satisficing Criteria") return(input$rbNumeric[2])
    if (fm() == "Manual Filtering") return(input$rbSlider[2])
  })
  
  rbLower <- reactive({
    if (fm() == "Improvements Across All PIs") return(0)
    if (fm() == "Satisficing Criteria") return(input$rbNumeric[1])
    if (fm() == "Manual Filtering") return(input$rbSlider[1])
  })
  
  # update sliding tickers
  observe({
    
    pols <- normalizedParetoFront()
    
    if (fm() == 'Satisficing Criteria') {
      maxValue <- max(pols$`Coastal Impacts: Upstream Buildings Impacted (#)`)
      updateNumericRangeInput(session, "cuNumeric", value = c(0, maxValue))
      maxValue <- max(pols$`Coastal Impacts: Downstream Buildings Impacted (#)`)
      updateNumericRangeInput(session, "cdNumeric", value = c(0, maxValue))
      maxValue <- max(pols$`Commercial Navigation: Ontario + Seaway + Montreal Transportation Costs ($)`)
      updateNumericRangeInput(session, "cnNumeric", value = c(0, maxValue))
      maxValue <- max(pols$`Hydropower: Moses-Saunders + Niagara Energy Value ($)`)
      updateNumericRangeInput(session, "hpNumeric", value = c(-0.5, maxValue))
      maxValue <- max(pols$`Meadow Marsh: Area (ha)`)
      updateNumericRangeInput(session, "mmNumeric", value = c(-5, maxValue))
      maxValue <- max(pols$`Recreational Boating: Impact Costs ($)`)
      updateNumericRangeInput(session, "rbNumeric", value = c(-10, maxValue))
    } 
    
    if (fm() == 'Manual Filtering') {
      minSlider <- floor(min(pols$`Coastal Impacts: Upstream Buildings Impacted (#)`)/10) * 10
      maxSlider <- ceiling(max(pols$`Coastal Impacts: Upstream Buildings Impacted (#)`)/10) * 10
      valueSlider <- c(minSlider, maxSlider)
      updateSliderInput(session, "cuSlider", min = minSlider, max = maxSlider, value = valueSlider)
      minSlider <- floor(min(pols$`Coastal Impacts: Downstream Buildings Impacted (#)`)/10) * 10
      maxSlider <- ceiling(max(pols$`Coastal Impacts: Downstream Buildings Impacted (#)`)/10) * 10
      valueSlider <- c(minSlider, maxSlider)
      updateSliderInput(session, "cdSlider", min = minSlider, max = maxSlider, value = valueSlider)
      minSlider <- floor(min(pols$`Commercial Navigation: Ontario + Seaway + Montreal Transportation Costs ($)`)/10) * 10
      maxSlider <- ceiling(max(pols$`Commercial Navigation: Ontario + Seaway + Montreal Transportation Costs ($)`)/10) * 10
      valueSlider <- c(minSlider, maxSlider)
      updateSliderInput(session, "cnSlider", min = minSlider, max = maxSlider, value = valueSlider)
      minSlider <- floor(min(pols$`Hydropower: Moses-Saunders + Niagara Energy Value ($)`)/10) * 10
      maxSlider <- ceiling(max(pols$`Hydropower: Moses-Saunders + Niagara Energy Value ($)`)/10) * 10
      valueSlider <- c(minSlider, maxSlider)
      updateSliderInput(session, "hpSlider", min = minSlider, max = maxSlider, value = valueSlider)
      minSlider <- floor(min(pols$`Meadow Marsh: Area (ha)`)/10) * 10
      maxSlider <- ceiling(max(pols$`Meadow Marsh: Area (ha)`)/10) * 10
      valueSlider <- c(minSlider, maxSlider)
      updateSliderInput(session, "mmSlider", min = minSlider, max = maxSlider, value = valueSlider)
      minSlider <- floor(min(pols$`Recreational Boating: Impact Costs ($)`)/10) * 10
      maxSlider <- ceiling(max(pols$`Recreational Boating: Impact Costs ($)`)/10) * 10
      valueSlider <- c(minSlider, maxSlider)
      updateSliderInput(session, "rbSlider", min = minSlider, max = maxSlider, value = valueSlider)
    }    
    
  })
  
  # switch dataset based on input
  dataInput <- reactive({
    
    if (mode() == "Pareto Front Across All Forecasts") {
      
      data <- paretoOverall
      
    } else if (mode() == "Forecast-Specific Pareto Front") {
      
      data <- paretoByForecast %>%
        filter(`Lead-Time` == leadtime() & Skill == skill())
      
    } 
    
    data
    
  })
  
  # get all policies
  levelColumn <- reactive({
    
    if (mode() == "Pareto Front Across All Forecasts") return("Experiment")
    if (mode() == "Forecast-Specific Pareto Front") return("Policy")
    
  })
  
  baselineColumn <- reactive({
    
    if (mode() == "Pareto Front Across All Forecasts") return("Policy")
    if (mode() == "Forecast-Specific Pareto Front") return("Policy")
    
  })
  
  # get all policies/experiments for factor levels
  plottingLevels <- reactive({
    
    basePol <- basePol()
    plotPol <- plotPol()
    data <- dataInput()
    levelColumn <- levelColumn()
    
    levs <- data %>%
      select(all_of(levelColumn)) %>%
      unique() %>%
      deframe()
    
    if (mode() == "Pareto Front Across All Forecasts") {
      
      baseLevs <- "Plan 2014 Baseline"
      
    } else if (mode() == "Forecast-Specific Pareto Front") {
      
      baseLevs <- c(basePol, plotPol[- which(plotPol == basePol)])
      
    }
    
    c(baseLevs, levs)
    
  })
  
  # join pareto front with baseline policies
  paretoFront <- reactive({    
    
    plotPol <- plotPol()
    basePol <- basePol()
    plottingLevels <- plottingLevels()
    data <- dataInput()
    
    # only keep baseline policies that match checkboxes from ui
    bline <- paretoBaseline %>%
      mutate(.before = 1, searchID = (max(data$searchID) + 1):(max(data$searchID) + nrow(.))) %>%
      filter(Policy %in% plotPol)
    
    # find baseline policy of interest and move it to the last row for normalization
    baseInd <- which(as.character(bline$Experiment) == basePol)
    bline <- rbind(bline %>% slice(- baseInd), bline %>% slice(baseInd))
    
    data <- data %>%
      bind_rows(bline) %>%
      mutate(across(as.name(levelColumn), ~ factor(., levels = plottingLevels)))
    
    data 
    
  })
  
  # normalize values from policies around baseline policy
  normalizedParetoFront <- reactive({
    
    data <- paretoFront()
    
    # calculate % change from baseline policy of interest and filter by dynamic panel
    data <- data %>%
      mutate(across(all_of(maxPIs), ~ (.x - last(.x)) / last(.x) * 100),
             across(all_of(minPIs), ~ (.x - last(.x)) / last(.x) * -100),
             across(all_of(pis), ~ round(.x, 2)))
    
    data
    
  })
  
  # policies based on input from ui (manual filtering or brushing/clicking)
  selectedPolicies <- reactive({
    
    plotPol <- plotPol()
    data <- normalizedParetoFront()
    
    if (!is.null(input$plotBrush)) {
      
      minmax <- data %>%
        pivot_longer(cols = all_of(pis), names_to = "PI", values_to = "Score") %>%
        group_by(PI) %>%
        mutate(Score = (Score - min(Score)) / (max(Score) - min(Score))) %>%
        ungroup(PI) %>%
        mutate(Skill = case_when(str_detect(Skill, "Status Quo") ~ "Status Quo", TRUE ~ as.character(Skill)))
      
      tmp <- brushedPoints(minmax, input$plotBrush, allRows = FALSE) %>%
        select(searchID)
      
      data <- data %>% 
        mutate(.before = 1, 
               Scenario = case_when(
                 .data[[baselineColumn()]] %in% plotPol ~ paste(.data[[baselineColumn()]]),
                 # .data[[levelColumn()]] %in% plotPol ~ paste(.data[[levelColumn()]]),
                 searchID %in% unique(tmp$searchID) ~ "Brushed Policy", 
                 TRUE ~ "Set"),
               Scenario = factor(Scenario, levels = c(plotPol, "Brushed Policy", "Set"))) %>%
        arrange(Scenario)
      
    } else {
      
      cuUpper <- cuUpper()
      cuLower <- cuLower()
      cdUpper <- cdUpper()
      cdLower <- cdLower()
      cnUpper <- cnUpper()
      cnLower <- cnLower()
      hpUpper <- hpUpper()
      hpLower <- hpLower()
      mmUpper <- mmUpper()
      mmLower <- mmLower()
      rbUpper <- rbUpper()
      rbLower <- rbLower()
      
      data <- data %>%
        mutate(.before = 1, Scenario = case_when(
          .data[[baselineColumn()]] %in% plotPol ~ paste(.data[[baselineColumn()]]),
          (round(`Coastal Impacts: Upstream Buildings Impacted (#)`, 0) >= cuLower
           & round(`Coastal Impacts: Upstream Buildings Impacted (#)`, 0) <= cuUpper
           & round(`Coastal Impacts: Downstream Buildings Impacted (#)`, 0) >= cdLower
           & round(`Coastal Impacts: Downstream Buildings Impacted (#)`, 0) <= cdUpper
           & `Commercial Navigation: Ontario + Seaway + Montreal Transportation Costs ($)` >= cnLower
           & `Commercial Navigation: Ontario + Seaway + Montreal Transportation Costs ($)` <= cnUpper
           & `Hydropower: Moses-Saunders + Niagara Energy Value ($)` >= hpLower
           & `Hydropower: Moses-Saunders + Niagara Energy Value ($)` <= hpUpper
           & round(`Meadow Marsh: Area (ha)`, 0) >= mmLower
           & round(`Meadow Marsh: Area (ha)`, 0) <= mmUpper
           & round(`Recreational Boating: Impact Costs ($)`, 0) >= rbLower
           & round(`Recreational Boating: Impact Costs ($)`, 0) <= rbUpper) ~ "Policy Improvement",
          TRUE ~ "Set")) %>%
        mutate(Scenario = factor(Scenario, levels = c(plotPol, "Policy Improvement", "Set"))) %>%
        arrange(Scenario)
      
    }
    
    data
    
  })
  
  # plot labels 
  plotLabels <- reactive({
    
    if (input$labelUnits == "Original PI Units") {
      
      data <- paretoFront() %>%
        mutate(across(all_of(pis), ~ (last(.x) - .x)), across(all_of(pis), ~ round(.x, 2)))
      
    } else if (input$labelUnits == "Percent Change from Baseline") { 
      
      data <- normalizedParetoFront() %>%
        mutate(across(all_of(pis), ~ round(.x, 2)))
      
    }
    
    if (mode() == "Pareto Front Across All Forecasts") {
      
      lbl <- data %>%
        pivot_longer(cols = - c(searchID, Experiment, `Lead-Time`, Skill, Policy), names_to = "PI", values_to = "Value") %>%
        mutate(Skill = case_when(str_detect(Skill, "Status Quo") ~ "Status Quo",
                                 str_detect(Skill, "Perfect") ~ "Perfect",
                                 TRUE ~ as.character(Skill))) %>%
        group_by(PI, Skill) %>%
        summarise(Min = min(Value), Max = max(Value)) %>%
        pivot_longer(cols = - c(Skill, PI), names_to = "Range", values_to = "Value")
      
    } else if (mode() == "Forecast-Specific Pareto Front") {
      
      lbl <- data %>%
        pivot_longer(cols = - c(searchID, Experiment, `Lead-Time`, Skill, Policy), names_to = "PI", values_to = "Value") %>%
        group_by(PI) %>%
        summarise(Min = min(Value), Max = max(Value)) %>%
        pivot_longer(cols = - PI, names_to = "Range", values_to = "Value")
      
    }
    
    if (input$labelUnits == "Original PI Units") { 
      
      lbl <- lbl %>%
        mutate(Y = ifelse(Range == "Min", -0.05, 1.05),
               Value = formatC(abs(Value), format = "d", big.mark = ","),
               Value = ifelse(Range == "Min", paste("-", Value), paste("+", Value)))
      
    } else if (input$labelUnits == "Percent Change from Baseline") { 
      
      lbl <- lbl %>%
        mutate(Y = ifelse(Range == "Min", -0.05, 1.05),
               Value = ifelse(abs(Value) >= 1, round(Value, 0), Value),
               Value = ifelse(Range == "Min", paste("-", abs(Value), "%"), paste("+", abs(Value), "%")))
    }
    
    lbl
    
  })
  
  # plot title
  output$plotTitle <- renderText(({
    
    if (mode() == "Pareto Front Across All Forecasts") {
      
      paste("Pareto Front Across All Forecast Lead-Times and Skills")
      
    } else if (mode() == "Forecast-Specific Pareto Front") {
      
      paste("Improvement over Plan 2014 with a", leadtime(), skill(), "Forecast")
      
    }
    
  }))
  
  # use selected tables from table on previous tab as which stochastic runs to load
  tableSelection <- reactive({
    
    if (is.null(input$filteredTable_rows_selected)) return(NULL)
    
    data <- selectedPolicies() %>% 
      arrange(Scenario, Experiment, `Lead-Time`, Skill, Policy, searchID) %>%
      slice(as.numeric(input$filteredTable_rows_selected)) %>%
      select(searchID) %>%
      deframe()
    
  })
  
  # plot that corresponds to sidebar filters
  output$filterPlot <- renderPlot({
    
    plotPol <- plotPol()
    plotLabels <- plotLabels()
    data <- selectedPolicies()
    tableSelection <- tableSelection()
    
    if (is.null(tableSelection)) {
      
      if (mode() == "Forecast-Specific Pareto Front") {
        
        # color palette for plotting
        bluePal <- getBluePal(1)
        redPal <- getRedPal(length(plotPol))
        cols <- c(redPal, bluePal, "gray")
        
        if (!is.null(input$plotBrush)) {
          
          scenarioLevels <- c(plotPol, "Brushed Policy", "Set")
          names(cols) <- scenarioLevels
          
        } else {
          
          scenarioLevels <- c(plotPol, "Policy Improvement", "Set")
          names(cols) <- c(plotPol, "Policy Improvement", "Set")
          
        }
        
        minmax <- data %>%
          pivot_longer(cols = all_of(pis), names_to = "PI", values_to = "Score") %>%
          group_by(PI) %>%
          mutate(Score = (Score - min(Score)) / (max(Score) - min(Score))) %>%
          ungroup(PI) %>%
          mutate(PI = factor(PI, levels = pis),
                 Scenario = factor(Scenario, levels = scenarioLevels))
        
        plt <- ggplot(data = minmax, aes(x = PI, y = Score, group = searchID, color = Scenario)) +
          geom_point(alpha = 0.5) +
          geom_path(size = 1, alpha = 0.25, data = ~subset(., Scenario == "Set")) +
          geom_path(size = 1, alpha = 0.75, data = ~subset(., (Scenario != "Set" & !(Scenario %in% plotPol)))) +
          geom_path(size = 2, data = ~subset(., Scenario %in% plotPol)) +
          geom_label(data = plotLabels, aes(x = as.factor(PI), y = Y, label = Value), inherit.aes = FALSE, family = "Arial", size = 5) +
          theme_bw() +
          scale_color_manual(values = cols, limits = names(cols)) +
          scale_y_continuous(position = "right") +
          ylab("Min-Max Normalized Performance\n(Darkest Red Line = Baseline)\n") +
          theme(text = element_text(family = "Arial", color = "black", size = 18),
                title = element_blank(),
                axis.title.x = element_blank(),
                axis.text.x = element_text(size = 16),
                axis.title.y = element_text(size = 18),
                legend.title = element_blank(),
                legend.position = "top",
                legend.text = element_text(size = 15),
                legend.box = "vertical",
                legend.margin = margin())  +
          scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "pf" , " "), width = 15))
        
      } else if (mode() == "Pareto Front Across All Forecasts") {
        
        forecastPareto <- data %>%
          pivot_longer(cols = all_of(pis), names_to = "PI", values_to = "Score") %>%
          group_by(PI) %>%
          mutate(Score = (Score - min(Score)) / (max(Score) - min(Score))) %>%
          ungroup(PI) %>%
          mutate(PI = factor(PI, levels = pis),
                 Skill = case_when(str_detect(Skill, "Status Quo") ~ "Status Quo", TRUE ~ as.character(Skill)),
                 plotSK = case_when(Scenario == "Set" ~ "Set", TRUE ~ as.character(Skill)),
                 plotSK = factor(plotSK, levels = c("Status Quo", "Perfect", "Set")),
                 plotLT = case_when(Scenario == "Plan 2014 Baseline" ~ "Plan 2014 Baseline", Scenario == "Set" ~ "Set", TRUE ~ as.character(`Lead-Time`)),
                 plotLT = factor(plotLT, levels = c("Plan 2014 Baseline", leadtimeOptions, "Set"))) %>%
          arrange(desc(plotLT))
        
        # color palette for plotting
        cols <- c(reds[1], blues[5], reds[4], blues[1], yellow, "gray")
        names(cols) <- c("Plan 2014 Baseline", leadtimeOptions,"Set")
        
        plt <- ggplot(data = forecastPareto, aes(x = PI, y = Score, group = fct_inorder(as.character(searchID)))) +
          geom_borderline(
            aes(linetype = plotSK, alpha = Scenario, color = plotLT, size = Scenario), 
            borderwidth = 0.75) + 
          geom_label(
            data = plotLabels, aes(x = as.factor(PI), y = Y, label = Value), 
            inherit.aes = FALSE,  family = "Arial", size = 5) +
          theme_bw() +
          scale_size_manual(values = c(1, 1, 0.25)) + 
          scale_color_manual(values = cols, limits = names(cols)) + 
          scale_alpha_manual(values = c(0.9, 0.75, 0.1)) +
          scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
          guides(alpha = "none", size = "none",
                 linetype = guide_legend(order = 1, keywidth = unit(2, "cm")), 
                 color = guide_legend(nrow = 1, keywidth = unit(1, "cm"))) +
          scale_x_discrete(
            labels = function(x) str_wrap(str_replace_all(x, "pf" , " "), width = 15)) + 
          scale_y_continuous(
            position = "right", 
            name = "Min-Max Normalized Performance\n") +
          theme(
            text = element_text(color = "black", family = "Arial", size = 18),
            title = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 18),
            axis.text = element_text(size = 16),
            legend.title = element_blank(),
            legend.position = "top",
            legend.box = "vertical", 
            legend.text = element_text(size = 16),
            legend.margin = margin()
          )
        
      }
      
      
    } else {
      
      if (mode() == "Forecast-Specific Pareto Front") {
        
        # color palette for plotting
        bluePal <- getBluePal(1)
        redPal <- getRedPal(length(plotPol))
        cols <- c(redPal, yellow, bluePal, "gray")
        
        if (!is.null(input$plotBrush)) {
          
          scenarioLevels <- c(plotPol, "Selected Policy", "Brushed Policy", "Set")
          names(cols) <- scenarioLevels
          
        } else {
          
          scenarioLevels <- c(plotPol, "Selected Policy", "Policy Improvement", "Set")
          names(cols) <- scenarioLevels
          
        }
        
        minmax <- data %>%
          pivot_longer(cols = all_of(pis), names_to = "PI", values_to = "Score") %>%
          group_by(PI) %>%
          mutate(Score = (Score - min(Score)) / (max(Score) - min(Score))) %>%
          ungroup(PI) %>%
          mutate(PI = factor(PI, levels = pis),
                 # Scenario = factor(Scenario, levels = scenarioLevels),
                 Selected = ifelse(searchID %in% tableSelection, "Selected Policy", as.character(Scenario)),
                 Selected = factor(Selected, levels = scenarioLevels))
        
        plt <- ggplot(data = minmax, aes(x = PI, y = Score, group = searchID, color = Selected)) +
          geom_point(alpha = 0.5) +
          geom_path(size = 1, alpha = 0.25, data = ~subset(., Selected == "Set")) +
          geom_path(size = 1, alpha = 0.75, data = ~subset(., (Selected != "Set" & !(Selected %in% plotPol)))) +
          geom_path(size = 1.5, alpha = 1.0, data = ~subset(., (Selected == "Selected Policy"))) +
          geom_path(size = 2, data = ~subset(., Selected %in% plotPol)) +
          geom_label(data = plotLabels, aes(x = as.factor(PI), y = Y, label = Value), inherit.aes = FALSE, family = "Arial", size = 5) +
          theme_bw() +
          scale_color_manual(values = cols, limits = names(cols)) +
          scale_y_continuous(position = "right") +
          ylab("Min-Max Normalized Performance\n(Darkest Red Line = Baseline)\n") +
          theme(text = element_text(family = "Arial", color = "black", size = 18),
                title = element_blank(),
                axis.title.x = element_blank(),
                axis.text.x = element_text(size = 16),
                axis.title.y = element_text(size = 18),
                legend.title = element_blank(),
                legend.position = "top",
                legend.text = element_text(size = 15),
                legend.box = "vertical",
                legend.margin = margin())  +
          scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "pf" , " "), width = 15))
        
      } else if (mode() == "Pareto Front Across All Forecasts") {
        
        # color palette for plotting
        cols <- c(reds[1], "orange", blues[5], reds[4], blues[1], yellow, "gray")
        names(cols) <- c("Plan 2014 Baseline", "Selected Policy", leadtimeOptions,"Set")
        
        forecastPareto <- data %>%
          pivot_longer(cols = all_of(pis), names_to = "PI", values_to = "Score") %>%
          group_by(PI) %>%
          mutate(Score = (Score - min(Score)) / (max(Score) - min(Score))) %>%
          ungroup(PI) %>%
          mutate(PI = factor(PI, levels = pis),
                 Skill = case_when(str_detect(Skill, "Status Quo") ~ "Status Quo", TRUE ~ as.character(Skill)),
                 plotSK = case_when(Scenario == "Set" ~ "Set", TRUE ~ as.character(Skill)),
                 plotSK = factor(plotSK, levels = c("Status Quo", "Perfect", "Set")),
                 plotLT = case_when(Scenario == "Plan 2014 Baseline" ~ "Plan 2014 Baseline", Scenario == "Set" ~ "Set", TRUE ~ as.character(`Lead-Time`)),
                 plotLT = factor(plotLT, levels = c("Plan 2014 Baseline", leadtimeOptions, "Set")),
                 Selected = case_when(searchID %in% tableSelection ~ "Selected Policy", 
                                      Scenario %in% plotPol ~ as.character(Scenario),
                                      Scenario == "Set" ~ as.character(Scenario),
                                      TRUE ~ as.character(`Lead-Time`)),
                 Selected = factor(Selected, levels = names(cols))) %>%
          arrange(desc(Selected))
        
        plt <- ggplot(data = forecastPareto, aes(x = PI, y = Score, group = fct_inorder(as.character(searchID)))) +
          geom_borderline(
            aes(linetype = plotSK, alpha = Scenario, color = plotLT), 
            linewidth = 0.9, borderwidth = 0.75
          ) + 
          geom_label(
            data = plotLabels, aes(x = as.factor(PI), y = Y, label = Value), 
            inherit.aes = FALSE, family = "Arial", size = 5
          ) +
          theme_bw() +
          scale_color_manual(values = cols, limits = names(cols)) + 
          scale_alpha_manual(values = c(1, 0.9, 0.1)) +
          scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
          guides(
            alpha = "none", 
            linetype = guide_legend(order = 1, keywidth = unit(2, "cm")), 
            color = guide_legend(nrow = 1, keywidth = unit(1, "cm")),
          ) +
          scale_x_discrete(
            labels = function(x) str_wrap(str_replace_all(x, "pf" , " "), width = 15)) + 
          scale_y_continuous(
            position = "right", 
            name = "Min-Max Normalized Performance\n") +
          theme(
            text = element_text(family = "Arial", color = "black", size = 18),
            title = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 18),
            axis.text = element_text(size = 16),
            legend.title = element_blank(),
            legend.position = "top",
            legend.box = "vertical", 
            legend.text = element_text(size = 16),
            legend.margin = margin()
          )
      }
      
    }
    
    plt 
    
  })
  
  # table that corresponds to policies
  output$filteredTable <- DT::renderDataTable({
    
    plotPol <- plotPol()
    selectedPolicies <- selectedPolicies()
    paretoFront <- paretoFront()
    
    if (input$filterTable == "Percent Change from Baseline") {
      
      data <- selectedPolicies
      
    } else if (input$filterTable == "Original PI Units") {
      
      data <- paretoFront %>%
        left_join(., selectedPolicies %>% select(Scenario, searchID, Experiment, `Lead-Time`, Skill, Policy), 
                  by = c("searchID", "Experiment", "Lead-Time", "Skill", "Policy"))
      
    }
    
    data %>% 
      arrange(Scenario, Experiment, `Lead-Time`, Skill, Policy, searchID) %>%
      select(Scenario, `Lead-Time`, Skill, Policy, all_of(pis), searchID)
    
  },
  options = list(
    language = list(lengthMenu = "_MENU_"), 
    search = list(regex = TRUE, caseInsensitive = TRUE),
    scrollX = TRUE, 
    scrollY = TRUE, 
    paging = TRUE,
    pageLength = 15,
    lengthChange = FALSE,
    width = 1000,
    sDom  = '<"top">rt<"bottom">ifp'
  ),
  rownames = FALSE,
  filter = list(position = 'bottom'),
  caption = 'Note: Hold shift and click to order by multiple columns.')
  
  # ---
  # stochastic/climate reanalysis
  # ---
  
  # use selected tables from table on previous tab as which stochastic runs to load
  candidateIndex <- reactive({
    input$filteredTable_rows_selected
  })
  
  policySelection <- reactive({input$policySelection})
  
  # update policies to reevaluate
  observeEvent(candidateIndex(), {
    
    if (policySelection() == "Select from Table") {
      
      data <- selectedPolicies() %>%
        arrange(Scenario, Experiment, `Lead-Time`, Skill, Policy, searchID) %>%
        slice(as.numeric(candidateIndex())) %>%
        select(searchID) %>%
        deframe()
      
      updatePickerInput(session, "evalPolicies", choices = data, selected = data)
      
    }
    
  })
  
  candidatePolicies <- reactive({
    
    if (is.null(input$evalPolicies) & is.null(input$evalPoliciesManual)) return()
    
    if (policySelection() == "Select from Table") {
      
      pols <- as.numeric(input$evalPolicies)
      
    } else if (policySelection() == "Select by searchID") {
      
      pols <- as.numeric(trimws(str_split(input$evalPoliciesManual, ",")[[1]]))
      
    }
    
    pols
    
  })
  
  runData <- reactive({
    
    dataType <- input$runData
    
    dataFilter <- ifelse(dataType == "Historic", "Historic",
                         ifelse(dataType == "Stochastic", "Stochastic",
                                ifelse(dataType == "Climate Scenarios", "ssp", "All SOW Traces")))
    dataFilter
    
  })
  
  runPolicies <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    indPolicies <- candidatePolicies()
    
    data <- paretoByForecast %>%
      filter(searchID %in% indPolicies) %>%
      mutate(.before = 1, Lookup = paste0(`Lead-Time`, " ", Skill, " (", searchID, ")")) %>%
      select(searchID, Lookup, `Lead-Time`, Skill, Policy, all_of(pis))
    
    bline <- paretoBaseline %>%
      filter(Policy == "Plan 2014 Baseline") %>%
      mutate(.before = 1, searchID = 0, Lookup = "Plan 2014") %>%
      select(searchID, Lookup, `Lead-Time`, Skill, Policy, all_of(pis))
    
    # print(paste(data$searchID, collapse = ","))
    tmp <- rbind(data, bline)
    tmp
    
  })
  
  output$candidatePolicyTable <- renderDataTable({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    runPolicies()
    
  },
  options = list(
    scrollX = TRUE,
    searching = FALSE,
    sDom  = '<"top">rt',
    width = 1000
  ),
  rownames = FALSE)
  
  filterCrit <- reactive({
    
    runPolicies() %>% select(searchID, Lookup, `Lead-Time`, Skill, Policy)
    
  })
  
  polLevels <- reactive({
    
    filterCrit <- filterCrit()
    pols <- filterCrit$Lookup
    c(pols[pols == "Plan 2014"], pols[pols != "Plan 2014"])
    
    
  })
  
  # ---
  # measures of robustness
  # ---
  
  output$robustTitle <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    n <- nSOWS
    paste0("Measure of Robustness (n = ", n, " Traces)")
    
  })
  
  # performance based on Plan 2014 with historic supplies (STATIC)
  staticRobustness <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    runData <- runData()
    
    data <- staticRob %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      filter(if (runData != "All SOW Traces") str_detect(SOW, runData) else TRUE) %>%
      mutate(`Robust Objectives` = rowSums(select(., contains(pis))),
             `Robust Policy` = ifelse(`Robust Objectives` == 6, 1, 0))
    
    data
    
  })
  
  # performance based on Plan 2014 with historic supplies (STATIC)
  staticNormalized <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    runData <- runData()
    
    data <- staticNorm %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      filter(if (runData != "All SOW Traces") str_detect(SOW, runData) else TRUE)
    
    data
    
  })
  
  # performance based on Plan 2014 with same stochastic trace of supplies (DYNAMIC)
  dynamicRobustness <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    runData <- runData()
    
    data <- dynamicRob %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      filter(if (runData != "All SOW Traces") str_detect(SOW, runData) else TRUE) %>%
      mutate(`Robust Objectives` = rowSums(select(., contains(pis))),
             `Robust Policy` = ifelse(`Robust Objectives` == 6, 1, 0))
    
    data
    
  })
  
  # performance based on Plan 2014 with same stochastic trace of supplies (DYNAMIC)
  dynamicNormalized <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    runData <- runData()
    
    data <- dynamicNorm %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      filter(if (runData != "All SOW Traces") str_detect(SOW, runData) else TRUE)
    
    data
    
  })
  
  # see what exogenous variables affect pi robustness
  factorRanking <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    runData <- runData()
    
    data <- factorRank %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      filter(if (runData != "All SOW Traces") str_detect(SOW, runData) else TRUE) %>%
      filter(Policy != "Plan 2014 Baseline")
    
    data
    
  })
  
  # see what exogenous variables affect pi robustness
  exogenousHydro <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    runData <- runData()
    
    data <- exoHydro %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      filter(if (runData != "All SOW Traces") str_detect(SOW, runData) else TRUE) %>%
      filter(Policy != "Plan 2014 Baseline")
    
    
    data
    
  })
  
  output$robustnessTable <- renderDataTable({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    staticRobustness <- staticRobustness()
    dynamicRobustness <- dynamicRobustness()
    polLevels <- polLevels()
    
    static <- staticRobustness %>%
      group_by(searchID, Lookup, `Lead-Time`, Skill, Policy) %>%
      summarise(`Robust SOWs` = sum(`Robust Policy`),
                `Total SOWs` = nSOWS,
                `Robustness Score` = round((`Robust SOWs` / `Total SOWs`) * 100, 2)) %>%
      ungroup() %>%
      select(Lookup, `Robust SOWs`, `Robustness Score`) %>%
      setNames(c("Lookup", "Robust SOWs (Static)", "Robustness Score (Static)"))
    
    dynamic <- dynamicRobustness %>%
      group_by(searchID, Lookup, `Lead-Time`, Skill, Policy) %>%
      summarise(`Robust SOWs` = sum(`Robust Policy`),
                `Total SOWs` = nSOWS,
                `Robustness Score` = round((`Robust SOWs` / `Total SOWs`) * 100, 2)) %>%
      ungroup() %>%
      select(Lookup, `Robust SOWs`, `Robustness Score`) %>%
      setNames(c("Lookup", "Robust SOWs (Dynamic)", "Robustness Score (Dynamic)"))
    
    full_join(static, dynamic, by = c("Lookup")) %>%
      mutate(Lookup = factor(Lookup, levels = polLevels))
    
  },
  options = list(
    sDom  = '<"top">t',
    width = 1000
  ),
  rownames = FALSE)
  
  output$staticRobustPlot <- renderPlot({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    staticRobustness <- staticRobustness()
    staticNormalized <- staticNormalized()
    exogenousHydro <- exogenousHydro()
    polLevels <- polLevels()
    
    tmp <- exogenousHydro %>%
      select(Lookup, SOW, ontNTS) %>%
      arrange(ontNTS) %>%
      mutate(Label = case_when(str_detect(SOW, "Historic") ~ "Historic",
                               str_detect(SOW, "Stochastic") ~ paste("ST", str_remove(SOW, "Stochastic Century ")),
                               str_detect(SOW, "ssp") ~ "CC", 
                               TRUE ~ "NA"))
    
    # get severity of failures for coloring
    failColor <- staticNormalized %>%
      select(Lookup, SOW, all_of(pis)) %>%
      setNames(c("Lookup", "SOW", "UpCoast", "DownCoast", "ComNav", "Hydro", "Wetland", "RecBoat")) %>%
      pivot_longer(cols = -c(Lookup, SOW), names_to = "PI", values_to = "Severity")
    
    # make matrix for portrait plot of rules
    staticPortrait <- staticRobustness %>%
      select(Lookup, SOW, all_of(pis)) %>%
      setNames(c("Lookup", "SOW", "UpCoast", "DownCoast", "ComNav", "Hydro", "Wetland", "RecBoat")) %>%
      pivot_longer(cols = -c(Lookup, SOW), names_to = "PI", values_to = "Value") %>%
      left_join(., tmp %>% select(Lookup, SOW, Label), by = c("Lookup", "SOW")) %>%
      left_join(., failColor, by = c("Lookup", "SOW", "PI")) %>%
      mutate(Value = ifelse(Value == 1, "Robust", "Fails")) %>%
      group_by(Lookup, PI, Value) %>%
      mutate(normSev = case_when(Value == "Robust" ~ NaN, TRUE ~ (Severity - min(Severity)) / (max(Severity) - min(Severity))),
             PI = factor(PI, levels = rev(c("UpCoast", "DownCoast", "ComNav", "Hydro", "Wetland", "RecBoat"))),
             # Policy = factor(Policy, levels = c("Plan 2014 Baseline", unique(staticRobustness$Policy)[which(unique(staticRobustness$Policy) != "Plan 2014 Baseline")])),
             Lookup = factor(Lookup, levels = polLevels),
             SOW = factor(SOW, levels = unique(tmp$SOW)),
             Label = case_when(str_detect(SOW, "Historic") ~ "H", str_detect(SOW, "Stochastic") ~ "S", str_detect(SOW, "ssp") ~ "C", TRUE ~ "NA"),
             Label = factor(Label, levels = c("H", "S", "C")))
    
    ggplot(data = staticPortrait, aes(x = SOW, y = PI)) +
      facet_wrap(~ Lookup, ncol = 1, scales = "free_y") +
      geom_tile(aes(color = "Robust", fill = normSev)) +
      theme_bw() +
      xlab("Supply Trace (Dry SOWs--> Wet SOWs)") +
      scale_fill_gradientn(colors = rev(reds), na.value = blues[1], breaks = c(0, 1), labels = c("Mild Failure", "Severe Failure"), guide = guide_legend(direction = "horizontal", title.position = "bottom")) +
      scale_color_manual(values = "white") +
      guides(fill = guide_colorbar(), color = guide_legend(override.aes = list(fill = blues[1]))) +
      scale_x_discrete(labels = "H",breaks = "H") +
      theme(text = element_text(family = "Arial", color = "black", size = 18),
            title = element_blank(),
            axis.title.x = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
            legend.position = "top",
            legend.title = element_blank(),
            legend.margin = margin(),
            legend.key.width = unit(2, "cm")
            
      )
    
  })
  
  output$dynamicRobustPlot <- renderPlot({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    dynamicRobustness <- dynamicRobustness()
    dynamicNormalized <- dynamicNormalized()
    exogenousHydro <- exogenousHydro()
    polLevels <- polLevels()
    
    tmp <- exogenousHydro %>%
      select(Lookup, SOW, ontNTS) %>%
      arrange(ontNTS) %>%
      mutate(Label = case_when(str_detect(SOW, "Historic") ~ "Historic",
                               str_detect(SOW, "Stochastic") ~ paste("ST", str_remove(SOW, "Stochastic Century ")),
                               str_detect(SOW, "ssp") ~ "CC", 
                               TRUE ~ "NA"))
    
    # get severity of failures for coloring
    failColor <- dynamicNormalized %>%
      select(Lookup, SOW, all_of(pis)) %>%
      setNames(c("Lookup", "SOW", "UpCoast", "DownCoast", "ComNav", "Hydro", "Wetland", "RecBoat")) %>%
      pivot_longer(cols = -c(Lookup, SOW), names_to = "PI", values_to = "Severity")
    
    # make matrix for portrait plot of rules
    dynamicPortrait <- dynamicRobustness %>%
      select(Lookup, SOW, all_of(pis)) %>%
      setNames(c("Lookup", "SOW", "UpCoast", "DownCoast", "ComNav", "Hydro", "Wetland", "RecBoat")) %>%
      pivot_longer(cols = -c(Lookup, SOW), names_to = "PI", values_to = "Value") %>%
      left_join(., tmp %>% select(Lookup, SOW, Label), by = c("Lookup", "SOW")) %>%
      left_join(., failColor, by = c("Lookup", "SOW", "PI")) %>%
      mutate(Value = ifelse(Value == 1, "Robust", "Fails")) %>%
      group_by(Lookup, PI, Value) %>%
      mutate(normSev = case_when(Value == "Robust" ~ NaN, TRUE ~ (Severity - min(Severity)) / (max(Severity) - min(Severity))),
             PI = factor(PI, levels = rev(c("UpCoast", "DownCoast", "ComNav", "Hydro", "Wetland", "RecBoat"))),
             Lookup = factor(Lookup, levels = polLevels),
             SOW = factor(SOW, levels = unique(tmp$SOW)),
             Label = case_when(str_detect(SOW, "Historic") ~ "H", str_detect(SOW, "Stochastic") ~ "S", str_detect(SOW, "ssp") ~ "C", TRUE ~ "NA"),
             Label = factor(Label, levels = c("H", "S", "C")))
    
    ggplot(data = dynamicPortrait, aes(x = SOW, y = PI)) +
      facet_wrap(~ Lookup, ncol = 1, scales = "free_y") +
      geom_tile(aes(color = "Robust", fill = normSev)) +
      theme_bw() +
      xlab("Supply Trace (Dry SOWs--> Wet SOWs)") +
      scale_fill_gradientn(colors = rev(reds), na.value = blues[1], breaks = c(0, 1), labels = c("Mild Failure", "Severe Failure"), guide = guide_legend(direction = "horizontal", title.position = "bottom")) +
      scale_color_manual(values = "white") +
      guides(fill = guide_colorbar(), color = guide_legend(override.aes = list(fill = blues[1]))) +
      scale_x_discrete(labels = "H",breaks = "H") +
      theme(text = element_text(family = "Arial", color = "black", size = 18),
            title = element_blank(),
            axis.title.x = element_text(size = 18),
            axis.text.y = element_text(size = 16),
            axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
            legend.position = "top",
            legend.title = element_blank(),
            legend.margin = margin(),
            legend.key.width = unit(2, "cm")
      )
    
  })
  
  output$factorMapPlot <- renderPlot({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    factorRanking <- factorRanking()
    dynamicRobustness <- dynamicRobustness()
    dynamicNormalized <- dynamicNormalized()
    exogenousHydro <- exogenousHydro()
    polLevels <- polLevels()
    
    output <- list()
    labels <- list()
    count <- 1
    
    # get severity of failures for coloring
    failColor <- dynamicNormalized %>%
      select(Lookup, SOW, all_of(pis)) %>%
      pivot_longer(cols = -c(Lookup, SOW), names_to = "PI", values_to = "Severity")
    
    # get plot labels
    plans <- unique(factorRanking$Lookup)
    for (p in plans) {
      
      # filter objective by plan
      obj <- dynamicRobustness %>%
        filter(Lookup == p) %>%
        pivot_longer(cols = all_of(pis), names_to = "PI", values_to = "Robust Score") %>%
        select(Lookup, SOW, PI, `Robust Score`)
      
      # get top two factors for each plan
      hydro <- factorRanking %>%
        filter(Lookup == p) %>% 
        select(-c(`Lead-Time`, Skill, Policy)) %>%
        pivot_longer(cols = - c(searchID, Lookup, PI), names_to = "Variable", values_to = "Value") %>% 
        mutate(Value = Value * 100) %>%
        arrange(desc(Value)) %>% 
        group_by(PI) %>%
        slice(1:2)
      
      x <- hydro %>%
        slice(1) %>%
        setNames(c("searchID", "Lookup", "PI", "X Variable", "X Importance")) %>%
        ungroup()
      
      y <- hydro %>%
        slice(-1) %>%
        setNames(c("searchID", "Lookup", "PI", "Y Variable", "Y Importance")) %>%
        ungroup()
      
      featureImportance <- full_join(x, y, by = c("searchID", "Lookup", "PI"))
      
      for (i in 1:length(pis)) {
        
        poi <- pis[i]
        
        xVar <- featureImportance %>%
          filter(PI == poi) %>%
          select(`X Variable`) %>%
          deframe()
        
        yVar <- featureImportance %>%
          filter(PI == poi) %>%
          select(`Y Variable`) %>%
          deframe()
        
        tmp <- exogenousHydro %>%
          filter(Lookup == p) %>%
          select(Lookup, SOW, all_of(xVar), all_of(yVar)) %>%
          setNames(c("Lookup", "SOW", "X", "Y"))
        
        tmp2 <- obj %>%
          filter(PI == poi) %>%
          left_join(., tmp, by = c("Lookup", "SOW"))
        
        output[[count]] <- tmp2
        
        tmpLabel <- featureImportance %>%
          mutate(# Label = paste0(" X: ", `X Variable`, " (", round(`X Importance`, 2), "%)", "\n Y: ", `Y Variable`, " (", round(`Y Importance`, 2), "%)"),
            xLabel = paste0(`X Variable`, " (", round(`X Importance`, 0), "%)"),
            yLabel = paste0(`Y Variable`, " (", round(`Y Importance`, 0), "%)"),
            Lookup = p) %>%
          # select(`Lead-Time`, Skill, Policy, PI, Label)
          select(Lookup, PI, xLabel, yLabel)
        
        labels[[count]] <- tmpLabel
        
        count <- count + 1
        
      }
      
    }
    
    data <- bind_rows(output) %>%
      mutate(Dataset = case_when(str_detect(SOW, "Historic") ~ "Historic",
                                 str_detect(SOW, "Stochastic") ~ "Stochastic",
                                 str_detect(SOW, "ssp") ~ "Climate Scenario",
                                 TRUE ~ "NA"),
             Robust = case_when(`Robust Score` == 1 ~ "Robust",
                                `Robust Score` == 0 ~ "Fails",
                                TRUE ~ "NA")) %>%
      left_join(., failColor, by = c("Lookup", "SOW", "PI")) %>%
      group_by(Lookup, PI, Robust) %>%
      mutate(normSev = case_when(Robust == "Robust" ~ NaN, TRUE ~ (Severity - min(Severity)) / (max(Severity) - min(Severity))),
             # Policy = factor(Policy, levels = c("Plan 2014 Baseline", as.character(plans[plans != "Plan 2014 Baseline"]))),
             Lookup = factor(Lookup, levels = polLevels),
             PI = factor(PI, levels = pis),
             Dataset = factor(Dataset, levels = c("Historic", "Stochastic", "Climate Scenario"))) %>%
      arrange(desc(Dataset))
    
    pltLabels <- bind_rows(labels) %>% 
      distinct() %>%
      mutate(Lookup = factor(Lookup, levels = polLevels),
             PI = factor(PI, levels = pis),
             xPer = str_split(xLabel, " ", simplify = TRUE)[, 2],
             yPer = str_split(yLabel, " ", simplify = TRUE)[, 2],
             xLabelTmp = case_when(str_detect(xLabel, "ontNTS") ~ paste("Upstream Supply", xPer), str_detect(xLabel, "stlouisontOut") ~ paste("Downstream Supply", xPer), str_detect(xLabel, "unstableIce") ~ paste("Unstable Ice Present", xPer), TRUE ~ "NA"),
             xLabel = case_when(str_detect(xLabel, "Spring") ~ paste("Spring", xLabelTmp), str_detect(xLabel, "Summer") ~ paste("Summer", xLabelTmp), str_detect(xLabel, "Fall") ~ paste("Autumn", xLabelTmp), str_detect(xLabel, "Winter") ~ paste("Winter", xLabelTmp), TRUE ~ xLabelTmp),
             yLabelTmp = case_when(str_detect(yLabel, "ontNTS") ~ paste("Upstream Supply", yPer), str_detect(yLabel, "stlouisontOut") ~ paste("Downstream Supply", yPer), str_detect(yLabel, "unstableIce") ~ paste("Unstable Ice Present", yPer), TRUE ~ "NA"),
             yLabel = case_when(str_detect(yLabel, "Spring") ~ paste("Spring", yLabelTmp), str_detect(yLabel, "Summer") ~ paste("Summer", yLabelTmp), str_detect(yLabel, "Fall") ~ paste("Autumn", yLabelTmp), str_detect(yLabel, "Winter") ~ paste("Winter", yLabelTmp), TRUE ~ yLabelTmp)) %>%
      select(-c(xLabelTmp, yLabelTmp, xPer, yPer))
    
    ggplot(data = data, aes(x = X, y = Y, fill = normSev, size = Dataset, shape = Dataset, alpha = Dataset)) +
      ggh4x::facet_grid2(rows = vars(PI), cols = vars(Lookup), scales = "free", independent = "all", labeller = label_wrap_gen(width = 22, multi_line = TRUE)) +
      geom_point(aes(color = "Robust"), stroke = 0.5) +
      scale_fill_gradientn(colors = rev(reds), na.value = blues[1], breaks = c(0, 1), labels = c("Mild Failure", "Severe Failure"), guide = guide_legend(direction = "horizontal", title.position = "bottom")) +
      scale_shape_manual(values = c(24, 21, 23)) +
      scale_alpha_manual(values = c(1.0, 0.75, 0.75)) +
      scale_size_manual(values = c(5, 2, 2)) + 
      scale_color_manual(values = c("gray")) +
      coord_trans(clip = "off") +
      scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) + 
      scale_y_continuous(guide = guide_axis(check.overlap = TRUE)) +
      guides(size = "none", alpha = "none",
             shape = guide_legend(order = 1, override.aes = list(size = 5, color = "gray")),
             color = guide_legend(order = 2, override.aes = list(size = 5, color = blues[1])),
             fill = guide_colorbar()) +
      geom_text(size = 3, family = "Arial", data = pltLabels, aes(label = xLabel), inherit.aes = FALSE, x = Inf, y = -Inf, hjust = 1.05, vjust = -0.5, color = "black") +
      geom_text(size = 3, family = "Arial", data = pltLabels, aes(label = yLabel), inherit.aes = FALSE, x = -Inf, y = Inf, angle = 90, hjust = 1.05, vjust = 1.5, color = "black") +
      theme_bw() +
      theme(text = element_text(color = "black", family = "Arial", size = 18),
            title = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_text(angle = 90, size = 10, hjust = 0.5),
            axis.text.x = element_text(size = 10, hjust = 0.5),
            legend.position = "top",
            legend.title = element_blank(),
            legend.box = "horizontal", 
            legend.margin = margin(),
            legend.key.width = unit(1, "cm"),
            legend.justification = "center",
            panel.spacing = unit(1.25, "lines"))
    
  })
  
  # ---
  # spatially disaggregated pis
  # ---
  
  # statistic selections  
  boxplotDisplay <- reactive({input$boxplot})
  mmSupplyType <- reactive({input$mmboxplot})
  
  spatialPlotData <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    runData <- runData()
    
    data <- annualObjs %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      filter(if (runData != "All SOW Traces") str_detect(SOW, runData) else TRUE) %>%
      left_join(., piLevels, by = "PI") %>%
      mutate(Value = case_when(`PI Group` %in% c("Hydropower", "Wetland Health & Services") ~ Value * 1,
                               `PI Group` %in% c("Coastal Impacts", "Commercial Navigation", "Recreational Boating") ~ Value * -1,
                               TRUE ~ Value))
    
    statFilter <- ifelse(boxplotDisplay() == "Net Annual Average", "annualAverage",
                         ifelse(boxplotDisplay() == "Net Annual Minimum", "annualMinimum",
                                ifelse(boxplotDisplay() == "Net Annual Maximum", "annualMaximum",
                                       ifelse(boxplotDisplay() == "Net Annual Total", "annualTotal", NA))))
    
    mmYears <- ifelse(mmSupplyType() == "All Years", paste0(statFilter, ""), paste0(statFilter, "LowSupply"))
    
    
    data <- data %>%
      filter((PI == "mmArea" & statType == mmYears) | (PI != "mmArea" & statType == statFilter))
    
    data
    
  })
  
  output$candidatePolicyPlots <- renderPlot({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    plt <- spatialPlotData()
    polLevels <- polLevels()
    
    plt <- plt %>%
      filter(`PI Location` != "Aggregate") %>%
      group_by(PI) %>%
      mutate(NormScore = (Value - min(Value)) / (max(Value) - min(Value))) %>%
      ungroup() %>%
      mutate(`PI Name` = factor(`PI Name`, levels = piLevels$`PI Name`),
             # Policy = factor(Policy, levels = c("Plan 2014 Baseline", unique(plt$Policy)[which(unique(plt$Policy) != "Plan 2014 Baseline")])),
             Lookup = factor(Lookup, levels = polLevels),
             Dataset = case_when(str_detect(SOW, "Historic") ~ "Historic",
                                 str_detect(SOW, "Stochastic") ~ "Stochastic",
                                 str_detect(SOW, "ssp") ~ "Climate Scenario",
                                 TRUE ~ "NA"),
             Dataset = factor(Dataset, levels = c("Historic", "Stochastic", "Climate Scenario"))) %>%
      arrange(desc(Dataset))
    
    ggplot(data = plt, aes(x = `PI Name`, y = NormScore, color = Lookup)) +
      facet_wrap(~ `Individual Group`, scales = "free", ncol = 1) +
      geom_boxplot(outlier.shape = NA) +
      geom_point(aes(group = Lookup, fill = Dataset, shape = Dataset, alpha = Dataset), position = position_jitterdodge(jitter.width = 0.3)) +
      theme_bw() +
      scale_shape_manual(values = c(21, 21, 24)) +
      scale_alpha_manual(values = c(1.0, 0.3, 0.3)) +
      scale_fill_manual(values = c("black", "gray", "gray")) +
      scale_color_manual(values = c(first(reds), getBluePal(length(unique(plt$Lookup)) - 1))) +
      ylab("Normalized Score (Better Performance = Higher Scores)") +
      theme(text = element_text(family = "Arial", color = "black", size = 18),
            title = element_blank(),
            axis.title.x = element_blank(),
            axis.text = element_text(size = 16),
            legend.position = "top",
            legend.title = element_blank(),
            legend.box = "vertical", 
            legend.margin = margin()) +
      scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "pf" , " "), width = 12))
    
  })
  
  output$reevaluationTable <- renderDataTable({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    tmp <- spatialPlotData() %>%
      select(searchID, Lookup, SOW, PI, Value)
    
    tmp
  },
  options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    sDom  = '<"top">rt<"bottom">ip',
    width = 1000
  ),
  rownames = FALSE,
  filter = list(position = 'bottom'))
  
  # ---
  # impact zones
  # ---
  
  impactzoneData <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    runData <- runData()
    
    data <- impactZones %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      filter(if (runData != "All SOW Traces") str_detect(SOW, runData) else TRUE) %>%
      pivot_longer(cols = -c(searchID, Lookup, `Lead-Time`, Skill, Policy, SOW, impactZone), names_to = "Impact Category", values_to = "Frequency")
    
  })
  
  output$impactzonePlot <- renderPlot({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    plt <- impactzoneData()
    polLevels <- polLevels()
    
    plt <- plt %>%
      mutate(impactZone = factor(impactZone, levels = ),
             `Impact Category` = factor(`Impact Category`, levels = c("Extreme", "Severe", "Major", "Moderate", "Low Concern")),
             # Policy = factor(Policy, levels = c("Plan 2014 Baseline", unique(plt$Policy)[which(unique(plt$Policy) != "Plan 2014 Baseline")])),
             Lookup = factor(Lookup, levels = polLevels),
             Dataset = case_when(str_detect(SOW, "Historic") ~ "Historic",
                                 str_detect(SOW, "Stochastic") ~ "Stochastic",
                                 str_detect(SOW, "ssp") ~ "Climate Scenario",
                                 TRUE ~ "NA"),
             Dataset = factor(Dataset, levels = c("Historic", "Stochastic", "Climate Scenario"))) %>%
      arrange(desc(Dataset))
    
    ggplot(data = plt, aes(x = `Impact Category`, y = Frequency, color = Lookup)) +
      facet_wrap(~ impactZone, ncol = 2, scales = "free") +
      geom_boxplot(outlier.shape = NA) +
      geom_point(aes(group = Lookup, fill = Dataset, shape = Dataset, alpha = Dataset), position = position_jitterdodge(jitter.width = 0.3)) +
      theme_bw() +
      scale_shape_manual(values = c(21, 21, 24)) +
      scale_alpha_manual(values = c(1.0, 0.3, 0.3)) +
      scale_fill_manual(values = c("black", "gray", "gray")) +
      scale_color_manual(values = c(first(reds), getBluePal(length(unique(plt$Lookup)) - 1))) +
      scale_y_continuous(breaks = c(0.01, 0.1, 1, 5, 10, 25, 50, 100)) +
      coord_trans(y = 'log10') +
      ylab("% of QMs in Category (Log)") +
      theme(text = element_text(family = "Arial", color = "black", size = 18),
            title = element_blank(),
            axis.title.x = element_blank(),
            axis.text = element_text(size = 16),
            legend.position = "top",
            legend.title = element_blank(),
            legend.box = "vertical", 
            legend.margin = margin()) +
      scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "pf" , " "), width = 5))
    
  })
  
  output$impactZoneDescription <- renderDataTable({
    impactZonesContext %>%
      filter(Category == "Context") %>%
      select(`Impact Location`, contains("Narrative"))
  },
  options = list(
    columnDefs = list(list(width = '10px', targets = c(0)), list(width = '250px', targets = c(1, 2, 3, 4))),
    scrollX = TRUE,
    scrollY = TRUE,
    paging = TRUE,
    width = 1000,
    sDom  = '<"top">rt<"bottom">ip'
  ), rownames = FALSE)
  
  output$impactZoneTable <- renderDataTable({
    impactZonesContext %>%
      filter(Category != "Context")
  },
  options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    paging = TRUE,
    width = 1000,
    sDom  = '<"top">rt<"bottom">ip'
  ), rownames = FALSE)
  
  # ---
  # deviations
  # ---
  
  h1Data <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    
    tmp <- h1 %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      group_by(Lookup) %>%
      mutate(`Policy Performance` = case_when(all(Performance == "Pass") ~ "Pass", TRUE ~ "Fail")) %>%
      ungroup() %>%
      mutate(`Policy Performance` = paste0(Lookup, "\n(", `Policy Performance`, ")")) %>%
      select(`Water Level Bin (m)`, `Plan 2014 Exceedances`, `Policy Performance`, `Simulated Exceedances`) %>%
      pivot_wider(id_cols = c(`Water Level Bin (m)`, `Plan 2014 Exceedances`), names_from = `Policy Performance`, values_from = `Simulated Exceedances`) %>%
      rename("Criteria Exceedance" = "Plan 2014 Exceedances") %>%
      select(`Water Level Bin (m)`, `Criteria Exceedance`, contains("Plan 2014"), everything())
    
    tmp
    
  })
  
  output$h1Table<- renderDataTable({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    h1Data()
    
  },
  options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    paging = FALSE,
    width = 1000,
    sDom  = '<"top">rt'
  ), rownames = FALSE)
  
  h2Data <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    
    tmp <- h2 %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      # filter(Policy != "Plan 2014 Baseline") %>%
      group_by(Lookup) %>%
      mutate(`Policy Performance` = case_when(all(Performance == "Pass") ~ "Pass", TRUE ~ "Fail")) %>%
      ungroup() %>%
      mutate(`Policy Performance` = paste0(Lookup, "\n(", `Policy Performance`, ")")) %>%
      select(`Water Level Bin (m)`, `Plan 2014 Exceedances`, `Policy Performance`, `Simulated Exceedances`) %>%
      pivot_wider(id_cols = c(`Water Level Bin (m)`, `Plan 2014 Exceedances`), names_from = `Policy Performance`, values_from = `Simulated Exceedances`) %>%
      rename("Criteria Exceedance" = "Plan 2014 Exceedances") %>%
      select(`Water Level Bin (m)`, `Criteria Exceedance`, contains("Plan 2014"), everything())
    
    tmp
    
  })
  
  output$h2Table<- renderDataTable({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    h2Data()
    
  },
  options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    paging = FALSE,
    width = 1000,
    sDom  = '<"top">rt'
  ), rownames = FALSE)
  
  h3Data <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    
    tmp <- h3 %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      # filter(Policy != "Plan 2014 Baseline") %>%
      group_by(Lookup) %>%
      mutate(`Policy Performance` = case_when(all(Performance == "Pass") ~ "Pass", TRUE ~ "Fail")) %>%
      ungroup() %>%
      mutate(`Policy Performance` = paste0(Lookup, "\n(", `Policy Performance`, ")")) %>%
      select(`Water Level Bin (m)`, `Plan 2014 Exceedances`, `Policy Performance`, `Simulated Exceedances`) %>%
      pivot_wider(id_cols = c(`Water Level Bin (m)`, `Plan 2014 Exceedances`), names_from = `Policy Performance`, values_from = `Simulated Exceedances`) %>%
      rename("Criteria Exceedance" = "Plan 2014 Exceedances") %>%
      select(`Water Level Bin (m)`, `Criteria Exceedance`, contains("Plan 2014"), everything())
    
    
    tmp
    
  })
  
  output$h3Table<- renderDataTable({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    h3Data()
    
  },
  options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    paging = FALSE,
    width = 1000,
    sDom  = '<"top">rt'
  ), rownames = FALSE)
  
  h4Data <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    
    tmp <- h4 %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      replace(is.na(.), 0) %>%
      group_by(Lookup) %>%
      mutate(# Text = paste("Pass:", Pass, "\nFail:", Fail),
        Text = paste(Fail, "/", as.character(Pass + Fail)),
        `Policy Performance` = case_when(all(Fail == 0) ~ "Pass", TRUE ~ "Fail")) %>%
      ungroup() %>%
      mutate(Lookup = paste0(Lookup, "\n(", `Policy Performance`, ")")) %>%
      select(Lookup, Month, Text) %>%
      pivot_wider(id_cols = Month, names_from = Lookup, values_from = Text) %>%
      select(Month, contains("Plan 2014"), everything())
    
    
    tmp
    
  })
  
  output$h4Table<- renderDataTable({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    h4Data()
    
  },
  options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    paging = FALSE,
    width = 1000,
    sDom  = '<"top">rt'
  ), rownames = FALSE)
  
  h6Data <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    
    tmp <- h6 %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      replace(is.na(.), 0) %>%
      group_by(Lookup) %>%
      mutate(Text = paste(Fail, "/", as.character(Pass + Fail)),
             `Policy Performance` = case_when(all(Fail == 0) ~ "Pass", TRUE ~ "Fail")) %>%
      ungroup() %>%
      mutate(Lookup = paste0(Lookup, "\n(", `Policy Performance`, ")")) %>%
      select(Lookup, Month, Text) %>%
      pivot_wider(id_cols = Month, names_from = Lookup, values_from = Text) %>%
      select(Month, contains("Plan 2014"), everything())
    
    tmp
    
  })
  
  output$h6Table<- renderDataTable({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    h6Data()
    
  },
  options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    paging = FALSE,
    width = 1000,
    sDom  = '<"top">rt'
  ), rownames = FALSE)
  
  h7Data <- reactive({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    
    tmp <- h7 %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      replace(is.na(.), 0) %>%
      group_by(Lookup) %>%
      mutate(Text = paste(Fail, "/", as.character(Pass + Fail)),
             `Policy Performance` = case_when(all(Fail == 0) ~ "Pass", TRUE ~ "Fail")) %>%
      ungroup() %>%
      mutate(Lookup = paste0(Lookup, "\n(", `Policy Performance`, ")")) %>%
      select(Lookup, Month, Text) %>%
      pivot_wider(id_cols = Month, names_from = Lookup, values_from = Text) %>%
      select(Month, contains("Plan 2014"), everything())
    
    tmp
    
  })
  
  output$h7Table<- renderDataTable({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    h7Data()
  },
  options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    paging = FALSE,
    width = 1000,
    sDom  = '<"top">rt'
  ), rownames = FALSE)
  
  output$hTable<-renderDataTable({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    h1Data <- h1Data()
    h2Data <- h2Data()
    h3Data <- h3Data()
    h4Data <- h4Data()
    h6Data <- h6Data()
    h7Data <- h7Data()
    
    htab <- data.frame("Criteria" = c("H1", "H2", "H3", "H4", "H6", "H7"))
    htab[1, unlist(lapply(str_split(colnames(h1Data)[str_detect(colnames(h1Data), "Pass|Fail")], "\n"), "[[", 1))] <- str_remove_all(unlist(lapply(str_split(colnames(h1Data)[str_detect(colnames(h1Data), "Pass|Fail")], "\n"), "[[", 2)), "[()]")
    htab[2, unlist(lapply(str_split(colnames(h2Data)[str_detect(colnames(h2Data), "Pass|Fail")], "\n"), "[[", 1))] <- str_remove_all(unlist(lapply(str_split(colnames(h2Data)[str_detect(colnames(h2Data), "Pass|Fail")], "\n"), "[[", 2)), "[()]")
    htab[3, unlist(lapply(str_split(colnames(h3Data)[str_detect(colnames(h3Data), "Pass|Fail")], "\n"), "[[", 1))] <- str_remove_all(unlist(lapply(str_split(colnames(h3Data)[str_detect(colnames(h3Data), "Pass|Fail")], "\n"), "[[", 2)), "[()]")
    htab[4, unlist(lapply(str_split(colnames(h4Data)[str_detect(colnames(h4Data), "Pass|Fail")], "\n"), "[[", 1))] <- str_remove_all(unlist(lapply(str_split(colnames(h4Data)[str_detect(colnames(h4Data), "Pass|Fail")], "\n"), "[[", 2)), "[()]")
    htab[5, unlist(lapply(str_split(colnames(h6Data)[str_detect(colnames(h6Data), "Pass|Fail")], "\n"), "[[", 1))] <- str_remove_all(unlist(lapply(str_split(colnames(h6Data)[str_detect(colnames(h6Data), "Pass|Fail")], "\n"), "[[", 2)), "[()]")
    htab[6, unlist(lapply(str_split(colnames(h7Data)[str_detect(colnames(h7Data), "Pass|Fail")], "\n"), "[[", 1))] <- str_remove_all(unlist(lapply(str_split(colnames(h7Data)[str_detect(colnames(h7Data), "Pass|Fail")], "\n"), "[[", 2)), "[()]")
    
    htab
    
  },
  options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    paging = FALSE,
    width = 1000,
    sDom  = '<"top">rt'
  ), rownames = FALSE)
  
  output$h14Plot<- renderPlot({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    filterCrit <- filterCrit()
    
    tmp <- h14 %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      pivot_longer(cols = -c(searchID, Lookup, `Lead-Time`, Skill, Policy, QM, `High Threshold (m)`, `Low Threshold (m)`), names_to = "Condition", values_to = "Count") %>%
      group_by(Lookup, QM) %>%
      mutate(Freq = Count / sum(Count)) %>%
      ungroup() %>%
      mutate(Condition = factor(Condition, levels = rev(c("2014 Applied - Okay", "58DD Applied - Okay", "2014 Applied - High", "58DD Applied - Still High", "2014 Applied - Low", "58DD Applied - Still Low"))))
    
    ggplot(data = tmp, aes(x = QM, y = Freq, fill = Condition)) +
      facet_wrap(~ Lookup, ncol = 2, scales = "free") +
      geom_col() +
      theme_bw() +
      scale_fill_manual(values = c(getBluePal(2), getRedPal(2), "orange", yellow)) +
      scale_x_continuous(breaks = seq(1, 48, by = 4), limits = c(1, 48)) +
      scale_y_continuous(labels = scales::percent_format()) + 
      xlab("Quarter-Month") +
      ylab("Frequency of Quarter-Months") +
      theme(text = element_text(family = "Arial", color = "black", size = 18),
            title = element_blank(),
            axis.title = element_text(size = 18),
            axis.text.x = element_text(size = 16),
            legend.title = element_blank(),
            legend.position = "top",
            legend.text = element_text(size = 15),
            legend.box = "vertical",
            legend.margin = margin())
    
  })
  
  # ---
  # water level statistics
  # ---
  
  summaryStatistic <- reactive({input$summaryStatistic})
  
  summaryData <- reactive({
    
    filterCrit <- filterCrit()
    runData <- runData()
    
    data <- hydroStats %>%
      inner_join(filterCrit, ., by = c("Lead-Time", "Skill", "Policy")) %>%
      filter(if (runData != "All SOW Traces") str_detect(SOW, runData) else TRUE)
    
    statFilter <- ifelse(summaryStatistic() == "Monthly Mean", "monthlyMean",
                         ifelse(summaryStatistic() == "Monthly Minimum", "monthlyMin",
                                ifelse(summaryStatistic() == "Monthly Maximum", "monthlyMax", NA)))
    
    data <- data %>%
      filter(statType == statFilter) %>%
      mutate(Month = month.name[Month],
             Month = factor(Month, levels = month.name)) %>%
      # pivot_longer(cols = c(ontFlow, ontLevel, ptclaireLevel), names_to = "Variable", values_to = "Value") %>%
      mutate(Variable = case_when(Variable == "ontFlow" ~ "Lake Ontario Release (10*cms)",
                                  Variable == "ontLevel" ~ "Lake Ontario Water Level (m)",
                                  Variable == "ptclaireLevel" ~ "Pointe-Claire Water Level (m)",
                                  TRUE ~ "NA"))
    
    data
    
  })
  
  output$waterLevelStatsPlot <- renderPlot({
    
    if(length(candidatePolicies()) == 0 || is.na(candidatePolicies())) return()
    
    sumstat <- summaryStatistic()
    plt <- summaryData()
    polLevels <- polLevels()
    
    plt <- plt %>%
      mutate(# Policy = factor(Policy, levels = c("Plan 2014 Baseline", unique(plt$Policy)[which(unique(plt$Policy) != "Plan 2014 Baseline")])),
        Lookup = factor(Lookup, levels = polLevels),
        Dataset = case_when(str_detect(SOW, "Historic") ~ "Historic",
                            str_detect(SOW, "Stochastic") ~ "Stochastic",
                            str_detect(SOW, "ssp") ~ "Climate Scenario",
                            TRUE ~ "NA"),
        Dataset = factor(Dataset, levels = c("Historic", "Stochastic", "Climate Scenario"))) %>%
      arrange(desc(Dataset))
    
    ggplot(data = plt, aes(x = Month, y = Value, color = Lookup)) +
      facet_wrap(~ Variable, ncol = 1, scales = "free") +
      geom_boxplot(outlier.shape = NA) +
      geom_point(aes(group = Lookup, fill = Dataset, shape = Dataset, alpha = Dataset), position = position_jitterdodge(jitter.width = 0.3)) +
      scale_shape_manual(values = c(21, 21, 24)) +
      scale_alpha_manual(values = c(1.0, 0.3, 0.3)) +
      scale_fill_manual(values = c("black", "gray", "gray")) +
      scale_color_manual(values = c(first(reds), getBluePal(length(unique(plt$Lookup)) - 1))) +
      theme_bw() +
      ylab("Monthly Summary Statistic Value\n") +
      theme(text = element_text(family = "Arial", color = "black", size = 18),
            axis.title.x = element_blank(),
            axis.text = element_text(size = 16),
            legend.position = "top",
            legend.title = element_blank(),
            legend.box = "vertical", 
            legend.margin = margin())
    
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}

shinyApp(ui, server)

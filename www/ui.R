# Program: ShinyVar (ui.R)
# Developer: Danusorn Lee
# Date created: 11 Jan 2024
library(shiny)
library(vcfR)
library(dplyr)
library(ggvenn)
library(grid)
library(ggplot2)
library(zip)
library(ggVennDiagram)
library(DT)
library(tools)

# App UI ----

## Navigation bar ----
ui <- tagList(
  
  ### Initialize 
  includeCSS("www/css/styles_mod.css"),
  shinyjs::useShinyjs(),
  waiter::useWaiter(), 
  waiter::waiterPreloader(
    html = tagList(
      waiter::spin_loaders(
        id = 2,
        color = "black"
      ),
      br(),
      div("Loading", style = "color:black;")
    ),
    color = "white",
    fadeout = TRUE
  ),
  
  ### Navigation bar
  navbarPage(title = img(src = "picture/ShinyVar-logo2-ezgif.com-resize.gif"), inverse = TRUE, fluid = TRUE, theme = bslib::bs_theme(version = 4), windowTitle = "ShinyVar app",
             # header = div("header test", style = "text-align: center"),
             
             
             
             ### Home ----
             tabPanel(title = "Home",
                      
                      # Sidebar layout
                      sidebarLayout(
                        
                        #### Sidebar panel ----
                        sidebarPanel = sidebarPanel(width = 4,
                                                    
                                                    # "DEBUG zone",
                                                    # verbatimTextOutput("debug"),
                                                    
                                                    div(id = "ui-upload",
                                                        
                                                        h5("Upload Data"),
                                                        
                                                        # Warning message
                                                        div(id = "warn_1",
                                                            icon("fas fa-exclamation-triangle"),
                                                            "Invalid file type (must be VCF file)",
                                                            class = "Warn"
                                                        ),
                                                        h6("1st VCF file:", class = "HeadWithWarn"),
                                                        # Warning message
                                                        div(id = "VCF_1",
                                                            icon("fas fa-exclamation-triangle"),
                                                            "Required",
                                                            class = "Warn"
                                                        ),
                                                        fileInput(inputId = "file1", label = NULL, multiple = FALSE, accept = ".vcf", buttonLabel = "Choose File", placeholder = ""),
                                                        
                                                        # Warning message
                                                        div(id = "warn_2",
                                                            icon("fas fa-exclamation-triangle"),
                                                            "Invalid file type (must be VCF file)",
                                                            class = "Warn"
                                                        ),
                                                        h6("2nd VCF file:", class = "HeadWithWarn"),
                                                        # Warning message
                                                        div(id = "VCF_2",
                                                            icon("fas fa-exclamation-triangle"),
                                                            "Required",
                                                            class = "Warn"
                                                        ),
                                                        fileInput(inputId = "file2", label = NULL, multiple = FALSE, accept = ".vcf", buttonLabel = "Choose File", placeholder = ""),
                                                        
                                                        # Warning message
                                                        div(id = "warn_3",
                                                            icon("fas fa-exclamation-triangle"),
                                                            "Invalid file type (must be VCF file)",
                                                            class = "Warn"
                                                        ),
                                                        h6("3rd VCF file:", class = "HeadWithWarn"),
                                                        # Warning message
                                                        div(id = "VCF_3",
                                                            icon("fas fa-exclamation-triangle"),
                                                            "Required",
                                                            class = "Warn"
                                                        ),
                                                        fileInput(inputId = "file3", label = NULL, multiple = FALSE, accept = ".vcf", buttonLabel = "Choose File", placeholder = ""),
                                                        
                                                        # Warning message
                                                        div(id = "warn_4",
                                                            icon("fas fa-exclamation-triangle"),
                                                            "Invalid file type (must be VCF file)",
                                                            class = "Warn"
                                                        ),
                                                        h6("4th VCF file:", class = "HeadWithWarn"),
                                                        # Warning message
                                                        div(id = "VCF_4",
                                                            icon("fas fa-exclamation-triangle"),
                                                            "Required",
                                                            class = "Warn"
                                                        ),
                                                        fileInput(inputId = "file4", label = NULL, multiple = FALSE, accept = ".vcf", buttonLabel = "Choose File", placeholder = ""),
                                                        
                                                    ),
                                                    
                                                    div(id = "warn_input",
                                                        icon("fas fa-exclamation-triangle"),
                                                        "Input more than 1 file", 
                                                        class = "Warn"
                                                        ),
                                                    ### Run
                                                    br(),
                                                    actionButton(inputId = "run_ShinyVar", label = "Run ShinyVar"),
                                                    
                        ),
                        
                        #### Main panel ----
                        mainPanel = mainPanel(
                          
                          div(
                             id = "ResultAreadiv",
                            # class = "ResultArea",
                            
                            ## Preview
                            ### VCF1
                            div(id = "preview1"),
                            
                            
                            ## Preview
                            ### VCF3
                            div(id = "preview2"),
                            
                            
                            ## Preview
                            ### VCF3
                            div(id = "preview3"),
                            
                            
                            ## Preview
                            ### VCF4
                            div(id = "preview4"),
                            
                        
                            splitLayout(
                              ### Venn plot
                              div(id = "resultvenndiv", class = "BoxArea"),
                              
                              ### Download button
                              div(id = "resultdownloaddiv", class = "BoxArea")
                              
                            ),
                            
                            # br(),
                            ### Final table
                            div(id = "resultdconsensusdiv", class = "BoxArea")
                            
                          )
                          
                        )
                        
                      )
                      
             ),
             
             
             ### Tutorial ----
             tabPanel(title = "Tutorial", 
                      
                      div(style = "min-height: 550px;",
                          
                          h5("Tutorial"),
                          
                          "The analysis workflow of ShinyVar is described below", br(),
                          
                          div(
                            "1) In", strong("\"1st VCF file\""), "section, upload your first VCF file to the app.",
                            br(),
                            "2) In", strong("\"2nd VCF file\""), "section, upload your second VCF file to the app.",
                            br(),
                            "3) In", strong("\"3rd VCF file\""), "section, upload your third VCF file to the app.",
                            br(),
                            "4) In", strong("\"4th VCF file\""), "section, upload your fourth VCF file to the app.",
                            br(),
                            "5) Click", strong("\"Run ShinyVar\""), "button.",
                            br(),
                            "6) Wait 1–5 minutes for the results to be presented."
                          ),
                          

                          br(),
                          strong("Notes:"),
                          br(),
                          strong("*Only VCF files are accepted! Please unzip the files before uploading."),
                          br(),
                          strong("*At least 2 files are required for the analysis."),
                          br(),
                          strong("*Output file names will be renamed using ordered numbers. "), "For example: the 1st VCF file will be saved as VCF1.csv.",
                          br(),
                          strong("*Only_VCF1.csv contains only variants from the first VCF file, VCF1_vs_VCF2.csv contains the intersecting variants between the first and second VCF files."),
                          br(),
                          strong("*Only_VCF1_VCF2_VCF3.csv contains the intersecting variants among the first, second, and third VCF files."),
                          br(),
                          strong("*Only_VCF1_VCF2_VCF3_VCF4.csv contains the intersecting variants from all four VCF files."),
                          br(),
                
                          br(),
                          "The example data can be downloaded by clicking the links below:", br(),
                          
                          tags$li(
                            "Example_VCF: ",
                            downloadLink(outputId = "Example_VCF", label = "link")
                          ),
                          tags$li(
                            "Example_summary: ",
                            downloadLink(outputId = "Example_summary", label = "link")
                          )
                      )
                      
             ),
             ### Citation
             tabPanel(title = "Citation",
                      div(style = "min-height: 550px;",
                          
                          h5("Citation"),
                          
                          "The description of citation below", br(),
                          "In processing", br(),
                          )
             ),
                          
             
             # footer
             footer = div(
               hr(),
               "ShinyVar created by Danusorn Lee",
               br(),
               "Division of Biological Science, Faculty of Science, Prince of Songkla University, Songkhla, Thailand",
               br(),
               "(C) 2024 Copyright: Danusorn Lee, All right reserved",
               class = "Footer"
             )
             
  )
)


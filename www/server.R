# Program: ShinyVar (server.R)
# Developer: Danusorn Lee
# Date created: 11 Jan 2024


# App Server ----

server <- function(input, output){
  
  
  
  # For shinyjs
  
  shinyjs::hide("resultvenndiv")
  shinyjs::hide("resultdownloaddiv")
  shinyjs::hide("resultdegtablediv")
  shinyjs::hide("ResultAreadiv")
  
  # files upload warning VCF_1
  observe(
    
    if (is.null(input$file1)) {
      shinyjs::show("VCF_1")
      shinyjs::hide("warn_1")
    } else {
      shinyjs::hide("VCF_1")
    }
    
  )
  
  # files upload warning VCF_2
  observe(
    
    if (is.null(input$file2)) {
      shinyjs::show("VCF_2")
      shinyjs::hide("warn_2")
    } else {
      shinyjs::hide("VCF_2")
    }
    
  )
  # files upload warning VCF_3
  observe(
    
    if (is.null(input$file3)) {
      shinyjs::show("VCF_3")
      shinyjs::hide("warn_3")
    } else {
      shinyjs::hide("VCF_3")
    }
    
  )
  
  # files upload warning VCF_4
  observe(
    
    if (is.null(input$file4)) {
      shinyjs::show("VCF_4")
      shinyjs::hide("warn_4")
    } else {
      shinyjs::hide("VCF_4")
    }
    
  )

  # Setup ----
  
  ## Create reactive value to store the value in server side
  internal <- reactiveValues()
  list_file <- reactiveValues()
  
  df_var <- function(vcf_file) {
  vcf <- read.vcfR(vcf_file)
  vcf.df <- as.data.frame(vcf@fix)
  if (length(vcf@gt) > 0) {
    vcf.df <- cbind(vcf.df, vcf@gt)
  }
  vcf.df[is.na(vcf.df)] <- "."
  return(vcf.df)
}
  # Upload & Preview variable ----
  ## Preview VCF_1 and store in internal
  observeEvent(eventExpr = input$file1, ignoreInit = TRUE, ignoreNULL = TRUE,
               handlerExpr = {
                 # Show result area if there is hidden
                 req(input$file1)
                 shinyjs::show("ResultAreadiv")
                 # Hide UI if there are show
                 shinyjs::hide("resultvenndiv")
                 shinyjs::hide("resultdownloaddiv")
                 shinyjs::hide("resultdconsensusdiv")
                 
                 # Remove UI if it already exists
                 removeUI(selector = "#div-preview1", immediate = TRUE)
                 
                 # Remove UI of the results if it already exists
                 removeUI(selector = "#div-result_header", immediate = TRUE)
                 removeUI(selector = "#div-result_venn", immediate = TRUE)
                 removeUI(selector = "#div-result_download", immediate = TRUE)
                 removeUI(selector = "#div-result_consensus", immediate = TRUE)
                 
                 
                 # Read the sample information to internal reactive value
                 file1 <- input$file1$datapath
                 if (base::grepl(".vcf", file1)) {
                   shinyjs::hide("warn_1")
                   var_1 <- df_var(file1)
                   internal$file1 <- var_1
                   list_file$file1 <- input$file1$name
                   # For preview data only
                   output$VCF_1_preview <- renderTable(bordered = TRUE, striped = TRUE,
                                                       expr = {
                                                         # Preview first 5 rows
                                                         return(head(internal$file1))
                                                       }
                   )
                 
                   } else {
                    shinyjs::show("warn_1")
                    file1 <- NULL
                    internal$file1 <- "0"
                 }
                 
                 
                 # Insert UI VCF_1
                 insertUI(
                   selector = "#preview1",
                   where = "beforeEnd",
                   ui = div(
                     class = "BoxArea",
                     id = "div-preview1",
                     h5("Preview VCF_1:", input$file1$name),
                     tableOutput("VCF_1_preview")
                   )
                 )
                 
               }
  )
  
  ## Preview VCF_2 and store in internal
  observeEvent(eventExpr = input$file2, ignoreInit = TRUE, ignoreNULL = TRUE,
               handlerExpr = {
                 # Show result area if there is hidden
                 req(input$file2)
                 shinyjs::show("ResultAreadiv")
                 
                 # Hide UI if there are show
                 shinyjs::hide("resultvenndiv")
                 shinyjs::hide("resultdownloaddiv")
                 shinyjs::hide("resultdconsensusdiv")
                 
                 # Remove UI if it already exists
                 removeUI(selector = "#div-preview2", immediate = TRUE)
                 
                 # Remove UI of the results if it already exists
                 removeUI(selector = "#div-result_header", immediate = TRUE)
                 removeUI(selector = "#div-result_venn", immediate = TRUE)
                 removeUI(selector = "#div-result_download", immediate = TRUE)
                 removeUI(selector = "#div-result_consensus", immediate = TRUE)
                 
                 
                 # Read the sample information to internal reactive value
                 file2 <- input$file2$datapath
                 if (base::grepl(".vcf", file2)) {
                   shinyjs::hide("warn_1")
                   var_2 <- df_var(file2)
                   internal$file2 <- var_2
                   list_file$file2 <- input$file2$name
                   # For preview data only
                   output$VCF_2_preview <- renderTable(bordered = TRUE, striped = TRUE,
                                                       expr = {
                                                         # Preview first 5 rows
                                                         return(head(internal$file2))
                                                       }
                   )
                   
                 } else {
                   shinyjs::show("warn_2")
                   file2 <- NULL
                   internal$file2 <- "0"
                 }
                 
                 # Insert UI VCF_2
                 insertUI(
                   selector = "#preview2",
                   where = "beforeEnd",
                   ui = div(
                     class = "BoxArea",
                     id = "div-preview2",
                     h5("Preview VCF_2:", input$file2$name),
                     tableOutput(outputId = "VCF_2_preview")
                   )
                 )

               }
  )

  ## Preview VCF_3 and store in internal
  observeEvent(eventExpr = input$file3, ignoreInit = TRUE, ignoreNULL = TRUE,
               handlerExpr = {
                 # Show result area if there is hidden
                 shinyjs::show("ResultAreadiv")
                 req(input$file3)
                 # Hide UI if there are show
                 shinyjs::hide("resultvenndiv")
                 shinyjs::hide("resultdownloaddiv")
                 shinyjs::hide("resultdconsensusdiv")
                 
                 # Remove UI if it already exists
                 removeUI(selector = "#div-preview3", immediate = TRUE)
                 
                 # Remove UI of the results if it already exists
                 removeUI(selector = "#div-result_header", immediate = TRUE)
                 removeUI(selector = "#div-result_venn", immediate = TRUE)
                 removeUI(selector = "#div-result_download", immediate = TRUE)
                 removeUI(selector = "#div-result_consensus", immediate = TRUE)
                 
                 
                 # Read the sample information to internal reactive value
                 file3 <- input$file3$datapath
                 if (base::grepl(".vcf", file3)) {
                   shinyjs::hide("warn_3")
                   var_3 <- df_var(file3)
                   internal$file3 <- var_3
                   list_file$file3 <- input$file3$name
                   # For preview data only
                   output$VCF_3_preview <- renderTable(bordered = TRUE, striped = TRUE,
                                                       expr = {
                                                         # Preview first 5 rows
                                                         return(head(internal$file3))
                                                       }
                   )
                   
                 } else {
                   shinyjs::show("warn_3")
                   file3 <- NULL
                   internal$file3 <- "0"
                 }
                 
                 
                 # Insert UI VCF_3
                 insertUI(
                   selector = "#preview3",
                   where = "beforeEnd",
                   ui = div(
                     class = "BoxArea",
                     id = "div-preview3",
                     h5("Preview VCF_3:", input$file3$name),
                     tableOutput("VCF_3_preview")
                   )
                 )
                 
               }
  )
  
  ## Preview VCF_4 and store in internal
  observeEvent(eventExpr = input$file4, ignoreInit = TRUE, ignoreNULL = TRUE,
               handlerExpr = {
                 # Show result area if there is hidden
                 shinyjs::show("ResultAreadiv")
                 req(input$file4)
                 # Hide UI if there are show
                 shinyjs::hide("resultvenndiv")
                 shinyjs::hide("resultdownloaddiv")
                 shinyjs::hide("resultdconsensusdiv")
                 
                 # Remove UI if it already exists
                 removeUI(selector = "#div-preview4", immediate = TRUE)
                 
                 # Remove UI of the results if it already exists
                 removeUI(selector = "#div-result_header", immediate = TRUE)
                 removeUI(selector = "#div-result_venn", immediate = TRUE)
                 removeUI(selector = "#div-result_download", immediate = TRUE)
                 removeUI(selector = "#div-result_consensus", immediate = TRUE)
                 
                 
                 # Read the sample information to internal reactive value
                 file4 <- input$file4$datapath
                 if (base::grepl(".vcf", file4)) {
                   shinyjs::hide("warn_4")
                   var_4 <- df_var(file4)
                   internal$file4 <- var_4
                   list_file$file4 <- input$file4$name
                   # For preview data only
                   output$VCF_4_preview <- renderTable(bordered = TRUE, striped = TRUE,
                                                       expr = {
                                                         # Preview first 5 rows
                                                         return(head(internal$file4))
                                                       }
                   )
                   
                 } else {
                   shinyjs::show("warn_4")
                   file4 <- NULL
                   internal$file4 <- "0"
                 }
                 
                 
                 # Insert UI VCF_4
                 insertUI(
                   selector = "#preview4",
                   where = "beforeEnd",
                   ui = div(
                     class = "BoxArea",
                     id = "div-preview4",
                     h5("Preview VCF_4:", input$file4$name),
                     tableOutput("VCF_4_preview")
                   )
                 )
                 
               }
  )
  observe(
    if (is.null(input$file1) && is.null(input$file2) && is.null(input$file3) && is.null(input$file4)){
      shinyjs::disable("run_ShinyVar")
    } else {
      check <- reactiveValuesToList(internal)
      if (length(check) >= 2 && !any(check=="0")) {
        shinyjs::enable("run_ShinyVar")
        shinyjs::hide("warn_input")
      } else if (length(check) >= 2 && any(check=="0")) {
        shinyjs::disable("run_ShinyVar")
      }
    }
  )
  
  
  
      
  # Main pipeline  ----
  ## Function to run if press the button
  Result_ShinyVar <- eventReactive(eventExpr = input$run_ShinyVar, 
                                  valueExpr = {
                                      # Show modal dialog during DEG analysis
                                      # showModal(
                                      #   modalDialog(
                                      #     div(
                                      #       id = "modal-text",
                                      #       h5(style = "text-align: center;", "Please wait"),
                                      #       div(
                                      #         style = "margin-top: 10px;",
                                      #         div(class = "spinner-border")
                                      #       ),
                                      #       style = "text-align: center;"
                                      #     ),
                                      #     title = NULL,
                                      #     size = "s",
                                      #     footer = NULL,
                                      #     easyClose = FALSE,
                                      #     fade = TRUE
                                      #   )
                                      # )
                                      
                                      # Remove UI before showing the results
                                      removeUI(selector = "#preview1", immediate = TRUE)
                                      removeUI(selector = "#preview2", immediate = TRUE)
                                      removeUI(selector = "#preview3", immediate = TRUE)
                                      removeUI(selector = "#preview4", immediate = TRUE)
                                      
                                      removeUI(selector = "#div-result_header", immediate = TRUE)
                                      removeUI(selector = "#div-result_venn", immediate = TRUE)
                                      removeUI(selector = "#div-result_download", immediate = TRUE)
                                      removeUI(selector = "#div-result_consensus", immediate = TRUE)
                                      
                                      # Hide UI if there are showed
                                      shinyjs::hide("resultvenndiv")
                                      shinyjs::hide("resultdownloaddiv")
                                      shinyjs::hide("resultdconsensusdiv")
                                      
                                      # Go to top page
                                      shinyjs::runjs("window.scrollTo({top: 0, behavior: 'smooth'})")
                                      
                                      
                                      # ---- Main Pipeline ---- #
                                      # Merge all VCF to 1 dataframe
                                      cat("Merging all VCF\n")
                                      keys <- c("CHROM", "POS", "REF", "ALT")
                                      data_var <- reactiveValuesToList(internal)
                                      data_name <- reactiveValuesToList(list_file)
                                      list_var <- list()
                                      for (num in 1:length(data_name)) {
                                        df <- data_name[[num]]
                                        list_var[[df]] <- as.data.frame(data_var[[num]])
                                      }
                                      
                                      if (length(list_var) == 3) {
                                        merge_df <- dplyr::full_join(list_var[[1]], list_var[[2]],by= keys, suffix = c("","_2")) %>%
                                          dplyr::full_join(list_var[[3]],by= keys ,suffix = c("_1","_3"))
                                        color = c("blue","red","green")
                                      } else if (length(list_var) == 2) {
                                        merge_df <- dplyr::full_join(list_var[[1]], list_var[[2]],by= keys, suffix = c("_1","_2"))
                                        color =  c("blue","red")
                                      } else if (length(list_var) == 4) {
                                        merge_df <- dplyr::full_join(list_var[[1]], list_var[[2]],by= keys, suffix = c("_1","_2")) %>%
                                          dplyr::full_join(list_var[[3]],by= keys ,suffix = c("","_3")) %>%
                                          dplyr::full_join(list_var[[4]],by= keys ,suffix = c("_3","_4"))
                                        color =  c("blue","red","green","purple")
                                      }
                                      #Sort merge_df
                                      merge_df$POS <- as.numeric(as.character(merge_df$POS))
                                      sort_merge <- dplyr::arrange(merge_df, merge_df$POS)
                                      #Add row number
                                      sort_merge$ROW <-c(1:nrow(sort_merge))
                                      #create final dataframes for consensus file
                                      for (num in 1:length(list_var)) {
                                        #if dataframes is 0 row
                                        if (nrow(list_var[[num]]) ==  0) {
                                          name <- data_name[[num]]
                                          sort_merge[, as.character(name)] <- 0
                                          #if dataframes is more than 0 row
                                        } else if (nrow(list_var[[num]]) > 0) {
                                          df_var <- list_var[[num]]
                                          df_var$POS <- as.numeric(df_var$POS)
                                          #Find intersection of Total_var and each group
                                          df_var <- dplyr::inner_join(sort_merge[keys], df_var[keys], by=keys)
                                          name <- paste("c",data_name[[num]], sep = "_")
                                          #add new column with file name and add value with FALSE
                                          df_var[, as.character(name)] <- 1
                                          sort_merge <- dplyr::full_join(sort_merge, df_var, by=keys)
                                        }
                                      }
                                      #find intersec variant
                                      intersec <- list()
                                      result_all <- list()
                                      list_final <- list()
                                      for (col in data_name) {
                                        new_col <- paste("c",col, sep = "_")
                                        sort_merge[new_col][is.na(sort_merge[new_col])] <- FALSE
                                        intersec[col] <- list(sort_merge[sort_merge[new_col] == TRUE, "ROW"])
                                      }
                                      sort_merge["ROW"] <- NULL
                                      sort_merge <- sort_merge %>% relocate("ID_1", .before ="QUAL_1")
                                      sort_merge[is.na(sort_merge)] <- ""
                                      data_venn <- process_region_data(Venn(intersec))
                                      data <- data_venn[["item"]]
                                      same_data <- 4
                                      #Create file
                                      if (length(data_name) == 4) {
                                        len1 <- ncol(list_var[[1]])-4
                                        len2 <- ncol(list_var[[2]])-4
                                        len3 <- ncol(list_var[[3]])-4
                                        len4 <- ncol(list_var[[4]])-4
                                        list_final$Only_VCF1 <- sort_merge[unlist(data[1]), c(1:(len1 + same_data))]
                                        list_final$Only_VCF2 <- sort_merge[unlist(data[2]), c(1:same_data, (same_data+len1+1):(same_data+len1+len2))]
                                        list_final$Only_VCF3 <- sort_merge[unlist(data[3]), c(1:same_data, (same_data+len1+len2+1):(same_data+len1+len2+len3))]
                                        list_final$Only_VCF4 <- sort_merge[unlist(data[4]), c(1:same_data, (same_data+len1+len2+len3+1):(same_data+len1+len2+len3+len4))]
                                        list_final$Only_VCF1_VCF2 <- sort_merge[unlist(data[5]), c(1:(same_data+len1+len2))]
                                        list_final$Only_VCF1_VCF3 <- sort_merge[unlist(data[6]), c(1:(same_data+len1),(same_data+len1+len2+1):(same_data+len1+len2+len3))]
                                        list_final$Only_VCF1_VCF4 <- sort_merge[unlist(data[7]), c(1:(same_data+len1),(same_data+len1+len2+len3+1):(same_data+len1+len2+len3+len4))]
                                        list_final$Only_VCF2_VCF3 <- sort_merge[unlist(data[6]), c(1:same_data,(same_data+len1+1):(same_data+len1+len2+len3))]
                                        list_final$Only_VCF2_VCF4 <- sort_merge[unlist(data[9]), c(1:same_data,(same_data+len1+1):(same_data+len1+len2), (same_data+len1+len2+len3+1):(same_data+len1+len2+len3+len4))]
                                        list_final$Only_VCF3_VCF4 <- sort_merge[unlist(data[10]), c(1:same_data,(same_data+len1+len2+1):(same_data+len1+len2+len3+len4))]
                                        list_final$Only_VCF1_VCF2_VCF3 <- sort_merge[unlist(data[11]), c(1:(same_data+len1+len2+len3))]
                                        list_final$Only_VCF1_VCF2_VCF4 <- sort_merge[unlist(data[12]), c(1:(same_data+len1+len2),(same_data+len1+len2+len3+1):(same_data+len1+len2+len3+len4))]
                                        list_final$Only_VCF1_VCF3_VCF4 <- sort_merge[unlist(data[13]), c(1:(same_data+len1),(same_data+len1+len2+1):(same_data+len1+len2+len3+len4))]
                                        list_final$Only_VCF2_VCF3_VCF4 <- sort_merge[unlist(data[14]), c(1:same_data,(same_data+len1+1):(same_data+len1+len2+len3+len4))]
                                        list_final$Only_VCF1_VCF2_VCF3_VCF4 <- sort_merge[unlist(data[15]), c(1:(same_data+len1+len2+len3+len4))]
                                        result_all[["consensus"]] <- list_final$Only_VCF1_VCF2_VCF3_VCF4
                                        result_all[["label"]] <- list("VCF1", "VCF2", "VCF3", "VCF4")
                                      } else if (length(data_name) == 3) {
                                        len1 <- ncol(list_var[[1]])-4
                                        len2 <- ncol(list_var[[2]])-4
                                        len3 <- ncol(list_var[[3]])-4
                                        list_final$Only_VCF1 <- sort_merge[unlist(data[1]), c(1:(len1 + same_data))]
                                        list_final$Only_VCF2 <- sort_merge[unlist(data[2]), c(1:same_data, (same_data+len1+1):(same_data+len1+len2))]
                                        list_final$Only_VCF3 <- sort_merge[unlist(data[3]), c(1:same_data, (same_data+len1+len2+1):(same_data+len1+len2+len3))]
                                        list_final$Only_VCF1_VCF2 <- sort_merge[unlist(data[4]), c(1:(same_data+len1+len2))]
                                        list_final$Only_VCF1_VCF3 <- sort_merge[unlist(data[5]), c(1:(same_data+len1),(same_data+len1+len2+1):(same_data+len1+len2+len3))]
                                        list_final$Only_VCF2_VCF3 <- sort_merge[unlist(data[6]), c(1:same_data,(same_data+len1+1):(same_data+len1+len2+len3))]
                                        list_final$Only_VCF1_VCF2_VCF3 <- sort_merge[unlist(data[7]), c(1:22)]
                                        result_all[["consensus"]] <- list_final$Only_VCF1_VCF2_VCF3
                                        result_all[["label"]] <- list("VCF1", "VCF2", "VCF3")
                                      } else if (length(data_name) == 2) {
                                        len1 <- ncol(list_var[[1]])-4
                                        len2 <- ncol(list_var[[2]])-4
                                        list_final$Only_VCF1 <- sort_merge[unlist(data[1]), c(1:(len1 + same_data))]
                                        list_final$Only_VCF2 <- sort_merge[unlist(data[2]), c(1:same_data, (same_data+len1+1):(same_data+len1+len2))]
                                        list_final$Only_VCF1_VCF2 <- sort_merge[unlist(data[3]), c(1:(same_data+len1+len2))]
                                        result_all[["consensus"]] <- list_final$Only_VCF1_VCF2
                                        result_all[["label"]] <- list("VCF1", "VCF2")
                                      }
                                      
                                      result_all[["overlap"]] <- intersec
                                      result_all[["All"]] <- sort_merge
                                      result_all[["color"]] <- color
                                      result_all[["data_venn"]] <- data
                                      result_all[["file"]] <- list_final
                                      
                                      cat("Done\n")
                                      # ---- Remove modal ---- #
                                      removeModal()
                                      
                                      # Show UI if there are hidden
                                      shinyjs::show("resultvenndiv")
                                      shinyjs::show("resultdownloaddiv")
                                      shinyjs::show("resultdconsensusdiv")
                                      
                                      # ---- Return the result of DEG pipeline ---- #
                                      return(result_all)
                                    
                                  }
  )
  
  # Display result ----
  
  ## Render UI ----
  
  ## Display the results after press submit button
  observeEvent(eventExpr = Result_ShinyVar(), ignoreInit = FALSE, ignoreNULL = TRUE,
               handlerExpr = {
                 # Insert UI (Venn plot)
                 insertUI(
                   selector = "#resultvenndiv",
                   ui = div(id = "div-result_venn",
                            h5(strong("Venn diagram"), style = "text-align: center;"),
                            plotOutput(outputId = "venn")
                            
                   )
                 )
                 
                 # Insert UI (Download button)
                 insertUI(
                   selector = "#resultdownloaddiv",
                   ui = div(id = "div-result_download",
                            h5(strong("Files download"), style = "text-align: center;"),
                            
                            div(
                              style = "text-align: center; height: 400px;",
                              # Venn diagram plot
                              downloadButton(outputId = "Result_plot", label = "Venn diagram plot", class = "DownloadButton"),
                              br(),
                              
                              # Intersection
                              downloadButton(outputId = "Result_file_intersec", label = "Consensus result", class = "DownloadButton"),
                              br(),
                              
                              # Summary
                              downloadButton(outputId = "Result_file_summary", label = "Summary result", class = "DownloadButton"),
                              br(),
                              
                            )
                   )
                 )
                 
                 # Insert UI (intersection)
                 insertUI(
                   selector = "#resultdconsensusdiv",
                   ui = div(id = "div-result_consensus",
                            h5(strong("Consensus ShinyVar result")),
                            br(),
                            DT::DTOutput(outputId = "consensus")
                   )
                 )
                 
               }
  )
  
  ## Render plot ----
  
  cache_venn <- eventReactive(
    eventExpr = Result_ShinyVar(),
    valueExpr = {
      label <- Result_ShinyVar()$label
      vennplot <- ggVennDiagram(Result_ShinyVar()$overlap, category.names = label,
                                set_color = Result_ShinyVar()$color,label_alpha = 0, edge_size = 2,
                                label = "count",label_color = "Black", label_size = 6, set_size = 6)
      
      vennplot + scale_fill_distiller(palette = "Reds", direction = 1)  + scale_x_continuous(expand = expansion(mult = .10))
      
    }
  )
  
  ### Venn diagram plot ----
  output$venn <- renderPlot(
    {
      if (!is.null(Result_ShinyVar())) {
        cache_venn()
      }
    }, res = 60, bg = "#00000005"
  )
  
  ## Output table ----
  output$consensus<- DT::renderDT(Result_ShinyVar()$consensus[,1:4], options = list(pageLength = 10, searchHighlight = TRUE, scrollX = TRUE),
                                   rownames = FALSE, selection = "none"
  )
  
  
  # File download function ----
  
  ## Venn diagram ----
  output$Result_plot <- downloadHandler(
    filename = "ShinyVar_venn_plot.png",
    content = function(file){
      png(filename = file, res = 300, width = 1440, height = 1080)
      print(cache_venn())
      dev.off()
    }
  )
  
  ## Consensus result ----
  output$Result_file_intersec <- downloadHandler(
    filename = "consensus_result.csv", 
    content = function(file){
      write.csv(Result_ShinyVar()$consensus,file=file,row.names = FALSE)
    }
  )
  
  output$Result_file_summary <- downloadHandler(
    file = 'summary.zip',
      content = function(file) {
        df_name <- names(Result_ShinyVar()$file)
        tmpdir <- tempdir()
        for (i in 1:length(Result_ShinyVar()$file)) {
          table <- Result_ShinyVar()$file[[i]]
          file_name <- paste0(df_name[[i]], ".csv")
          file_path <-  file.path(tmpdir, file_name)
          write.csv(table,file=file_path,row.names = FALSE)
        }
        All <- names(Result_ShinyVar())
        All <- paste0(All[4], ".csv")
        All <- file.path(tmpdir, All)
        write.csv(Result_ShinyVar()$All,file=All, row.names= FALSE)
        fs <- list.files(path = tmpdir, pattern = ".csv$", full.names=TRUE)
        zip::zipr(zipfile=file, files=fs)
        unlink(x=fs)
      },
      contentType = "application/zip",
    )

  # Example files in tutorial ----
  
  ## Read count table ----
  output$Example_VCF <- downloadHandler(
    filename = "Example_VCF.zip",
    content = function(file){
      file.copy(from = "Example_VCF.zip", to = file)
    }
  )
  
  ## Sample information ----
  output$Example_summary <- downloadHandler(
    filename = "Example_summary.zip",
    content = function(file){
      file.copy(from = 'Example_summary.zip', to = file)
    }
  )
}




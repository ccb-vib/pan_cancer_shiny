########################### DILA Pan Cancer shiny app ##########################
## This app takes as input a list of genes of interest in the first column of a
## CSV format file. The user can select Cancer Type, Major Cell Type and Cell
## Sub Type and the app will generate a bar chart displaying the frequences of 
## the cell types for that cancer type as well as a scatter plot displaying
## correlation of cell frequency against the signature score of genes of
## interest

## load libraries
library("Seurat")
library("dplyr")
library("tibble")
library("ggplot2")
library("patchwork")
library("ggpubr")
library("plotly")
library("shiny")
library("shinydashboard")

############################## User Interface ##################################
ui <- dashboardPage(
        dashboardHeader(title = "Pan Cancer App"),
        dashboardSidebar(
          br(),
          h5("Select Pan Cancer Features ", align = "center"),
          ## choose cancer cell type
          selectInput(inputId = "cancer_type",
                      label =  "Cancer Type:",
                      choices = c("BC" = "bc1",
                                  "CC" = "cc1",
                                  "CRC" = "crc1",
                                  "Endometrial" = "endomet1",
                                  "GBM" = "gbm1",
                                  "HCC" = "hcc1",
                                  "HGSOC" = "hgsoc1",
                                  "HNSCC" = "hnscc1",
                                  "Melanoma" = "mel1",
                                  "NSCLC" = "nsclc1")),
          ## choose major cell type 
          selectInput(inputId = "major_cell_type",
                      label =  "Major Cell Type:",
                      choices = c("B cell" = "b_cell_major2",
                                  "Cancer" = "cancer_major2",
                                  "cDC" = "cdc_major2",
                                  "DC" = "dc_major2",
                                  "Endothelial" = "endo_major2",
                                  "Fibroblast" = "fibro_major2",
                                  "Hepatocyte" = "hepatocyte_major2",
                                  "Mast Cell" = "mast_cell_major2",
                                  "Melanocytes" = "melanocyte_major2",
                                  "Myeloid" = "myeloid_major2",
                                  "pDC" = "pdc_major2",
                                  "T cell" = "t_cell_major2")),
          ## choose cell sub type 
          selectInput(inputId = "cell_sub_type",
                      label =  "Cell Sub Type:",
                      choices = c("Arterial" = "art3",
                                  "CD4+ effector-memory" = "cd4_em3",
                                  "CD4+ follicular helper" = "cd4_fh3",
                                  "CD4+ naive" = "cd4_naive3",
                                  "CD4+ regulatory" = "cd4_reg3",
                                  "CD4+ T-helper 1" = "cd4_t_helper1_3",
                                  "CD4+ T-helper 17" = "cd4_t_helper17_3",
                                  "CD8- gamma-delta" = "cd8_gd_minus3",
                                  "CD8+ EMRA" = "cd8_emra3",
                                  "CD8+ effector memory" = "cd8_em3",
                                  "CD8+ gamma-delta" = "cd8_gd_plus3",
                                  "CD8+ MAIT" = "cd8_mai3t",
                                  "CD8+ naive" = "cd8_naive3",
                                  "CD8+ resident memory" = "cd8_rm3",
                                  "cDC2" = "cdc2_3",
                                  "Fibroblast" = "fibro_sub3",
                                  "Hepatocyte" = "hepatocyte_sub3",
                                  "igA mature" = "igA_mature3",
                                  "igG mature" = "igG_mature3",
                                  "Lymphatic" = "lymphatic3",
                                  "Macro_CCL18" = "macro_CCL18_3",
                                  "Macro_CCL2" = "macro_CCL2_3",
                                  "Macro_CCL2_CCL18" = "macro_CCL2_CCL18_3",
                                  "Macro_CCR2" = "macro_CCR2_3",
                                  "Macro_CCR2_CX3CR1" = "macro_CCR2_CX3CR1_3",
                                  "Macro_CX3CR1" = "macro_CX3CR1_3",
                                  "Macro_CXCL10" = "macro_CXCL10_3",
                                  "Macro_LYVE1" = "macro_LYVE1_3",
                                  "Macro_MT1G" = "macro_MT1G_3",
                                  "Macro_prolif" = "macro_prolif_3",
                                  "Macro_SLC2A1" = "macro_SLC2A1_3",
                                  "MAIT" = "mait_3",
                                  "Mast Cell" = "mast_cell_sub_3",
                                  "Melanocytes" = "melanocyte_sub_3",
                                  "Memory igM-" = "mem_igM_minus_3",
                                  "Memory igM+" = "mem_igM_plus_3",
                                  "Mono_CD14" = "mono_CD14_3",
                                  "Mono_CD16" = "mono_CD16_3",
                                  "Naive mature" = "naive_mature_3",
                                  "Neutrophils" = "neutrophils_3",
                                  "NK cytotoxic" = "nk_cytotoxic_3",
                                  "NK inflammatory" = "nk_inflam_3",
                                  "PCV" = "pcv_3",
                                  "pDC" = "pdc_sub_3",
                                  "Proliferative endothelial" = "prol_endo_3",
                                  "Proliferative T-cell" = "prol_T_3",
                                  "Quiesc_mig_DC" = "quiesc_mig_DC_3",
                                  "Reg Memory" = "reg_mem_3",
                                  "Reg_mig_DC" = "reg_mig_DC_3",
                                  "Stalk cells" = "stalk_cells_3",
                                  "Tip cells" = "tip_cells_3")),
          # textInput(inputId = "text_box",
          #           label = "Paste Genes of Interest Here",
          #           width = "400px",
          #           placeholder = "CD8"
          #           ),
          ## upload genes of interest
          tags$hr(),  ## makes line
          fileInput(inputId = "file1",
                    label = "Upload CSV file containing genes of interest",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          # tags$hr(),  ## makes line
          h5("CSV file parameters", align = "center"),
          radioButtons(inputId = "sep",
                       label = "Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t"),
                       selected = ","),
          radioButtons(inputId = "quote",
                       label = "Quote",
                       choices = c(None = "",
                                   "Double Quote" = '"',
                                   "Single Quote" = "'"),
                       selected = '"'),
          checkboxInput(inputId = "header",
                        label = "Header",
                        value = TRUE) #,
          # tags$hr(), ## makes line
          # checkboxInput(inputId = "log",
          #               label = "Log transform data",
          #               value = FALSE)
          ), ## end dashboardSidebar
        dashboardBody(
          tabsetPanel(type = "tabs",
              tabPanel(title = "Genes of Interest",
                     fluidRow(column(width = 12,
                     # textOutput("cell_sub_type"),
                     h5("This displays the list of uploaded genes of interest"),
                     DT::dataTableOutput("table1"), align = "left"))),
              tabPanel(title = "Metadata",
                     fluidRow(column(width = 12,
                     h5("This displays the metadata of dataset of interest"),
                     DT::dataTableOutput("table2"), align = "left"))),
              tabPanel(title = "Bar Chart",
                       fluidRow(column(width = 12, 
                                       # h5("YOLO"),
                                       plotlyOutput("bar" #, height = 350
                                                    ),
                                       align = "left"))),
              tabPanel(title = "Scatter Plot",
                       fluidRow(column(width = 12, 
                                       #h5("SWAG"),
                                       plotlyOutput("scatter" #, height = 350
                                                    ),
                                       align = "left")))
            # tabPanel(title = "HTML Table",
            #   fluidRow(column(width = 12, #h4("HTML table"),
            #     h5("Note: specific terms can be queried in the search bar."),
            #     DT::dataTableOutput("table"), align = "left"))) #,
            # tabPanel(title = "PCA",
            #     fluidRow(box(title = "PCA plot",
            #                  plotlyOutput("pca", height = 350)),
            #              box(title = "PCA parameters",
            #                  tags$hr(),
            #                  checkboxInput(inputId = "center",
            #                                label = "Center",
            #                                value = TRUE),
            #                  checkboxInput(inputId = "scale",
            #                                label = "Scale",
            #                                value = TRUE)
            #              # ,tags$hr(),
            #              # h5("Download this figure as a .pdf or .png file"),
            #              # downloadButton('downloadData1', 'Download pdf'),
            #              # downloadButton('downloadData2', 'Download png')
            #              ))),
            # tabPanel(title = "tSNE",         
            #   fluidRow(box(title = "tSNE plot",
            #                plotlyOutput("tsne", height = 350)),
            #            box(title = "tSNE parameters",
            #                tags$hr(),
            #                sliderInput(inputId = "perplexity",
            #                            label = "tSNE Perplexity",
            #                            min = 1,
            #                            max = 100,
            #                            value = 3),
            #                sliderInput(inputId = "iterations",
            #                            label = "tSNE Iterations",
            #                            min = 1,
            #                            max = 5000,
            #                            value = 1000),
            #                sliderInput(inputId = "theta",
            #                            label = "tSNE Theta",
            #                            min = 0,
            #                            max = 1,
            #                            value = 0.5)
            #                # ,tags$hr(),
            #                # h5("Download this figure as a .pdf or .png file"),
            #                # downloadButton('downloadData3', 'Download pdf'),
            #                # downloadButton('downloadData4', 'Download png')
            #                ))),
            # tabPanel(title = "Heatmap",
            #   fluidRow(box(title = "Heatmap plot",
            #                plotlyOutput("heatmap1", height = 350)),
            #            box(title = "Heatmap parameters",
            #                tags$hr(),
            #                checkboxInput(inputId = "label1",
            #                              label = "Y-labels",
            #                              value = FALSE),
            #                radioButtons(inputId = "distance1",
            #                             label = "Heatmap Distance Metric",
            #                             choices = c("Euclidean" = "euclidean",
            #                                         "Maximum" = "maximum",
            #                                         "Manhattan" = "manhattan",
            #                                         "Minkowski" = "minkowski",
            #                                         "Canberra" = "canberra"),
            #                             selected = "euclidean")
            #               # ,tags$hr(),
            #               # h5("Download this figure as a .pdf or .png file"),
            #               # downloadButton('downloadData5', 'Download pdf'),
            #               # downloadButton('downloadData6', 'Download png')
            #               ))),
            # tabPanel(title = "Corrplot",
            #   fluidRow(box(title = "Correlation plot",
            #                plotlyOutput("heatmap2", height = 350)),
            #            box(title = "Corrplot parameters",
            #                tags$hr(),
            #                radioButtons(inputId = "distance2",
            #                             label = "Corrplot Distance Metric",
            #                             choices = c("Euclidean" = "euclidean",
            #                                         "Maximum" = "maximum",
            #                                         "Manhattan" = "manhattan",
            #                                         "Minkowski" = "minkowski",
            #                                         "Canberra" = "canberra"),
            #                             selected = "euclidean"),
            #               radioButtons(inputId = "cluster",
            #                             label = "Correlation Metric",
            #                             choices = c("Distance" = "dist",
            #                                         "Pearson" = "pearson",
            #                                         "Spearman" = "spearman",
            #                                         "Kendall" = "kendall"),
            #                             selected = "dist")
            #               # ,tags$hr(),
            #               # h5("Download this figure as a .pdf or .png file"),
            #               # downloadButton('downloadData7', 'Download pdf'),
            #               # downloadButton('downloadData8', 'Download png')
            #               ))),
            # tabPanel(title = "Violin Plot",
            # fluidRow(box(title = "Violin plot",
            #              plotlyOutput("violin", height = 350))
            #          # ,box(title = "Violin plot parameters",
            #          #     tags$hr(),
            #          #     h5("Download this figure as a .pdf or .png file"),
            #          #     downloadButton('downloadData9', 'Download pdf'),
            #          #     downloadButton('downloadData10', 'Download png'))
            #          )
          #)
          ) ## tabsetPanel
      )) ## end dashboardBody + dashboardPage
## end UI
################################## Server ######################################

server <- function(input, output) {

  ## load datasets
  rds1 = readRDS(file = paste0("../test_data/pancancer_1000.rds"))
   
  ############################## READ  Gene of Interest table ##################
   output$table1 <- DT::renderDataTable({
       req(input$file1)
       tryCatch({
          read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote) %>%
          dplyr::rename(ID = names(.)[1]) -> df1
                },
      error = function(e) {## return a safeError if a parsing error occurs
        stop(safeError(e))
      })
      return(df1)
  })
  
  ############################ READ CANCER DATASETS ############################
   
  ## define CalculateSignatureScore function
  CalculateSignatureScore = function (
  object, 
  features, 
  assay = "RNA",
  slot = "data") {
  
  # Get the data from the Seurat object
  data = GetAssayData(object,
                      slot = slot,
                      assay = assay)[features, ]
  
  # If using log-normalized data then perform average in non-log space
  if (slot == "data")
    data = expm1(data)
  
  # Calculate average per cell
  data = data %>% Matrix::colMeans(data)
  
  # Return named vector with the signature score
  return(data)
  }
  
  output$table2 <- DT::renderDataTable({
       req(input$file1)
       tryCatch({
         ## NOTE: this will need to be dynamic to select the right dataset
         
          rds1@meta.data %>%
          tibble::rownames_to_column() %>% 
          dplyr::rename(ID = names(.)[1]) -> meta_data
         
         ## load genes of interest
         read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote) %>%
          dplyr::rename(ID = names(.)[1]) -> df1
          
            ## calculate scores
        as.data.frame(
          CalculateSignatureScore(rds1, as.list(df1)[[1]])) %>%
          tibble::rownames_to_column() %>%
          dplyr::rename(ID = names(.)[1],
                        sig_score = names(.)[2]) %>%
          dplyr::left_join(meta_data, by = "ID") %>%
          dplyr::rename(cell_id = ID,
                        sample_id = `orig.ident`) %>%
          dplyr::mutate(sample_id = as.factor(sample_id),
                        CellType_lev5 = as.factor(CellType_lev5)) -> df2
                },
      error = function(e) {## return a safeError if a parsing error occurs
        stop(safeError(e))
      })
      return(df2)
  })
  
  ################################ BAR CHART ###################################
  
   output$bar <- renderPlotly({
      req(input$file1)
      tryCatch({
        
          rds1@meta.data %>%
          tibble::rownames_to_column() %>% 
          dplyr::rename(ID = names(.)[1]) -> meta_data
         
         ## load genes of interest
         read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote) %>%
          dplyr::rename(ID = names(.)[1]) -> df1
          
            ## calculate scores and plot scores by cell type
        as.data.frame(
          CalculateSignatureScore(rds1, as.list(df1)[[1]])) %>%
          tibble::rownames_to_column() %>%
          dplyr::rename(ID = names(.)[1],
                        sig_score = names(.)[2]) %>%
          dplyr::left_join(meta_data, by = "ID") %>%
          dplyr::rename(cell_id = ID,
                        sample_id = `orig.ident`) %>%
          dplyr::mutate(sample_id = as.factor(sample_id),
                        CellType_lev5 = as.factor(CellType_lev5)) %>%
          dplyr::group_by(CellType) %>%
          dplyr::summarise(mean_sig_score = mean(sig_score)) %>%
          dplyr::arrange(desc(mean_sig_score)) %>%
          dplyr::mutate(CellType = factor(CellType, levels = unique(CellType))) %>%
          ggplot(aes(x = mean_sig_score, y = CellType, fill = CellType)) +
          geom_bar(stat = "identity") +
          coord_flip(expand = TRUE) +
          theme_classic() +
          ylab("Cell Types") +
          xlab("Mean Expression Score") +
          theme(legend.position = "none",
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_viridis_d() +
          ggtitle("Mean Expression Score by Major Cell Type") -> p1
               },
      error = function(e) {## return a safeError if a parsing error occurs
        stop(safeError(e))
      })
      return(p1)
   })

  ################################ SCATTER PLOT ################################
  
  # input_1 <- reactive(get(input$cell_sub_type))
  
  output$scatter <- renderPlotly({
      req(input$file1)
      tryCatch({
        
          rds1@meta.data %>%
          tibble::rownames_to_column() %>% 
          dplyr::rename(ID = names(.)[1]) -> meta_data
         
         ## load genes of interest
         read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote) %>%
          dplyr::rename(ID = names(.)[1]) -> df1
          
         target_cell = "Fibroblast" # Name of cell type of interest
         score = "nCount_RNA" # This should be the name of the signature score
         ncol = 3
         
          ## calculate scores and plot scores by cell type
        as.data.frame(
          CalculateSignatureScore(rds1, as.list(df1)[[1]])) %>%
          tibble::rownames_to_column() %>%
          dplyr::rename(ID = names(.)[1],
                        sig_score = names(.)[2]) %>%
          dplyr::left_join(meta_data, by = "ID") %>%
          dplyr::rename(cell_id = ID,
                        sample_id = `orig.ident`) %>%
          dplyr::mutate(sample_id = as.factor(sample_id),
                        CellType_lev5 = as.factor(CellType_lev5)) -> df2
          
          # Calculate the frequencies
          df2 %>%
            dplyr::group_by(sample_id, TumorType3, CellType_lev5, .drop = FALSE) %>%
            dplyr::summarise(n = n()) %>%
            dplyr::mutate(freq = n/sum(n)) %>%
            dplyr::filter(CellType_lev5 == target_cell) -> res1
          
          # Calculate the average score per sample and merge with the other dataframe
          df2 %>%
            dplyr::group_by(sample_id) %>%
            dplyr::summarise_at(vars(sig_score), mean) %>%
            merge(res1, by = "sample_id") %>%
            dplyr::filter(CellType_lev5 == target_cell) %>%
            ggplot(aes(x = freq, y = sig_score)) +
            geom_point(size = 1, aes(color = TumorType3)) +
            stat_smooth(method = "lm", se = TRUE, fill = "gray", color = "black",
                        formula = y ~ poly(x, 1, raw = TRUE)) +
            ggpubr::stat_cor(method = "spearman") +
            theme_classic() +
            theme(axis.title = element_text(size = 20)) +
            labs(y = "Signature score", x = paste0(target_cell, " proportion")
                 ) -> p2
               },
      error = function(e) {## return a safeError if a parsing error occurs
        stop(safeError(e))
      })
      return(p2)
   })
  
  ############################# download buttons ###############################
  
  ## I might implement functionality to allow users to download sig scores,,,
    
  #  output$downloadData1 <- downloadHandler(
  #   filename = "plot.pdf",
  #   content = function(file) {
  #     orca(input$pca, file = "plot.pdf")
  #     # pdf(file)
  #     # print(output$pca)
  #     # dev.off()
  #     # dev.copy2pdf(file="file")
  #   }
  # )
   
## end server   
}
## Run the application 
shinyApp(ui = ui, server = server)

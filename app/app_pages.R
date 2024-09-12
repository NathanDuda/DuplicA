


cdrom_orthofinder_tab <- function() {
  tabPanel("OrthoFinder Input", 
           titlePanel("CDROM Model"),
           h3("OrthoFinder Input"),
           p("Run the CDROM model using OrthoFinder data."),
           
           fluidRow(
             column(6, shinyFilesButton("expression_file", "Browse: Expression File", '', multiple = F)),
             column(6, textOutput("expression_file_path"))  # Add text output for file path
           ),
           tags$div(style = "margin-bottom: 10px;"), # empty space
           fluidRow(
             column(6, shinyFilesButton("ortho_dir", "Browse: OrthoFinder Folder", '', multiple = F)),
             column(6, textOutput("ortho_dir_path"))  # Add text output for folder path
           ),
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           actionButton("run_cdrom", "Run Model", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           actionButton("toggle_additional_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_additional_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             fluidRow(
               column(6,  # left column 
                      numericInput("min_dups_per_species_pair", tags$span("Min dups per species pair", style = "font-size: 13px;"), value = 10, min = 1),
                      numericInput("exp_cutoff", tags$span("Expression cutoff", style = "font-size: 13px;"), value = 1)
               ),
               column(6,  # right column
                      tags$div(style = "margin-bottom: 20px;"), # empty space
                      checkboxInput("add_pseudofunc", tags$span("Add pseudofunctionalization?", style = "font-size: 13px;"), value = TRUE),
                      checkboxInput("missing_expr_is_pseudo", tags$span("Should genes with missing expression data be considered pseudofunctionalized?", style = "font-size: 13px;"), value = FALSE),
                      checkboxInput("use_absolute_exp", tags$span("Use absolute expression?", style = "font-size: 13px;"), value = FALSE)
               )
             )
           )
  )
}

cdrom_custom_tab <- function() {
  tabPanel("Custom Input", 
           titlePanel("CDROM Model"),
           h3("Custom Input"),
           p("Run the CDROM model using custom input."),
           
           fluidRow(
             column(6, shinyFilesButton("dups_file", "Browse: Duplicate Genes File", '', multiple = F)),
             column(6, textOutput("dups_file_path"))  # Add text output for file path
           ),
           tags$div(style = "margin-bottom: 10px;"), # empty space
           fluidRow(
             column(6, shinyFilesButton("exp_dir", "Browse: Expression Folder", '', multiple = F)),
             column(6, textOutput("exp_dir_path"))  # Add text output for folder path
           ),
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           actionButton("run_custom_cdrom", "Run Model", class = "btn-success"),
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           
           actionButton("toggle_additional_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_additional_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             fluidRow(
               column(6,  # left column 
                      numericInput("min_dups_per_species_pair", tags$span("Min dups per species pair", style = "font-size: 13px;"), value = 10, min = 1),
                      numericInput("exp_cutoff", tags$span("Expression cutoff", style = "font-size: 13px;"), value = 1)
               ),
               column(6,  # right column
                      tags$div(style = "margin-bottom: 10px;"), # empty space
                      checkboxInput("PC", tags$span("Are parental/child copies differentiated?", style = "font-size: 13px;"), value = FALSE),
                      checkboxInput("add_pseudofunc", tags$span("Add pseudofunctionalization?", style = "font-size: 13px;"), value = TRUE),
                      checkboxInput("missing_expr_is_pseudo", tags$span("Should genes with missing expression data be considered pseudofunctionalized?", style = "font-size: 13px;"), value = FALSE),
                      checkboxInput("use_absolute_exp", tags$span("Use absolute expression?", style = "font-size: 13px;"), value = FALSE)
               )
             )
           )
  )
  
}

orthofinder_page <- function() {
  tabPanel("OrthoFinder",
           titlePanel("OrthoFinder"),
           p("Run OrthoFinder."),
           
           # Protein Folder input
           fluidRow(
             column(6, shinyFilesButton("protein_folder", "Browse: Protein Folder", '', multiple = FALSE)),
             column(6, textOutput("protein_folder_path"))
           ),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           actionButton("run_orthofinder", "Run OrthoFinder", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           # Additional options
           actionButton("toggle_orthofinder_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_orthofinder_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             fluidRow(
               column(6,
                      tags$span("Custom Species Tree", style = "font-size: 13px;"),
                      shinyFilesButton("custom_species_tree", 'Browse: Species Tree', tags$span("Use a Custom Species Tree", style = "font-size: 13px;"), multiple = FALSE),
                      tags$div(style = "margin-bottom: 10px;"), # empty space
                      
                      selectInput("sequence_search_method", tags$span("Sequence Search Method", style = "font-size: 13px;"), choices = c("diamond", "blast", "mmseqs2")),
                      selectInput("gene_tree_inference_method", tags$span("Gene Tree Inference Method", style = "font-size: 13px;"), choices = c("dendroblast", "msa")),
                      numericInput("mcl_inflation", tags$span("MCL Inflation", style = "font-size: 13px;"), value = 1.5, step = 0.1),
                      checkboxInput("split_hogs", tags$span("Split HOGs", style = "font-size: 13px;"), value = FALSE),
                      checkboxInput("nuc_not_prot", tags$span("DNA Instead of Protein", style = "font-size: 13px;"), value = FALSE),
                      textOutput("species_tree_path")
               ),
               column(6,
                      conditionalPanel(
                        condition = "input.gene_tree_inference_method == 'msa'",
                        column(6,
                               selectInput("msa_method", tags$span("MSA Method", style = "font-size: 13px;"), choices = c("mafft", "muscle")),
                               selectInput("tree_method", tags$span("MSA Tree Inference Method", style = "font-size: 13px;"), choices = c("fasttree", "raxml")),
                               checkboxInput("msa_trim", tags$span("No MSA Trim", style = "font-size: 13px;"), value = FALSE)
              ))
             )
           )
  ))
}

dnds_page <- function() {
  tabPanel("Dn/Ds", 
           titlePanel("Dn/Ds"),
           p("Get a Dn/Ds ratio for every duplicate pair.")
           
           )
}

segregating_duplicates_page <- function() {
  
  h3('seg')
  
}



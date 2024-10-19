




orthofinder_options <- function() {
  
  fluidRow(
    column(6, shinyFilesButton("protein_folder", "Browse: Protein Folder", '', multiple = FALSE)),
    column(6, textOutput("protein_folder_path"))
  )
  
  
}

orthofinder_additional_options <- function() {
  
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
}



cdrom_options <- function() {
  fluidRow(
    column(6, shinyFilesButton("expression_file", "Browse: Expression File", '', multiple = F)),
    column(6, textOutput("expression_file_path"))  # Add text output for file path
  )
  tags$div(style = "margin-bottom: 10px;")
  fluidRow(
    column(6, shinyFilesButton("ortho_dir", "Browse: OrthoFinder Folder", '', multiple = F)),
    column(6, textOutput("ortho_dir_path"))  # Add text output for folder path
  )
  
}


cdrom_additional_options <- function() {
  
  fluidRow(
    column(6,  # left column 
           numericInput("min_dups_per_species_pair", tags$span("Min dups per species pair", style = "font-size: 13px;"), value = 10, min = 1),
           numericInput("exp_cutoff", tags$span("Expression cutoff", style = "font-size: 13px;"), value = 1, min = 0)
    ),
    column(6,  # right column
           tags$div(style = "margin-bottom: 20px;"), # empty space
           checkboxInput("add_pseudofunc", tags$span("Add pseudofunctionalization?", style = "font-size: 13px;"), value = TRUE),
           checkboxInput("missing_expr_is_pseudo", tags$span("Should genes with missing expression data be considered pseudofunctionalized?", style = "font-size: 13px;"), value = FALSE),
           checkboxInput("use_absolute_exp", tags$span("Use absolute expression?", style = "font-size: 13px;"), value = FALSE)
    )
  )
  
}







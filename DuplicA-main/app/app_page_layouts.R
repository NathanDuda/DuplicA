


source('./app_page_options.R')





cdrom_orthofinder_tab <- function() {
  tabPanel("OrthoFinder Input", 
           titlePanel("CDROM Model"),
           h3("OrthoFinder Input"),
           p("Run the CDROM model using OrthoFinder data."),
           
           cdrom_options(),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           actionButton("run_cdrom", "Run Model", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           actionButton("toggle_additional_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_additional_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             
             cdrom_additional_options()
             
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
           orthofinder_options(),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           actionButton("run_orthofinder", "Run OrthoFinder", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           # Additional options
           actionButton("toggle_orthofinder_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_orthofinder_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             
             
             orthofinder_additional_options()
             
             
  ))
}

dnds_page <- function() {
  tabPanel("Dn/Ds", 
           titlePanel("Dn/Ds"),
           p("Get a Dn/Ds ratio for every duplicate pair."),
           
           # Input: Nucleotide Sequences
           dnds_options(),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
          
           
           # Run Button
           actionButton("run_dnds_pop", "Run Dn/Ds", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           # Additional options (if needed)
           actionButton("toggle_dnds_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_dnds_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             dnds_additional_options()
             )
           
  )
}



segregating_duplicates_page <- function() {
  
  tabPanel("Segregating Duplications",
           titlePanel("Segregating Duplications"),
           p("Run the Segregating Duplications model."),
           
           
           
           # Input: dN/dS Results
           segregating_duplicates_options(),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           # Run Button
           actionButton("run_segregating_duplications", "Run Model", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           # Additional options
           actionButton("toggle_segregating_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_segregating_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             segregating_duplicates_additional_options()
           )
  )

  
  
  
  
  
  # ks_highest_cutoff default is 0.8, mention that: with a high n_individuals, this value needs to be lower
  # when error, suggest lowering ks_highest cutoff bc of high n_individuals??????? test with diff values 
  
  
  # option to only look at duplicates in specific individuals or a duplicates present in range number of individuals 
}

expression_shift_page <- function() {
  
  tabPanel("Expression Shift",
           titlePanel("Expression Shift"),
           p("Run the Expression Shift model."),
           
           # input required files and parameters
           expression_shift_options(),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           # Run Button
           actionButton("run_expression_shift", "Run Model", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           # Additional options
           actionButton("toggle_expression_shift_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_expression_shift_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             expression_shift_additional_options()
           )
  )
}

diversity_divergence_page <- function() {

  tabPanel("Diversity / Divergence Model",
           titlePanel("Diversity / Divergence Model"),
           p("Run the Diversity / Divergence model."),
           
           # input required files and parameters
           diversity_divergence_options(),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           # Run Button
           actionButton("run_expression_shift", "Run Model", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           # Additional options
           actionButton("toggle_expression_shift_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_expression_shift_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             diversity_divergence_additional_options()
           )
  )
  
}
# (OF_dir_path, exp_path, dup_species_list, copy_amount, nondup_species_need_onecopy, rm_exp_lower_than, use_gene_trees, missing_exp_is_zero, lower_beta_lim, upper_beta_lim)

# if fasta doesnt include all individuals, suggest use_all_fastas_in_dir
# use_all_fastas_in_dir, default F



blat_page <- function() {
  tabPanel("BLAT", 
           titlePanel("BLAT"),
           p("Run BLAT to find duplicate genes."),
           
           blat_options(),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           
           # Run Button
           actionButton("run_blat", "Run BLAT", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           # Additional options
           actionButton("toggle_segregating_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_segregating_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             blat_additional_options()
           )
  )
}



blast_page <- function() {
  tabPanel("BLAST", 
           titlePanel("BLAST"),
           p("Run the BLAST model."),
           
           blast_options(),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           # Run Button
           actionButton("run_blast", "Run BLAST", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           
           # Additional options
           actionButton("toggle_segregating_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_segregating_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             blast_additional_options()
           )
  )
}









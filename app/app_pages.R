


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
                      numericInput("exp_cutoff", tags$span("Expression cutoff", style = "font-size: 13px;"), value = 1, min = 0)
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
           p("Get a Dn/Ds ratio for every duplicate pair."),
           
           # Input: Nucleotide Sequences
           fluidRow(
             column(6, shinyFilesButton("nuc_seqs_file", "Browse: Nucleotide Sequences", '', multiple = FALSE),
                       textOutput("nuc_seqs_file")),
             column(6, shinyFilesButton("cnvs_path", "Browse: CNVs File", '', multiple = FALSE),
                       textOutput("cnvs_path"))
           ),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
          
           
           # Run Button
           actionButton("run_dnds_pop", "Run Dn/Ds", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           # Additional options (if needed)
           actionButton("toggle_dnds_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_dnds_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             fluidRow(
               column(6, checkboxInput("use_all_fastas_in_dir", tags$span("Combine all fasta files in the directory?", style = "font-size: 13px;"), value = FALSE)),
               column(6, selectInput("dnds_aligner", tags$span("Protein Alignment Program", style = "font-size: 13px;"), choices = c("muscle")),
               )
             )
           )
  )
}

segregating_duplicates_page <- function() {
  
  tabPanel("Segregating Duplications",
           titlePanel("Segregating Duplications"),
           p("Run the Segregating Duplications model."),
           
           
           
           # Input: dN/dS Results
           fluidRow(
             column(6, shinyFilesButton("cnvs_path", "Browse: CNVs File", '', multiple = FALSE),
                       column(6, textOutput("cnvs_path")),
                       shinyFilesButton("dnds_results_path", "Browse: DnDs Results File", '', multiple = FALSE),
                       column(6, textOutput("dnds_results_path"))),
             column(6, 
                    numericInput("n_individuals", "Number of Individuals", value = NA, step = 1, min = 2), 
                    # CHANGE: SHOULD MIN BE 1?????????????^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                    numericInput("ploidy", "Ploidy", value = 2, min = 1, max = 2, step = 1))
           ),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           # Run Button
           actionButton("run_segregating_duplications", "Run Model", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           # Additional options
           actionButton("toggle_segregating_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_segregating_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             fluidRow(
               column(6,
                      selectInput("full_or_approx", "Full or Approximate Model", choices = c("full", "approximate"))
               ),
               column(6,
                      numericInput("ks_cutoff", "KS Oversaturation Cutoff", value = 0.8, step = 0.1, min = 0, max = 1),
                      checkboxInput("filter_whole_group", "Filter Whole Group", value = TRUE)
               )
             )
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
           fluidRow(
             column(6, 
                    shinyFilesButton("ortho_dir", "Browse: OrthoFinder Folder", '', multiple = FALSE),
                    textOutput("ortho_dir_path"),
                    
                    shinyFilesButton("expression_file", "Browse: Expression File", '', multiple = FALSE),
                    textOutput("expression_file_path")
             ),
             column(6, 
                    selectInput("dup_species_list", "Test for shifted expression in:", 
                                choices = NA, multiple = TRUE)
             )
           ),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           # Run Button
           actionButton("run_expression_shift", "Run Model", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           # Additional options
           actionButton("toggle_expression_shift_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_expression_shift_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             
             fluidRow(
               column(6,
                      checkboxInput("use_gene_trees", 
                                    tags$span("Use gene trees for branch lengths", style = "font-size: 13px;"), 
                                    value = TRUE),
                      selectInput("tissue_list", "Tissues to use:", 
                                  choices = c('All Tissues'), selected = 'All Tissues', multiple = TRUE)
               ),
               column(6,
                      numericInput("copy_amount", 
                                   tags$span("How many duplicate copies in duplicate species", 
                                             style = "font-size: 13px;"), 
                                   value = 2, min = 2),
                      
                      checkboxInput("nondup_species_need_onecopy", 
                                    tags$span("Other species need one copy", style = "font-size: 13px;"), 
                                    value = TRUE),
                      
                      numericInput("exp_cutoff", 
                                   tags$span("Expression cutoff", style = "font-size: 13px;"), 
                                   value = 1, min = 0),
                      
                      checkboxInput("missing_exp_is_zero", 
                                    tags$span("Genes with missing expression data are 0", 
                                              style = "font-size: 13px;"), 
                                    value = FALSE)
               )
             )
           )
  )
}

diversity_divergence_page <- function() {

  tabPanel("Diversity / Divergence Model",
           titlePanel("Diversity / Divergence Model"),
           p("Run the Diversity / Divergence model."),
           
           # input required files and parameters
           fluidRow(
             column(6, 
                    shinyFilesButton("ortho_dir", "Browse: OrthoFinder Folder", '', multiple = FALSE),
                    textOutput("ortho_dir_path"),
                    
                    shinyFilesButton("expression_file", "Browse: Expression File", '', multiple = FALSE),
                    textOutput("expression_file_path")
             ),
             column(6, 
                    selectInput("dup_species_list", "Test for shifts in beta when duplicates are in:", 
                                choices = NA, multiple = TRUE)
             )
           ),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           # Run Button
           actionButton("run_expression_shift", "Run Model", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           # Additional options
           actionButton("toggle_expression_shift_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_expression_shift_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             
             fluidRow(
               column(6,
                      checkboxInput("use_gene_trees", 
                                    tags$span("Use gene trees for branch lengths", style = "font-size: 13px;"), 
                                    value = TRUE),
                      selectInput("tissue_list", "Tissues to use:", 
                                  choices = c('All Tissues'), selected = 'All Tissues', multiple = TRUE)
               ),
               column(6,
                      numericInput("copy_amount", 
                                   tags$span("How many duplicate copies in duplicate species", 
                                             style = "font-size: 13px;"), 
                                   value = 2, min = 2),
                      
                      checkboxInput("nondup_species_need_onecopy", 
                                    tags$span("Other species need one copy", style = "font-size: 13px;"), 
                                    value = TRUE),
                      
                      numericInput("exp_cutoff", 
                                   tags$span("Expression cutoff", style = "font-size: 13px;"), 
                                   value = 1, min = 0),
                      
                      checkboxInput("missing_exp_is_zero", 
                                    tags$span("Genes with missing expression data are 0", 
                                              style = "font-size: 13px;"), 
                                    value = FALSE)
               )
             )
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
           
           fluidRow(
             column(6, 
                    shinyFilesButton("seq_files", "Browse: Sequences File", '', multiple = FALSE),
                    textOutput("seq_files")),
             column(6, 
                    numericInput("copy_number", "Copy Number", value = 2, step = 1, min = 2),
                    selectInput("type", "Type", choices = c('protein', 'nucleotide'), selected = "protein"))
           ),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           
           # Run Button
           actionButton("run_blat", "Run BLAT", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           # Additional options
           actionButton("toggle_segregating_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_segregating_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             fluidRow(
               column(6,
                      numericInput("min_align_length", "Minimum Alignment Length", value = 30, step = 1, min = 0),
                      numericInput("min_percent_identity", "Minimum Percent Identity", value = 90, step = 1, min = 0),
                      numericInput("min_score", "Minimum Score", value = 100, min = 0)
               ),
               column(6,
                      numericInput("min_align_length_percent", "Minimum Alignment Length Percent", value = 90, step = 1, min = 0, max = 100),
                      numericInput("min_gn_length", "Minimum Gene Length", value = 30, step = 1, min = 0)
               )
             )
           )
  )
}



blast_page <- function() {
  tabPanel("BLAST", 
           titlePanel("BLAST"),
           p("Run the BLAST model."),
           
           fluidRow(
             column(6, 
                    shinyFilesButton("seq_files", "Browse: Sequences File", '', multiple = FALSE),
                    textOutput("seq_files")),
             column(6, 
                    numericInput("copy_number", "Copy Number", value = 2, step = 1, min = 1),
                    selectInput("type", "Type", choices = c('protein', 'nucleotide', 'nucleotide (translate to protein)'), selected = "protein"))
           ),
           
           tags$div(style = "margin-bottom: 20px;"), # empty space
           
           # Run Button
           actionButton("run_blast", "Run BLAST", class = "btn-success"),
           
           tags$div(style = "margin-bottom: 40px;"), # empty space
           
           
           # Additional options
           actionButton("toggle_segregating_options", "Additional Options", class = "btn-secondary"),
           
           conditionalPanel(
             condition = "input.toggle_segregating_options % 2 == 1",
             tags$div(style = "margin-top: 20px;"),
             fluidRow(
               column(6,
                      numericInput("min_align_length", "Minimum Alignment Length", value = 30, step = 1, min = 0),
                      numericInput("min_align_length_percent", "Minimum Alignment Length Percent", value = 90, step = 1, min = 0, max = 100),
                      numericInput("min_percent_identity", "Minimum Percent Identity", value = 90, step = 1, min = 0),
                      numericInput("min_gn_length", "Minimum Gene Length", value = 30, step = 1, min = 0)
               ),
               column(6,
                      numericInput("min_score", "Minimum Score", value = 100, min = 0),
                      numericInput("min_bitscore", "Minimum Bitscore", value = 100, min = 0),
                      numericInput("e_value", "E-value", value = 1, min = 0, step = 0.01)
               )
             )
           )
  )
}









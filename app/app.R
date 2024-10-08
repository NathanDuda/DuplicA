library(shiny)
library(shinyFiles)
library(bslib)
library(htmltools)
library(shinyalert)
library(fs) # for file paths 
library(Biostrings) # translate() in translate_nucs_to_prots() in model_OrthoFinder.R
library(tools) # file_ext() in combine_raw_fastas() in model_OrthoFinder.R and file_path_sans_ext() in blat

source('./Scripts/setup.R')
source('./app_functions.R')
source('./app_pages.R')
source('./Scripts/model_OrthoFinder.R')
source('./Scripts/model_DnDs.R')
source('./Scripts/model_Segregating_Duplicates.R')
source('./Scripts/model_EVE.R')
source('./Scripts/model_Blat_Blast.R')

# UI with dark mode theme and sidebar layout
ui <- fluidPage(
  theme = bs_theme(bg = "#222", fg = "white", primary = '#555'),  # Dark mode
  
  
  # Add CSS to ensure buttons are styled and the sidebar is narrower
  add_css_style(),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar-buttons",  # Apply a class to target buttons in the sidebar

          actionButton("select_HOME", "HOME", class = "btn-primary"),
          h5('Detect Duplications'),
          actionButton("select_blat", "BLAT", class = "btn-primary"),
          actionButton("select_blast", "BLAST", class = "btn-primary"),
          actionButton("select_orthofinder", "OrthoFinder", class = "btn-primary"),
          h3("Models"),
          h5('Functional Divergence'),
          actionButton("select_cdrom", "CDROM", class = "btn-primary"),
          actionButton("select_expression_shift", "Expression Shift", class = "btn-primary"), # EVE
          h5('Selective Pressure'),
          actionButton("select_dnds", "Dn/Ds", class = "btn-primary"),
          actionButton("select_diversity_divergence", "Diversity/Divergence", class = "btn-primary"), # EVE
          actionButton("select_segregating_duplicates", "Segregating Duplicates", class = "btn-primary"),
          br()
      )
    ),
    
    mainPanel(
      uiOutput("model_ui")  # Dynamic UI output for the selected model
    )
  )
)

server <- function(input, output, session) {
  
  roots <- c(home = "C:/Users/17735/Downloads")
  
  options(shiny.maxRequestSize = 50 * 1024^2)
  
  # Reactive value to store the current model selection
  current_model <- reactiveVal(NULL)
  
  # Update the UI based on the model selection
  observeEvent(input$select_cdrom, {current_model("cdrom")})
  observeEvent(input$select_cloud, {current_model("cloud")})
  observeEvent(input$select_orthofinder, {current_model("orthofinder")})
  observeEvent(input$select_HOME, {current_model("HOME")})
  observeEvent(input$select_dnds, {current_model("dnds")})
  observeEvent(input$select_segregating_duplicates, {current_model("segregating_duplicates")})
  observeEvent(input$select_expression_shift, {current_model("expression_shift")}) 
  observeEvent(input$select_diversity_divergence, {current_model("diversity_divergence")})
  observeEvent(input$select_blat, {current_model("blat")})
  observeEvent(input$select_blast, {current_model("blast")})
  
  
  # Render UI dynamically based on the selected model
  output$model_ui <- renderUI({
    model <- current_model()
    
    if (is.null(model)) {
      return(h3("Select a model from the sidebar."))  # Default message when no model is selected
    }
    
    switch(model,
           "cdrom" = tabsetPanel(
             cdrom_orthofinder_tab(),
             cdrom_custom_tab()
           ),
           "orthofinder" = tagList(
             orthofinder_page()
           ),
             "dnds" = tagList(
               dnds_page()
           ),
           'segregating_duplicates' = tagList(
             segregating_duplicates_page()
           ), 
           'expression_shift' = tagList(
             expression_shift_page()
           ),
           'diversity_divergence' = tagList(
             diversity_divergence_page()
           ),
           'blat' = tagList(
             blat_page()
           ),
           'blast' = tagList(
             blast_page()
           ),
           "HOME" = tagList(
             titlePanel("DuplicA"),
             tags$div(style = "margin-bottom: 50px;"), # empty space
             
             h4('Select Data Types:'),
             checkboxGroupInput("data_types", "", 
                                choices = c(# multi species
                                            "Expression Data", 
                                            "OrthoFinder Data", 
                                            "Duplicate Gene Data",
                                            'Nucleotide CDS Sequences', 
                                            'Protein Sequences',
                                            # pop gen 
                                            'Population Duplicate Gene Data',
                                            'Population Nucleotide CDS Sequences'),
                                selected = character(0)),
             tags$div(style = "margin-bottom: 40px;"), # empty space
             uiOutput("available_models")
           ),
           h3("Please select a model from the sidebar.")  # Default content when no model is selected
    )
  })
  
  output$available_models <- renderUI({
    data_types <- input$data_types
    models <- c()
    
    # multi species
    if (("Nucleotide Sequences" %in% data_types) | ("Protein Sequences" %in% data_types)) {
      models <- c(models, "OrthoFinder")
      data_types <- c(data_types, 'OrthoFinder Data')
    }
    
    if (("Expression Data" %in% data_types) & ("OrthoFinder Data" %in% data_types)) {
      models <- c(models, "CDROM Model - OrthoFinder Input")
    }
    
    if (("Expression Data" %in% data_types) & ("Duplicate Gene Data" %in% data_types)) {
      models <- c(models, "CDROM Model - Custom Input")  # Assuming "CDROM Model" also works with OrthoFinder Data
    }
    
    # pop gen 
    if (("Population Duplicate Gene Data" %in% data_types) & ("Population Nucleotide CDS Sequences" %in% data_types)) {
      models <- c(models, "DnDs")  
      models <- c(models, "Segregating Duplicates Selection Model") 
    }
    
    
    
    # Output the list of available models
    tagList(
      h4("Available Models:"),
      tags$ul(lapply(models, function(model) {
        tags$li(model)
      }))
    )
  })
      
      
  # Function to handle file selection
  shinyFileChoose(input, "expression_file", roots = roots, session = session)
  shinyFileChoose(input, "ortho_dir", roots = roots, session = session)
  shinyFileChoose(input, "dups_file", roots = roots, session = session)
  shinyFileChoose(input, "exp_dir", roots = roots, session = session)
  shinyFileChoose(input, "protein_folder", roots = roots, session = session)
  
  shinyFileChoose(input, "cnvs_path", roots = roots, session = session)
  shinyFileChoose(input, "nuc_seqs_file", roots = roots, session = session)
  shinyFileChoose(input, "dnds_results_path", roots = roots, session = session)
  shinyFileChoose(input, 'seq_files', roots = roots, session = session)
  
  # Reactive expression to get the selected file's absolute path
  expression_file <- reactive({
    req(input$expression_file)
    parseFilePaths(roots, input$expression_file)$datapath
  })
  
  ortho_dir <- reactive({
    req(input$ortho_dir)
    parseFilePaths(roots, input$ortho_dir)$datapath
  })
  
  dups_file <- reactive({
    req(input$dups_file)
    parseFilePaths(roots, input$dups_file)$datapath
  })
  
  exp_dir <- reactive({
    req(input$exp_dir)
    parseFilePaths(roots, input$exp_dir)$datapath
  })
  
  protein_folder <- reactive({
    req(input$protein_folder)
    parseFilePaths(roots, input$protein_folder)$datapath
  })
  
  
  cnvs_path <- reactive({
    req(input$cnvs_path)
    parseFilePaths(roots, input$cnvs_path)$datapath
  })
  
  nuc_seqs_file <- reactive({
    req(input$nuc_seqs_file)
    parseFilePaths(roots, input$nuc_seqs_file)$datapath
  })

  dnds_results_path <- reactive({
    req(input$dnds_results_path)
    parseFilePaths(roots, input$dnds_results_path)$datapath
  }) 
  
  seq_files <- reactive({
    req(input$seq_files)
    parseFilePaths(roots, input$seq_files)$datapath
  }) 
  
  # show file paths in UI when selected 
  output$expression_file_path <- renderText({expression_file()})
  output$ortho_dir_path <- renderText({ortho_dir()})
  output$dups_file_path <- renderText({dups_file()})
  output$exp_dir_path <- renderText({exp_dir()})
  output$protein_folder_path <- renderText({protein_folder()})
  output$cnvs_path <- renderText({cnvs_path()})
  output$nuc_seqs_file <- renderText({nuc_seqs_file()})
  output$dnds_results_path <- renderText({dnds_results_path()})
  output$seq_files <- renderText({seq_files()})
  
  
  ##########
  # for expression shift model, 
  # Ensure nondup_species_need_onecopy is TRUE when use_gene_trees is TRUE
  observeEvent(input$use_gene_trees, {if (input$use_gene_trees) {updateCheckboxInput(session, "nondup_species_need_onecopy", value = TRUE)}})
  
  # Read the expression file and update choices in tissue_list
  observeEvent(expression_file(), {
    req(expression_file())
    file_path <- expression_file() 
    if (file.exists(file_path)) {
      expression_data <- read.delim(file_path, sep = ' ')
      tissues <- colnames(expression_data)[2:ncol(expression_data)]
      tissues <- c('All Tissues', tissues)
      updateSelectInput(session, "tissue_list", choices = tissues, selected = 'All Tissues')
    }
  })
  
  # Read the orthofinder input and update choices in dup_species_list
  observeEvent(ortho_dir(), {
    req(ortho_dir())
    file_path <- ortho_dir() 
    if (file.exists(file_path)) {
      OF_dir_path <- paste0(OF_dir_path, '/')
      species <- read.delim(paste0(OF_dir_path, "Comparative_Genomics_Statistics/Statistics_PerSpecies.tsv"))
      species <- colnames(species)[2:ncol(species)]
      updateSelectInput(session, "dup_species_list", choices = species)
    }
  })
  ###########
  
  
  
  # Run model when "Run Model" button is clicked
  observeEvent(input$run_cdrom, {
    withProgress(message = 'Running CDROM model...', value = 0, {
      incProgress(0.3, detail = "This may take a few minutes")
      
      result <- run_r_script(
        run_type = "OF",
        script_name = "model_CDROM.R",
        expression_file = expression_file(),
        ortho_dir = dirname(dirname(ortho_dir())),
        dups_file = NULL,
        exp_dir = NULL,
        add_pseudofunc = input$add_pseudofunc,
        missing_expr_is_pseudo = input$missing_expr_is_pseudo,
        exp_cutoff = input$exp_cutoff,
        PC = FALSE,
        min_dups_per_species_pair = input$min_dups_per_species_pair,
        use_absolute_exp = input$use_absolute_exp
      )
      
      incProgress(0.7, detail = "Processing results")
      
      shinyalert("Result", result, type = "info")
    })
  })
  
  observeEvent(input$run_custom_cdrom, {
    withProgress(message = 'Running CDROM model...', value = 0, {
      incProgress(0.3, detail = "This may take a few minutes")
        
      req(input$dups_file)
      req(input$exp_dir)
      
      result <- run_r_script(
        run_type = "custom",
        script_name = "model_CDROM.R",
        dups_file = dups_file(),
        exp_dir = exp_dir(),
        add_pseudofunc = input$add_pseudofunc_custom,
        missing_expr_is_pseudo = input$missing_expr_is_pseudo_custom,
        exp_cutoff = input$exp_cutoff_custom,
        PC = input$PC,
        min_dups_per_species_pair = input$min_dups_per_species_pair_custom,
        use_absolute_exp = input$use_absolute_exp_custom
      )
      
      shinyalert("Success!", result, type = "info")
    })
  })
  
  observeEvent(input$run_expression_shift, {
    withProgress(message = 'Running Expression Shift model...', value = 0, {
      incProgress(0.3, detail = "This may take a few minutes")
      
      result <- main_Expression_Shift(
        OF_dir_path = dirname(dirname(ortho_dir())), 
        exp_path = expression_file(), 
        dup_species_list = input$dup_species_list, 
        tissue_list = input$tissue_list,
        copy_amount = input$copy_amount, 
        nondup_species_need_onecopy = input$nondup_species_need_onecopy, 
        rm_exp_lower_than = input$exp_cutoff, 
        use_gene_trees = input$use_gene_trees, 
        missing_exp_is_zero = input$missing_exp_is_zero
      )
      
      shinyalert("Success!", result, type = "info")
    })
  })
  
  observeEvent(input$run_orthofinder, {
    withProgress(message = 'Running OrthoFinder', value = 0, {
      incProgress(0.3, detail = "This may take a few minutes")
      
      main_OrthoFinder(
        protein_folder = dirname(protein_folder()),
        is_dna = input$nuc_not_prot, 
        method = input$gene_tree_inference_method,
        sequence_search = input$sequence_search_method, 
        msa_program = input$msa_method,
        tree_method = input$tree_method,
        species_tree = input$species_tree_path, 
        mcl_inflation = input$mcl_inflation,
        split_hogs = input$split_hogs, 
        no_msa_trim = input$msa_trim) 
        
      
      shinyalert("Success!", '', type = "info")
    })
  })
  
  observeEvent(input$run_dnds_pop, {
    withProgress(message = 'Calculating Dn/Ds', value = 0, {
      incProgress(0.3, detail = "This may take a few minutes")

      main_pop_dnds(cnvs_path = cnvs_path(),
                      nuc_file_path = nuc_seqs_file(), 
                      aligner = input$dnds_aligner, 
                      replace_dirs = F,# CHANGE DEFAULT TO TRUE WHEN can input directory of nuc and prot files 
                      use_all_fastas_in_dir = input$use_all_fastas_in_dir)
      
      
      shinyalert("Success!", '', type = "info")
    })
  })
  
  observeEvent(input$run_segregating_duplications, {
    withProgress(message = 'Running Segregating Duplications', value = 0, {
      incProgress(0.3, detail = "This may take a few minutes")

      main_Segregating_Duplications(
        cnvs_path = cnvs_path(), 
        dnds_results_path = dnds_results_path(),
        n_individuals = input$n_individuals, 
        ploidy = input$ploidy, 
        ks_oversaturation_cutoff = input$ks_cutoff,
        filter_whole_group = input$filter_whole_group)

      
      shinyalert("Success!", '', type = "info")
    })
  })
  
  observeEvent(input$run_diversity_divergence, {
    withProgress(message = 'Running Diversity / Divergence model...', value = 0, {
      incProgress(0.3, detail = "This may take a few minutes")
      
      # OF_dir_path, exp_path, dup_species_list, copy_amount, nondup_species_need_onecopy, rm_exp_lower_than, use_gene_trees, missing_exp_is_zero, lower_beta_lim, upper_beta_lim) {
      
      result <- main_DiversityDivergence(
        OF_dir_path = dirname(dirname(ortho_dir())), 
        exp_path = expression_file(), 
        dup_species_list = input$dup_species_list, 
        tissue_list = input$tissue_list,
        copy_amount = input$copy_amount, 
        nondup_species_need_onecopy = input$nondup_species_need_onecopy, 
        rm_exp_lower_than = input$exp_cutoff, 
        use_gene_trees = input$use_gene_trees, 
        missing_exp_is_zero = input$missing_exp_is_zero
      )
      
      shinyalert("Success!", result, type = "info")
    })
  })
  
  observeEvent(input$run_blat, {
    withProgress(message = 'Running BLAT...', value = 0, {
      incProgress(0.3, detail = "This may take a few minutes")
      
      result <- main_pop_dup_finder(
        seq_files_path = dirname(seq_files()),
        program = 'blat',
        input_options = list(type = input$type,
                             copy_number = input$copy_number,
                             min_gn_length = input$min_gn_length, 
                             min_align_length = input$min_align_length,
                             min_align_length_percent = input$min_align_length_percent,
                             min_perc_identity = input$min_perc_identity,
                             min_score = input$min_score)
      )
      
      shinyalert("Success!", result, type = "info")
    })
  })
  
  observeEvent(input$run_blast, {
    withProgress(message = 'Running BLAST...', value = 0, {
      incProgress(0.3, detail = "This may take a few minutes")
      
      result <- main_pop_dup_finder(
        seq_files_path = dirname(seq_files()),
        program = 'blast',
        input_options = list(type = input$type,
                             copy_number = input$copy_number,
                             min_gn_length = input$min_gn_length, 
                             min_align_length = input$min_align_length,
                             min_align_length_percent = input$min_align_length_percent,
                             min_perc_identity = input$min_perc_identity,
                             min_score = input$min_score, 
                             min_bitscore = input$min_score,
                             e_value = input$evalue
                          )
      )
      
      shinyalert("Success!", result, type = "info")
    })
  })
  
  
}

shinyApp(ui, server)

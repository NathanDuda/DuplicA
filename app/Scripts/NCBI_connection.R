

install.packages("rentrez")



# get data from a specific paper
library(rentrez)
hox_paper <- entrez_search(db="pubmed", term="10.1038/nature08789[doi]")
hox_paper$ids
hox_data <- entrez_link(db="all", id=hox_paper$ids, dbfrom="pubmed")
hox_data
hox_proteins <- entrez_fetch(db="protein", id=hox_data$links$pubmed_protein, rettype="fasta")


library(rentrez)



# Define the species name
species_name <- "Homo sapiens"  # Replace with your species of interest

# Step 1: Search for all gene IDs for the species in the "gene" database
gene_search <- entrez_search(db = "gene", term = paste0(species_name, "[ORGN]"), retmax = 22000)
all_gene_ids <- gene_search$ids  # List of all gene IDs for the species
cat("Number of genes found:", length(all_gene_ids), "\n")

# Initialize lists to store nucleotide and protein sequences
nucleotide_sequences <- list()
protein_sequences <- list()

# Step 2: Loop through each gene ID and retrieve the nucleotide and protein sequences
for (gene_id in all_gene_ids) {
  # Get gene summary to find related nucleotide and protein accessions
  gene_summary <- entrez_summary(db = "gene", id = gene_id)
  
  # Get the nucleotide accession (if available)
  nuc_accession <- gene_summary$genomic_nucleotide_accession.version
  if (!is.null(nuc_accession)) {
    nuc_seq <- entrez_fetch(db = "nucleotide", id = nuc_accession, rettype = "fasta")
    nucleotide_sequences[[gene_id]] <- nuc_seq
  }
  
  # Get the protein accessions (if available)
  prot_accessions <- gene_summary$protein_accession.version
  if (!is.null(prot_accessions) && length(prot_accessions) > 0) {
    # For each protein accession, fetch the protein sequence
    prot_seqs <- lapply(prot_accessions, function(acc) {
      entrez_fetch(db = "protein", id = acc, rettype = "fasta")
    })
    protein_sequences[[gene_id]] <- prot_seqs
  }
  
  # Pause briefly to comply with NCBI rate limits
  Sys.sleep(0.3)
}

# The results are stored in nucleotide_sequences and protein_sequences lists
cat("Nucleotide sequences retrieved:", length(nucleotide_sequences), "\n")
cat("Protein sequences retrieved:", length(protein_sequences), "\n")




###

BiocManager::install("ropensci/biomartr")
library(biomartr)

# one organism
biomartr::getCollection( db = "refseq", organism = "Saccharomyces cerevisiae")

# all vertebrate genomes
meta.retrieval(kingdom = "vertebrate_mammalian", db = "refseq", type = "genome")



biomartr::getCollection( db = "refseq", organism = "Drosophila melanogaster")


########

# interactive




# Load biomartr
library(biomartr)

# Step 1: List available databases
available_databases <- c("refseq", "genbank", "ensembl", "ensemblgenomes")

# Step 2: Let the user select a database
cat("Select a database:\n")
db_choice <- menu(available_databases, title = "Choose Database")

# Store the chosen database
selected_db <- available_databases[db_choice]

# Step 3: List available organisms for the selected database
# For demonstration, we'll use `meta.retrieval` to list organisms
available_organisms <- getMetaGenomeSummary()$organism_name

# Step 4: Let the user select an organism
cat("Select an organism:\n")
organism_choice <- menu(available_organisms, title = "Choose Organism")

# Store the chosen organism
selected_organism <- available_organisms[organism_choice]

# Step 5: Retrieve the collection data based on user selections
cat(paste("Retrieving data for", selected_organism, "from", selected_db, "...\n"))
collection_data <- getCollection(db = selected_db, organism = selected_organism)

# Display a summary of the retrieved data
print(collection_data)



######


available_databases <- c('refseq', 'genbank', 'ensembl')
selected_database <- 'ensembl'

selected_organisms <- c('Drosophila mojavensis', 'Drosophila melanogaster', 'Drosophila ananassae')
i <- 1



# listGenomes()
getCollection(organism = selected_organisms[i], db = selected_database)


organismAttributes(organism = selected_organisms[i])



coding 
peptide


getAttributes(mart = 'metazoa_mart', dataset = 'dmojavensis_eg_gene')



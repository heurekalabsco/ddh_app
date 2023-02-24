#DOWNLOAD/LOAD DATA-----
# datasets to load when app loads

##app----
gene_surprise <- get_content("gene_surprise", dataset = TRUE)

##search----
search_index <- get_content("search_index", dataset = TRUE)

universal_gene_summary <- get_content("universal_gene_summary", dataset = TRUE)
pathway_genes <- get_content("pathway_genes", dataset = TRUE)
cell_expression_names <- get_content("cell_expression_names", dataset = TRUE)
compound_prism_names <- get_content("compound_prism_names", dataset = TRUE)
compound_hmdb_names. <- get_content("compound_hmdb_names.", dataset = TRUE)

# DO WE NEED THESE?
# universal_prism_long
# universal_prism_meta



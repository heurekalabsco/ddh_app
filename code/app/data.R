#DOWNLOAD/LOAD DATA-----
# datasets to load when app loads

##app----
gene_surprise <- ddh::get_content("gene_surprise", dataset = TRUE)
universal_pathways <- ddh::get_content("universal_pathways", dataset = TRUE) #makes search pathway table
universal_gene_summary <- ddh::get_content("universal_gene_summary", dataset = TRUE)

##search----
search_index <- ddh::get_content("search_index", dataset = TRUE)
#needed for search table; could replace with search_details
compound_hmdb_names <- ddh::get_content("compound_hmdb_names", dataset = TRUE)
compound_prism_names <- ddh::get_content("compound_prism_names", dataset = TRUE)

##methods----
#download the zip file from S3
#unzip into container

# DO WE NEED THESE? # these no longer exist?
# pathway_genes <- ddh:::get_content("pathway_genes", dataset = TRUE)
# cell_expression_names <- ddh:::get_content("cell_expression_names", dataset = TRUE)
# universal_prism_long
# universal_prism_meta



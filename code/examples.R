source(here::here("code", "app_params.R"), local = TRUE)

#need to dynamically switch depending on privateMode
tld <- dplyr::if_else(privateMode == TRUE, "com", "org")
url <- glue::glue('https://www.datadrivenhypothesis.{tld}/')

#need full urls so works in methods, instead of reference url
examples <- glue::glue('<h4>Search for genes</h4>
              <ul>
                <li>A single gene, such as <a href="{url}?show=gene&query=TP53">TP53</a> or <a href="{url}?show=gene&query=BRCA1">BRCA1</a></li>
                <li>An alias, such as <a href="{url}?show=search&query=p160ROCK">p160ROCK</a>, which will find <a href="{url}?show=gene&query=ROCK1">ROCK1</a></li>
                <li>A custom list of genes (separated by commas), such as <a href="{url}?show=search&query=BRCA1,%20BRCA2">BRCA1, BRCA2</a>, which will search <a href="{url}?show=gene_list&query=BRCA1,BRCA2">a custom gene list</a></li>
              </ul>') 

example_pathways <- glue::glue('<h4>Search for pathways</h4>
              <ul>
                <li>A pathway key word, such as <a href="{url}?show=search&query=cholesterol">cholesterol</a>, which will lead you to <a href="{url}?show=pathway&query=15312">GOBP REGULATION OF CHOLESTEROL METABOLIC PROCESS (GSID:15312)</a></li>
              </ul>') 

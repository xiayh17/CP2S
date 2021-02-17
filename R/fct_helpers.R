
# createLink for GeneCards ------------------------------------------------
geneCardsLink <- function(val,name) {
  sprintf('<a href="https://www.genecards.org/cgi-bin/carddisp.pl?gene=%s" target="_blank" class="btn btn-primary">%s</a>',val,name)
}

# createLink for GPL ------------------------------------------------------
gplLink <- function(val,gpl) {
  sprintf('<a href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=%s" target="_blank" class="btn btn-primary">%s</a>',val,gpl)
}

# createLink for NCBI -----------------------------------------------------
ncbiLink <- function(val,ncbi) {
  sprintf('<a href="https://www.ncbi.nlm.nih.gov/gene/?term=%s" target="_blank" class="btn btn-primary">%s</a>',val,ncbi)
}

# createLink for Esemble --------------------------------------------------
ensemblLink <- function(val,ensembl) {
  sprintf('<a href="https://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=%s" target="_blank" class="btn btn-primary">%s</a>',val,ensembl)
}


# group pdata according to key words---------------------------------------
group_pdata <- function(data, g) {
  group_key <- unlist(strsplit(g,split=';'))
  group_list <- list()
  group_key <- group_key
  pattern <- gsub(";","|",g)
  i <- group_key[1]
  for (i in group_key) {
    group_list[[i]] <- data[grepl(i,data)]
  }
  #group_list <- sub("[0-9]","",names(unlist(group_list)))
  group_list <- stringr::str_extract(names(unlist(group_list)), pattern)
  return(group_list)
}

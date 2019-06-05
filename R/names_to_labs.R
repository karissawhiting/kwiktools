library(tidyverse)
library(tools)

# labelling funs-------------------------------------------------------------

# label trans for everything else (helper function)
trans_other_labs <- function(names_vec) {
  result <- stringr::str_replace_all(names_vec, "_", " ") %>%
    tolower(.)

  return(result)
}

# function to transform names of a dataframe to friendly labels
names_to_labs <- function(names_df) {
  names_df_lower <- tools::toTitleCase(names_df)

  labels1 <- ifelse(stringr::str_detect(names_df_lower, "alteration"),
                    trans_gene_labs(names_df_lower),
                    trans_other_labs(names_df_lower))

  return(labels1)
}


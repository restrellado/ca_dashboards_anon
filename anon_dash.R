library(tidyverse) 
library(forcats)

anon_dash <- function(a, b = NULL, c = NULL) {
  # Cleans CA Dashboard ela or math data so district info is anonymized 
  #
  #  Args: 
  #   a: CA Dashboard math or ela dataset as a .txt file 
  #
  #  Returns: A tibble 
  as.list(c(a, b, c)) %>% 
    map(~read_tsv(., col_types = cols(cds = col_character()))) %>% 
    bind_rows() %>% 
    # Change county and district levels to a single level 
    mutate(districtname = "x", countyname = "x") %>% 
    mutate_at(vars(cds, schoolname:countyname), funs(as.factor)) %>%
    mutate(cds = fct_anon(cds), 
           schoolname = fct_anon(schoolname, "school_"), 
           districtname = fct_anon(districtname, "district_"), 
           countyname = fct_anon(countyname, "county_"))
}
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
    map(read_tsv) %>% 
    bind_rows() %>% 
    # Change county and district levels to a single level 
    mutate(districtname = "x", countyname = "x") %>% 
    mutate_at(vars(cds, schoolname:countyname), funs(as.factor)) %>%
    mutate(cds = fct_anon(cds), 
           schoolname = fct_anon(schoolname, "school_"), 
           districtname = fct_anon(districtname, "district_"), 
           countyname = fct_anon(countyname, "county_"))
}

#------------------------------------------------------------------------------

# Try with three datasets 
db <- anon_dash(
  "data/PaloAltoUnifiedmath.txt", 
  "data/SanFranciscoUnifiedmath.txt", 
  "data/LongBeachUnifiedmath.txt"
  ) %>% 
  write_tsv(., "~/Documents/github/data_for_blog/Somedistrictmath_2.txt")

#------------------------------------------------------------------------------

# Try with one dataset 
db <- anon_dash("data/PaloAltoUnifiedmath.txt") %>% 
  write_tsv(., "~/Documents/github/data_for_blog/Somedistrictmath.txt")
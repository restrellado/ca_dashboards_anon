join_ela_math <- function(e_data, m_data, dist_name) {
  library(tidyverse)
  library(forcats)
  
  ela <- read_tsv(e_data) 
  math <- read_tsv(m_data) 
  
  inner_join(ela, math, by = c(
    "schoolname" = "schoolname", "studentgroup" = "studentgroup"
  )) %>% 
    select(districtname = districtname.x, 
           schoolname, 
           studentgroup, 
           currstatus_ela = currstatus.x, 
           currstatus_math = currstatus.y) %>% 
    mutate(districtname = dist_name, 
           schoolname = as.factor(schoolname), 
           schoolname = fct_anon(schoolname, "school_")) 
} 
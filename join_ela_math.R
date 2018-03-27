join_ela_math <- function(e_data, m_data, dist_name) {
  # Joins CA Dashboard ELA and Math data and anonymizes district 
  #  Args: 
  #    e_data: ELA dashboard dataset path
  #    m_data: Math dashboard dataset path 
  #    dist_name: character string to use in anonymization 
  #  Returns: a tibble
  ela <- readr::read_tsv(e_data) 
  math <- readr::read_tsv(m_data) 
  
  dplyr::inner_join(ela, math, by = c(
    "schoolname" = "schoolname", "studentgroup" = "studentgroup"
  )) %>% 
    dplyr::select(districtname = districtname.x, 
                  schoolname, 
                  studentgroup, 
                  currstatus_ela = currstatus.x, 
                  currstatus_math = currstatus.y) %>% 
    dplyr::mutate(districtname = dist_name, 
                  schoolname = as.factor(schoolname), 
                  schoolname = forcats::fct_anon(schoolname, "school_")) 
} 
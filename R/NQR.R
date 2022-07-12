library(magrittr)
library(tidyverse)
df<- list.files("E:/03_Regional Accounts/03D_Data Production/2021/revision_NQR/data",
                pattern="*.csv$",
                recursive=FALSE,
                full.names=TRUE) %>%
  purrr::map(.,data.table::fread) %>%
  data.table::rbindlist(fill=TRUE) %>%
  dplyr::mutate(across(starts_with("20"), as.numeric)) %>%
  tidyr::pivot_longer(cols=starts_with("20"),
                      names_to= "time",
                      values_to= "values")

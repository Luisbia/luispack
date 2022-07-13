library(tidyverse)
library(data.table)


df <-rio::import("U:/03_Regional Accounts/03D_Data Production/2021/BE/matis/t1200_BE_01.xlsx") %>%

  .[, c("type", "table_identifier", "freq", "ref_area","counterpart_area","ref_sector","counterpart_sector",
        "accounting_entry", "sto", "activity", "valuation", "prices","transformation","unit_measure") := tstrsplit(ANNUAL, ".", fixed=TRUE)] %>%

  pivot_longer(cols= !starts_with("ANNUAL"),
               names_to = "time",
               values_to = "values") %>%
  as.data.table() %>%
   .[,ANNUAL:= NULL]

keep_cols <-c ("type","table_identifier","time","values")

select(df,keep_cols)
drop.cols <- grep(("type|table_identifier"), colnames(df))
df_regacc[,(drop.cols):= NULL]

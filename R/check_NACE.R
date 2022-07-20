#' Check NACE
#'
#' @param df the data frame with the data. It should have the following column:TOTAL,A,BTE,F,GTI,GTJ,J,K,L,M_N,KTN,OTQ,RTU,OTU
#' @param ths_abs absolute threshold. Differences below the threshold are ignored.
#' @param ths_rel relative threshold. Differences below the threshold are ignored.
#'
#' @return a dataframe. If all pass are passed it will be empty.
#' @export check_NACE
#'
#' @examples
#'library(tidyverse)
#' df <- regacc_load_all_csv(
#'folder = "D:/data/REGACC/csv",
#'geo = c("AT"),
#'time = "2021-12-01"
#') %>%
#'  dplyr::filter(table_identifier == "T1002") %>%
#'  dplyr::select(ref_area, sto, activity, unit_measure, time_period, obs_value) %>%
#'  tidyr::pivot_wider(
#'    names_from = activity,
#'    values_from = obs_value
#'  ) %>%
#'  rename(TOTAL = "_T")
#'
#'check_NACE(df, ths_abs = 1, ths_rel = 0.5)

check_NACE <- function(df, ths_abs = 1, ths_rel = 0.05) {

 check_packages()

  if ("TOTAL" %in% colnames(df) &&
      "A" %in% colnames(df) &&
      "BTE" %in% colnames(df) &&
      "F" %in% colnames(df) &&
      "GTI" %in% colnames(df) &&
      "J" %in% colnames(df) &&
      "K" %in% colnames(df) &&
      "L" %in% colnames(df) &&
      "M_N" %in% colnames(df) &&
      "OTQ" %in% colnames(df) &&
      "RTU" %in% colnames(df) &&
      "GTJ" %in% colnames(df) &&
      "KTN" %in% colnames(df) &&
      "OTU" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(
        TOTAL_c = A + BTE + F + GTJ + KTN + OTU,
        GTJ_c = GTI + J,
        KTN_c = K + L + M_N,
        OTU_c = OTQ + RTU,
        TOTAL_d = round(TOTAL_c - TOTAL, digits = 0),
        GTJ_d = round(GTJ_c - GTJ, digits = 0),
        KTN_d = round(KTN_c - KTN, digits = 0),
        OTU_d = round(OTU_c - OTU, digits = 0),
        TOTAL_dp = round((TOTAL_d * 100) / TOTAL, 1),
        GTJ_dp = round((GTJ_d * 100) / GTJ, 1),
        KTN_dp = round((KTN_d * 100) / KTN, 1),
        OTU_dp = round((OTU_d * 100) / OTU, 1)
      ) %>%
      dplyr::filter(if_any(ends_with("_d"), ~ abs(.x) > ths_abs)) %>%
      dplyr::filter(if_any(ends_with("_dp"), ~ abs(.x) > ths_rel))

    return(df)

  } else {
    print("incorrect data frame")
  }
}


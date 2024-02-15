


._prevent <- function(age_years,
                      sex,
                      smoke_current,
                      chol_total_mgdl,
                      chol_hdl_mgdl,
                      bp_sys_mmhg,
                      bp_meds,
                      statin_meds,
                      diabetes,
                      bmi,
                      egfr_mlminm2,
                      acr,
                      hba1c,
                      sdi,
                      override_boundary_errors,
                      sex_levels,
                      smoke_current_levels,
                      bp_meds_levels,
                      statin_meds_levels,
                      diabetes_levels,
                      prevent_type,
                      pred_type,
                      year){


  # use something other than length(acr) b/c acr may be null
  ln_acr <- rep(0, length(sex))

  if(prevent_type %in% c("acr", "full")){

    if(is.null(acr)) stop("missing required variable: acr")

    ln_acr <- log(acr)

  }

  if(prevent_type %in% c("hba1c", "full") && is.null(hba1c)){
    stop("missing required variable: hba1c")
  }

  if(prevent_type %in% c("sdi", "full") && is.null(sdi)){
    stop("missing required variable: sdi")
  }

  # coerce categorical data to character values ----

  sex = as.character(sex)
  smoke_current = as.character(smoke_current)
  bp_meds = as.character(bp_meds)
  statin_meds = as.character(statin_meds)
  diabetes = as.character(diabetes)

  # recoding categorical variables ----

  sex_recode <- sex
  sex_recode[sex %in% sex_levels$female] <- 'female'
  sex_recode[sex %in% sex_levels$male] <- 'male'

  smoke_current_recode <-
    as.numeric(tolower(smoke_current) %in% tolower(smoke_current_levels$yes))

  smoke_current_recode[is.na(smoke_current)] <- NA_integer_

  bp_meds_recode <-
    as.numeric(tolower(bp_meds) %in% tolower(bp_meds_levels$yes))

  bp_meds_recode[is.na(bp_meds)] <- NA_integer_

  statin_meds_recode <-
    as.numeric(tolower(statin_meds) %in% tolower(statin_meds_levels$yes))

  statin_meds_recode[is.na(statin_meds)] <- NA_integer_

  diabetes_recode <-
    as.numeric(tolower(diabetes) %in% tolower(diabetes_levels$yes))

  diabetes_recode[is.na(diabetes)] <- NA_integer_

  # transforming data for input into PREVENT equations ----
  ._data <- data.frame(
    sex = sex_recode,
    age_per_10_years = (age_years - 55) / 10,
    age_per_10_years_squared = ((age_years - 55) / 10) ^ 2,
    non_hdl_c_per_1_mmol_l = (chol_total_mgdl - chol_hdl_mgdl) * 0.02586 - 3.5,
    hdl_c_per_0.3_mmol_l = (chol_hdl_mgdl * 0.02586 - 1.3) / 0.3,
    sbp_lt110_per_20_mmhg = (pmin(bp_sys_mmhg, 110) - 110) / 20,
    sbp_gteq110_per_20_mmhg = (pmax(bp_sys_mmhg, 110) - 130) / 20,
    diabetes = diabetes_recode,
    current_smoking = smoke_current_recode,
    bmi_lt30_per_5_kg_m2 = (pmin(bmi, 30) - 25) / 5,
    bmi_gt30_per_5_kg_m2 = (pmax(bmi, 30) - 30) / 5,
    egfr_lt60_per_15_ml = (pmin(egfr_mlminm2, 60) - 60) / -15,
    egfr_gteq60_per_15_ml = (pmax(egfr_mlminm2, 60)  - 90) / -15,
    anti_hypertensive_use = bp_meds_recode,
    statin_use = statin_meds_recode,
    treated_sbp_gteq110_mm_hg_per_20_mm_hg = (pmax(bp_sys_mmhg, 110) - 130) /20 * bp_meds_recode,
    treated_non_hdl_c = ((chol_total_mgdl - chol_hdl_mgdl) * 0.02586 - 3.5) * statin_meds_recode,
    age_per_10yr_x_non_hdl_c_per_1_mmol_l = (age_years - 55) / 10 * ((chol_total_mgdl - chol_hdl_mgdl) * 0.02586 - 3.5),
    age_per_10yr_x_hdl_c_per_0.3_mml_l = (age_years - 55) / 10 * (chol_hdl_mgdl * 0.02586 - 1.3) / 0.3,
    age_per_10yr_x_sbp_gteq110_mm_hg_per_20_mmhg = (age_years - 55)/10 * (pmax(bp_sys_mmhg, 110) - 130) / 20,
    age_per_10yr_x_diabetes = (age_years - 55) / 10 * diabetes_recode,
    age_per_10yr_x_current_smoking = (age_years - 55) / 10 * smoke_current_recode,
    age_per_10yr_x_bmi_gteq30_per_5_kg_m2 = (age_years - 55) / 10 * (pmax(bmi, 30) - 30) / 5,
    age_per_10yr_x_egfr_lt60_per_15_ml = (age_years - 55) / 10 * (pmin(egfr_mlminm2, 60) - 60) / -15,
    # initialize as 0 since these are used conditionally
    sdi_decile_between_4_and_6 = 0,
    sdi_decile_between_7_and_10 = 0,
    ln_acr = 0,
    hba1c_minus_5.3_x_diabetes = 0,
    hba1c_minus_5.3_x_1_minus_diabetes = 0,
    miss_sdi = 0,
    miss_ln_acr = 0,
    miss_hba1c = 0
  )


  if(prevent_type %in% c("full", "sdi")){

    missing_sdi <- is.na(sdi)

    # to match online calculator:
    # (Is this a bug in the online calculator??)
    if(pred_type == 'ascvd'){
      sdi[missing_sdi & sex_recode == 'female'] <- 1
      missing_sdi[sex_recode == 'female'] <- FALSE
    }


    ._data$sdi_decile_between_4_and_6 = as.numeric(sdi %in% c(4, 5, 6))
    ._data$sdi_decile_between_7_and_10 = as.numeric(sdi >= 7)


    if(any(missing_sdi)){

      ._data$sdi_decile_between_4_and_6[missing_sdi] <- 0
      ._data$sdi_decile_between_7_and_10[missing_sdi] <- 0

      ._data$miss_sdi = as.numeric(missing_sdi)

    }

  }

  if(prevent_type %in% c("full", "acr")){

    ._data$ln_acr = ln_acr

    missing_ln_acr <- is.na(acr)

    if(any(missing_ln_acr)){

      ._data$ln_acr[missing_ln_acr] <- 0
      ._data$miss_ln_acr = as.numeric(missing_ln_acr)

    }

  }

  if(prevent_type %in% c("full", "hba1c")){

    ._data$hba1c_minus_5.3_x_diabetes = (hba1c - 5.3) * (diabetes_recode)
    ._data$hba1c_minus_5.3_x_1_minus_diabetes = (hba1c - 5.3) * (1 - diabetes_recode)

    ._data$miss_hba1c <- as.numeric(is.na(hba1c))

    missing_hba1c <- is.na(hba1c)

    if(any(missing_hba1c)){

      ._data$hba1c_minus_5.3_x_diabetes[missing_hba1c] <- 0
      ._data$hba1c_minus_5.3_x_1_minus_diabetes[missing_hba1c] <- 0
      ._data$miss_hba1c = as.numeric(missing_hba1c)

    }

  }

  ._data$._ID <- seq_len(nrow(._data))

  coefs <- generate_prevent_coefs(prevent_type, pred_type, year)

  # risk computation ----

  ._data <- merge(x = ._data,
                  y = coefs,
                  by = c('sex'),
                  all.x = TRUE,
                  all.y = FALSE,
                  sort = FALSE)

  ._data$ind_sum <- with(
    ._data,
    coef_age_per_10_years * age_per_10_years +
      coef_age_per_10_years_squared * age_per_10_years_squared +
      coef_non_hdl_c_per_1_mmol_l * non_hdl_c_per_1_mmol_l +
      coef_hdl_c_per_0.3_mmol_l * hdl_c_per_0.3_mmol_l +
      coef_sbp_lt110_per_20_mmhg * sbp_lt110_per_20_mmhg +
      coef_sbp_gteq110_per_20_mmhg * sbp_gteq110_per_20_mmhg +
      coef_diabetes * diabetes +
      coef_current_smoking * current_smoking +
      coef_bmi_lt30_per_5_kg_m2 * bmi_lt30_per_5_kg_m2 +
      coef_bmi_gt30_per_5_kg_m2 * bmi_gt30_per_5_kg_m2 +
      coef_egfr_lt60_per_15_ml * egfr_lt60_per_15_ml +
      coef_egfr_gteq60_per_15_ml * egfr_gteq60_per_15_ml +
      coef_anti_hypertensive_use * anti_hypertensive_use +
      coef_statin_use * statin_use +
      coef_treated_sbp_gteq110_mm_hg_per_20_mm_hg * treated_sbp_gteq110_mm_hg_per_20_mm_hg +
      coef_treated_non_hdl_c * treated_non_hdl_c +
      coef_age_per_10yr_x_non_hdl_c_per_1_mmol_l * age_per_10yr_x_non_hdl_c_per_1_mmol_l +
      coef_age_per_10yr_x_hdl_c_per_0.3_mml_l * age_per_10yr_x_hdl_c_per_0.3_mml_l +
      coef_age_per_10yr_x_sbp_gteq110_mm_hg_per_20_mmhg * age_per_10yr_x_sbp_gteq110_mm_hg_per_20_mmhg +
      coef_age_per_10yr_x_diabetes * age_per_10yr_x_diabetes +
      coef_age_per_10yr_x_current_smoking * age_per_10yr_x_current_smoking +
      coef_age_per_10yr_x_bmi_gteq30_per_5_kg_m2 * age_per_10yr_x_bmi_gteq30_per_5_kg_m2 +
      coef_age_per_10yr_x_egfr_lt60_per_15_ml * age_per_10yr_x_egfr_lt60_per_15_ml +
      # optional covariates (0 if not used)
      coef_sdi_decile_between_4_and_6 * sdi_decile_between_4_and_6 +
      coef_sdi_decile_between_7_and_10 * sdi_decile_between_7_and_10 +
      coef_ln_acr * ln_acr +
      coef_hba1c_minus_5.3_x_diabetes * hba1c_minus_5.3_x_diabetes +
      coef_hba1c_minus_5.3_x_1_minus_diabetes * hba1c_minus_5.3_x_1_minus_diabetes +
      # missing status for optionals (0 if not used or used & observed)
      coef_miss_sdi * miss_sdi +
      coef_miss_ln_acr * miss_ln_acr +
      coef_miss_hba1c * miss_hba1c +
      const
  )

  output <- with(._data, exp(ind_sum) / (1 + exp(ind_sum)))

  # merge() does not maintain order, so re-order here
  output[order(._data$._ID)]


}


generate_prevent_coefs <- function(prevent_type, pred_type, year){

  .name <- paste(prevent_type, year, sep = '_')
  .coefs <- coefs_prevent[[.name]]

  slice_prevent_coefs(.coefs, pred_type)

}

slice_prevent_coefs <- function(data, pred_type){

  col_women <- paste("women", pred_type, sep = "_")
  col_men <- paste("men", pred_type, sep = "_")

  coef_women <- matrix(data[[col_women]], nrow = 1)
  coef_men <- matrix(data[[col_men]], nrow = 1)

  coef_slice <- rbind(coef_women, coef_men)

  colnames(coef_slice) <- c(data$variable)

  result <- as.data.frame(coef_slice)

  result$sex <- c("female", "male")

  result

}




test_that(
  desc = "Replicate examples from Khan supplemental table S12",
  code = {

    prevent_base_10 <- predict_10yr_cvd_risk(
      sex = c("female", "male"),
      age_years = c(50, 50),
      chol_total_mgdl = c(200, 200),
      chol_hdl_mgdl = c(45, 45),
      bp_sys_mmhg = c(160, 160),
      diabetes = c("yes", "yes"),
      smoke_current = c("no", "no"),
      bmi = c(35, 35),
      egfr_mlminm2 = c(90, 90),
      bp_meds = c("yes", "yes"),
      statin_meds = c("no", "no"),
      equation_version = "Khan_2023",
      prevent_type = 'base'
    )

    expect_equal( round(prevent_base_10, 4), c(.1468,	.1632) )

    prevent_acr_10 <- predict_10yr_cvd_risk(
      sex = c("female", "male"),
      age_years = c(50, 50),
      chol_total_mgdl = c(200, 200),
      chol_hdl_mgdl = c(45, 45),
      bp_sys_mmhg = c(160, 160),
      diabetes = c("yes", "yes"),
      smoke_current = c("no", "no"),
      bmi = c(35, 35),
      egfr_mlminm2 = c(90, 90),
      bp_meds = c("yes", "yes"),
      statin_meds = c("no", "no"),
      equation_version = "Khan_2023",
      acr = c(40, 40),
      prevent_type = 'acr'
    )

    expect_equal( round(prevent_acr_10, 4), c(.1599,	.1718) )

    prevent_hba1c_10 <- predict_10yr_cvd_risk(
      sex = c("female", "male"),
      age_years = c(50, 50),
      chol_total_mgdl = c(200, 200),
      chol_hdl_mgdl = c(45, 45),
      bp_sys_mmhg = c(160, 160),
      diabetes = c("yes", "yes"),
      smoke_current = c("no", "no"),
      bmi = c(35, 35),
      egfr_mlminm2 = c(90, 90),
      bp_meds = c("yes", "yes"),
      statin_meds = c("no", "no"),
      equation_version = "Khan_2023",
      hba1c = c(7.5, 7.5),
      prevent_type = 'hba1c'
    )

    expect_equal( round(prevent_hba1c_10, 4), c(.1359,	.1551) )

    prevent_sdi_10 <- predict_10yr_cvd_risk(
      sex = c("female", "male"),
      age_years = c(50, 50),
      chol_total_mgdl = c(200, 200),
      chol_hdl_mgdl = c(45, 45),
      bp_sys_mmhg = c(160, 160),
      diabetes = c("yes", "yes"),
      smoke_current = c("no", "no"),
      bmi = c(35, 35),
      egfr_mlminm2 = c(90, 90),
      bp_meds = c("yes", "yes"),
      statin_meds = c("no", "no"),
      equation_version = "Khan_2023",
      sdi = c(8, 8),
      prevent_type = 'sdi'
    )

    expect_equal( round(prevent_sdi_10, 4), c(.1565,	.1823) )

    prevent_full_10 <- predict_10yr_cvd_risk(
      sex = c("female", "male"),
      age_years = c(50, 50),
      chol_total_mgdl = c(200, 200),
      chol_hdl_mgdl = c(45, 45),
      bp_sys_mmhg = c(160, 160),
      diabetes = c("yes", "yes"),
      smoke_current = c("no", "no"),
      bmi = c(35, 35),
      egfr_mlminm2 = c(90, 90),
      bp_meds = c("yes", "yes"),
      statin_meds = c("no", "no"),
      equation_version = "Khan_2023",
      acr = c(40, 40),
      hba1c = c(7.5, 7.5),
      sdi = c(8, 8),
      prevent_type = 'full'
    )

    expect_equal( round(prevent_full_10, 4), c(.1560,	.1808))

  }
)

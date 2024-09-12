
test_that(
  desc = "Replicate examples from Khan supplemental table S12",
  code = {

    prevent_base_30 <- predict_30yr_ascvd_risk(
      sex = c("female", "male"),
      age_years = c(50, 50),
      chol_total_mgdl = c(200, 200),
      chol_hdl_mgdl = c(45, 45),
      bp_sys_mmhg = c(160, 160),
      diabetes = c("yes", "yes"),
      smoke_current = c("no", "no"),
      # bmi = c(35, 35),
      egfr_mlminm2 = c(90, 90),
      bp_meds = c("yes", "yes"),
      statin_meds = c("no", "no"),
      equation_version = "Khan_2023",
      prevent_type = 'base'
    )

    expect_equal( round(prevent_base_30, 3), c(0.354, 0.349) )

    prevent_acr_30 <- predict_30yr_ascvd_risk(
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

    expect_equal( round(prevent_acr_30, 3), c(.363, .355) )

    prevent_hba1c_30 <- predict_30yr_ascvd_risk(
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

    expect_equal( round(prevent_hba1c_30, 3), c(.322, .317) )

    prevent_sdi_30 <- predict_30yr_ascvd_risk(
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

    expect_equal( round(prevent_sdi_30, 4), c(.367, .361) )

    prevent_full_30 <- predict_30yr_ascvd_risk(
      sex = c("female", "male"),
      age_years = c(50, 50),
      chol_total_mgdl = c(200, 200),
      chol_hdl_mgdl = c(45, 45),
      bp_sys_mmhg = c(160, 160),
      bmi = c(35, 35),
      egfr_mlminm2 = c(90, 90),
      diabetes = c("yes", "yes"),
      smoke_current = c("no", "no"),
      bp_meds = c("yes", "yes"),
      statin_meds = c("no", "no"),
      equation_version = "Khan_2023",
      acr = c(40, 40),
      hba1c = c(7.5, 7.5),
      sdi = c(8, 8),
      prevent_type = 'full'
    )

    expect_equal( round(prevent_full_30, 3), c(.344,	.338))

    # prevent_full_missing_sdi <- predict_30yr_ascvd_risk(
    #   sex = c("female", "male"),
    #   age_years = c(56, 56),
    #   chol_total_mgdl = c(231, 231),
    #   chol_hdl_mgdl = c(46, 46),
    #   bp_sys_mmhg = c(150, 150),
    #   bmi = c(35, 35),
    #   egfr_mlminm2 = c(90, 90),
    #   diabetes = c("yes", "yes"),
    #   smoke_current = c("no", "no"),
    #   bp_meds = c("yes", "yes"),
    #   statin_meds = c("no", "no"),
    #   equation_version = "Khan_2023",
    #   acr = c(900, 900),
    #   hba1c = c(3, 3),
    #   sdi = c(NA, NA),
    #   prevent_type = 'full'
    # )
    #
    # expect_equal( round(prevent_full_missing_sdi, 3), c(.077, .116))
    #
    #
    # prevent_full_missing_sdi <- predict_30yr_ascvd_risk(
    #   sex = c("female", "male"),
    #   age_years = c(50, 50),
    #   chol_total_mgdl = c(200, 200),
    #   chol_hdl_mgdl = c(46, 46),
    #   bp_sys_mmhg = c(169, 169),
    #   bmi = c(35, 35),
    #   egfr_mlminm2 = c(90, 90),
    #   diabetes = c("no", "no"),
    #   smoke_current = c("no", "no"),
    #   bp_meds = c("no", "no"),
    #   statin_meds = c("no", "no"),
    #   equation_version = "Khan_2023",
    #   acr = c(40, 40),
    #   hba1c = c(3, 3),
    #   sdi = c(NA, NA),
    #   prevent_type = 'full'
    # )
    #
    # expect_equal( round(prevent_full_missing_sdi, 3), c(.026, .041))

  }
)




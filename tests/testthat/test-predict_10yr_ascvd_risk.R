
# pooled cohort risk equations from 2013

test_that(
  desc = "Goff supplement examples are correct",
  code = {

    sex = c('female', 'female', 'male', 'male')
    race = c('black', 'white', 'black', 'white')
    # 55 years of age
    age_years = rep(55, times = 4)
    # total cholesterol 213 mg/dL
    chol_total_mgdl = rep(213, times = 4)
    # HDL–C 50 mg/dL
    chol_hdl_mgdl = rep(50, times = 4)
    # untreated systolic BP 120 mm Hg
    bp_sys_mmhg = rep(120, times = 4)
    bp_meds = rep('no', times = 4)
    # nonsmoker
    smoke_current = rep('no', times = 4)
    # without diabetes
    diabetes = rep('no', times = 4)

    pcr_probs <- predict_10yr_ascvd_risk(
      sex = sex,
      race = race,
      age_years = age_years,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      smoke_current = smoke_current,
      diabetes = diabetes
    )

    # to match online format
    pcr_perc_rounded <- round(pcr_probs * 100, 1)

    expect_equal(
      pcr_perc_rounded,
      c(3.0, 2.1, 6.1, 5.4)
    )

  }

)

test_that(
  desc = 'some additional random examples work',
  code = {
    expect_equal(
      round(
        100 * predict_10yr_ascvd_risk(
          age_years = 54,
          sex = 'male',
          race = 'white',
          smoke_current = 'yes',
          chol_total_mgdl = 170,
          chol_hdl_mgdl = 50,
          bp_sys_mmhg = 157,
          bp_meds = 'no',
          diabetes = 'yes'
        ),
        digits = 1
      ),
      20.8
    )
  }
)

test_that(
  desc = 'NA values propagate',
  code = {
    expect_equal(
      suppressWarnings(
        predict_10yr_ascvd_risk(
          age_years = 54,
          sex = 'male',
          race = 'white',
          smoke_current = 'yes',
          chol_total_mgdl = NA_real_,
          chol_hdl_mgdl = 50,
          bp_sys_mmhg = 157,
          bp_meds = 'no',
          diabetes = 'yes'
        )
      ),
      NA_real_
    )
  }
)

test_that(
  desc = "examples run when different category levels are used",
  code = {

    sex = c('woman', 'woman', 'men', 'men')
    race = c('AA', 'CA', 'AA', 'CA')
    # 55 years of age
    age_years = rep(55, times = 4)
    # total cholesterol 213 mg/dL
    chol_total_mgdl = rep(213, times = 4)
    # HDL–C 50 mg/dL
    chol_hdl_mgdl = rep(50, times = 4)
    # untreated systolic BP 120 mm Hg
    bp_sys_mmhg = rep(120, times = 4)
    bp_meds = rep('No', times = 4)
    # nonsmoker
    smoke_current = rep('No', times = 4)
    # without diabetes
    diabetes = rep('No', times = 4)

    pcr_probs <- predict_10yr_ascvd_risk(
      sex = sex,
      race = race,
      age_years = age_years,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      smoke_current = smoke_current,
      diabetes = diabetes,
      race_levels = list(black = 'AA', white = 'CA'),
      sex_levels = list(female = 'woman', male = 'men'),
      smoke_current_levels = list(no = 'No', yes = 'Yes'),
      bp_meds_levels = list(no = 'No', yes = 'Yes'),
      diabetes_levels = list(no = 'No', yes = 'Yes')
    )

    # to match online format
    pcr_perc_rounded <- round(pcr_probs * 100, 1)

    expect_equal(
      pcr_perc_rounded,
      c(3.0, 2.1, 6.1, 5.4)
    )

  }

)

test_that(
  desc = "examples run in a data frame with different category levels",
  code = {

    some_data <- data.frame(
      gender = c('woman', 'woman', 'man', 'male'),
      race_3cats = c('AA', 'white', 'AA', 'other'),
      # 55 years of age
      age_years = rep(55, times = 4),
      # total cholesterol 213 mg/dL
      chol_total_mgdl = rep(213, times = 4),
      # HDL–C 50 mg/dL
      chol_hdl_mgdl = rep(50, times = 4),
      # untreated systolic BP 120 mm Hg
      bp_sys_mmhg = rep(120, times = 4),
      bp_meds = rep('No', times = 4),
      # nonsmoker
      smoke_current = c("no", "former", "no", "never"),
      # without diabetes
      diabetes = rep('No', times = 4),
      stringsAsFactors = FALSE
    )

    pcr_probs <- with(
      some_data,
      predict_10yr_ascvd_risk(
        sex = gender,
        sex_levels = list(female = 'woman', male = c('man', 'male')),
        race = race_3cats,
        age_years = age_years,
        chol_total_mgdl = chol_total_mgdl,
        chol_hdl_mgdl = chol_hdl_mgdl,
        bp_sys_mmhg = bp_sys_mmhg,
        bp_meds = bp_meds,
        smoke_current = smoke_current,
        diabetes = diabetes,
        race_levels = list(black = 'AA', white = c('white', 'other')),
        smoke_current_levels = list(no = c('no', 'former', 'never'), yes = 'Yes'),
        bp_meds_levels = list(no = 'No', yes = 'Yes'),
        diabetes_levels = list(no = 'No', yes = 'Yes')
      )
    )

    # to match online format
    pcr_perc_rounded <- round(pcr_probs * 100, 1)

    expect_equal(
      pcr_perc_rounded,
      c(3.0, 2.1, 6.1, 5.4)
    )

  }

)

test_that(
  desc = "examples run in a data frame with different category levels and NAs",
  code = {

    some_data <- data.frame(
      gender = c('woman', NA_character_, 'man', 'male'),
      race_3cats = c('AA', 'white', 'AA', 'other'),
      # 55 years of age
      age_years = rep(55, times = 4),
      # total cholesterol 213 mg/dL
      chol_total_mgdl = rep(213, times = 4),
      # HDL–C 50 mg/dL
      chol_hdl_mgdl = rep(50, times = 4),
      # untreated systolic BP 120 mm Hg
      bp_sys_mmhg = rep(120, times = 4),
      bp_meds = rep('No', times = 4),
      # nonsmoker
      smoke_current = c("no", "former", "no", "never"),
      # without diabetes
      diabetes = rep('No', times = 4),
      stringsAsFactors = FALSE
    )

    expect_warning(
      with(
        some_data,
        predict_10yr_ascvd_risk(
          sex = gender,
          sex_levels = list(female = 'woman', male = c('man', 'male')),
          race = race_3cats,
          age_years = age_years,
          chol_total_mgdl = chol_total_mgdl,
          chol_hdl_mgdl = chol_hdl_mgdl,
          bp_sys_mmhg = bp_sys_mmhg,
          bp_meds = bp_meds,
          smoke_current = smoke_current,
          diabetes = diabetes,
          race_levels = list(black = 'AA', white = c('white', 'other')),
          smoke_current_levels = list(no = c('no', 'former', 'never'), yes = 'Yes'),
          bp_meds_levels = list(no = 'No', yes = 'Yes'),
          diabetes_levels = list(no = 'No', yes = 'Yes')
        )
      ),
      'account for 1 missing values in the output'
    )

    pcr_probs <- suppressWarnings(
      with(
        some_data,
        predict_10yr_ascvd_risk(
          sex = gender,
          sex_levels = list(female = 'woman', male = c('man', 'male')),
          race = race_3cats,
          age_years = age_years,
          chol_total_mgdl = chol_total_mgdl,
          chol_hdl_mgdl = chol_hdl_mgdl,
          bp_sys_mmhg = bp_sys_mmhg,
          bp_meds = bp_meds,
          smoke_current = smoke_current,
          diabetes = diabetes,
          race_levels = list(black = 'AA', white = c('white', 'other')),
          smoke_current_levels = list(no = c('no', 'former', 'never'), yes = 'Yes'),
          bp_meds_levels = list(no = 'No', yes = 'Yes'),
          diabetes_levels = list(no = 'No', yes = 'Yes')
        )
      )
    )

    # to match online format
    pcr_perc_rounded <- round(pcr_probs * 100, 1)

    expect_equal(
      pcr_perc_rounded,
      c(3.0, NA, 6.1, 5.4)
    )

  }

)

test_that(
  desc = "examples run in data with different category levels and factors",
  code = {

    some_data <- data.frame(
      gender = c('woman', 'woman', 'man', 'male'),
      race_3cats = c('AA', 'white', 'AA', 'other'),
      # 55 years of age
      age_years = rep(55, times = 4),
      # total cholesterol 213 mg/dL
      chol_total_mgdl = rep(213, times = 4),
      # HDL–C 50 mg/dL
      chol_hdl_mgdl = rep(50, times = 4),
      # untreated systolic BP 120 mm Hg
      bp_sys_mmhg = rep(120, times = 4),
      bp_meds = rep('No', times = 4),
      # nonsmoker
      smoke_current = c("no", "former", "no", "never"),
      # without diabetes
      diabetes = rep('No', times = 4),
      stringsAsFactors = TRUE
    )

    pcr_probs <- with(
      some_data,
      predict_10yr_ascvd_risk(
        sex = gender,
        sex_levels = list(female = 'woman', male = c('man', 'male')),
        race = race_3cats,
        age_years = age_years,
        chol_total_mgdl = chol_total_mgdl,
        chol_hdl_mgdl = chol_hdl_mgdl,
        bp_sys_mmhg = bp_sys_mmhg,
        bp_meds = bp_meds,
        smoke_current = smoke_current,
        diabetes = diabetes,
        race_levels = list(black = 'AA', white = c('white', 'other')),
        smoke_current_levels = list(no = c('no', 'former', 'never'), yes = 'Yes'),
        bp_meds_levels = list(no = 'No', yes = 'Yes'),
        diabetes_levels = list(no = 'No', yes = 'Yes')
      )
    )

    # to match online format
    pcr_perc_rounded <- round(pcr_probs * 100, 1)

    expect_equal(
      pcr_perc_rounded,
      c(3.0, 2.1, 6.1, 5.4)
    )

  }

)


test_that(
  desc = "error messages for incorrect levels are informative",
  code = {

    sex = c('woman', 'woman', 'men', 'man')
    race = c('AA', 'CA', 'AA', 'CA')
    # 55 years of age
    age_years = rep(55, times = 4)
    # total cholesterol 213 mg/dL
    chol_total_mgdl = rep(213, times = 4)
    # HDL–C 50 mg/dL
    chol_hdl_mgdl = rep(50, times = 4)
    # untreated systolic BP 120 mm Hg
    bp_sys_mmhg = rep(120, times = 4)
    bp_meds = rep('No', times = 4)
    # nonsmoker
    smoke_current = rep('No', times = 4)
    # without diabetes
    diabetes = rep('No', times = 4)

    expect_error(
      predict_10yr_ascvd_risk(
        sex = sex,
        race = race,
        age_years = age_years,
        chol_total_mgdl = chol_total_mgdl,
        chol_hdl_mgdl = chol_hdl_mgdl,
        bp_sys_mmhg = bp_sys_mmhg,
        bp_meds = bp_meds,
        smoke_current = smoke_current,
        diabetes = diabetes,
        race_levels = list(black = 'AA', white = 'CA'),
        sex_levels = list(female = 'woman', male = 'men'),
        smoke_current_levels = list(no = 'No', yes = 'Yes'),
        bp_meds_levels = list(no = 'No', yes = 'Yes'),
        diabetes_levels = list(no = 'No', yes = 'Yes')
      ),
      regexp = 'instead has values <woman, men and man>'
    )

    age_years[1] <- 39

    expect_error(
      predict_10yr_ascvd_risk(
        sex = sex,
        race = race,
        age_years = age_years,
        chol_total_mgdl = chol_total_mgdl,
        chol_hdl_mgdl = chol_hdl_mgdl,
        bp_sys_mmhg = bp_sys_mmhg,
        bp_meds = bp_meds,
        smoke_current = smoke_current,
        diabetes = diabetes,
        race_levels = list(black = 'AA', white = 'CA'),
        sex_levels = list(female = 'woman', male = c('men', 'man')),
        smoke_current_levels = list(no = 'No', yes = 'Yes'),
        bp_meds_levels = list(no = 'No', yes = 'Yes'),
        diabetes_levels = list(no = 'No', yes = 'Yes')
      ),
      regexp = 'min\\(age_years\\) is 39 but should be >= 40'
    )

  }

)

# pooled cohort risk equations from 2018

test_that(
  desc = "Yadlowsky supplement examples are correct",
  code = {

    sex = c('female', 'female', 'male', 'male')
    race = c('black', 'white', 'black', 'white')
    # 55 years of age
    age_years = rep(55, times = 4)
    # total cholesterol 213 mg/dL
    chol_total_mgdl = rep(213, times = 4)
    # HDL–C 50 mg/dL
    chol_hdl_mgdl = rep(50, times = 4)
    # untreated systolic BP 120 mm Hg
    bp_sys_mmhg = rep(120, times = 4)
    bp_meds = rep('no', times = 4)
    # nonsmoker
    smoke_current = rep('no', times = 4)
    # without diabetes
    diabetes = rep('no', times = 4)

    pcr_probs <- predict_10yr_ascvd_risk(
      sex = sex,
      race = race,
      age_years = age_years,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      smoke_current = smoke_current,
      diabetes = diabetes,
      equation_version = 'Yadlowsky_2018'
    )

    # to match online format
    pcr_perc_rounded <- round(pcr_probs * 100, 1)

    expect_equal(
      pcr_perc_rounded,
      c(1.8, 1.2, 3.5, 3.3)
    )

  }

)


test_that(
  desc = "Goff supplement examples (with bp meds = yes) are correct",
  code = {

    sex = c('female', 'female', 'male', 'male')
    race = c('black', 'white', 'black', 'white')
    # 55 years of age
    age_years = rep(55, times = 4)
    # total cholesterol 213 mg/dL
    chol_total_mgdl = rep(213, times = 4)
    # HDL–C 50 mg/dL
    chol_hdl_mgdl = rep(50, times = 4)
    # treated systolic BP 120 mm Hg
    bp_sys_mmhg = rep(120, times = 4)
    bp_meds = rep('yes', times = 4)
    # nonsmoker
    smoke_current = rep('no', times = 4)
    # without diabetes
    diabetes = rep('no', times = 4)

    pcr_probs <- predict_10yr_ascvd_risk(
      sex = sex,
      race = race,
      age_years = age_years,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      smoke_current = smoke_current,
      diabetes = diabetes
    )

    # to match online format
    pcr_perc_rounded <- round(pcr_probs * 100, 1)

    expect_equal(
      pcr_perc_rounded,
      c(4.6, 2.8, 9.9, 6.3)
    )

  }

)

test_that(
  desc = "Yadlowsky supplement examples (with bp meds = yes) are correct",
  code = {

    sex = c('female', 'female', 'male', 'male')
    race = c('black', 'white', 'black', 'white')
    # 55 years of age
    age_years = rep(55, times = 4)
    # total cholesterol 213 mg/dL
    chol_total_mgdl = rep(213, times = 4)
    # HDL–C 50 mg/dL
    chol_hdl_mgdl = rep(50, times = 4)
    # untreated systolic BP 120 mm Hg
    bp_sys_mmhg = rep(120, times = 4)
    bp_meds = rep('yes', times = 4)
    # nonsmoker
    smoke_current = rep('no', times = 4)
    # without diabetes
    diabetes = rep('no', times = 4)

    pcr_probs <- predict_10yr_ascvd_risk(
      sex = sex,
      race = race,
      age_years = age_years,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      smoke_current = smoke_current,
      diabetes = diabetes,
      equation_version = 'Yadlowsky_2018'
    )

    # to match online format
    pcr_perc_rounded <- round(pcr_probs * 100, 1)

    expect_equal(
      pcr_perc_rounded,
      c(2.7, 1.6, 7.1, 4.7)
    )

  }

)

test_that(
  desc = "Goff supplement examples (with smoker = yes) are correct",
  code = {

    sex = c('female', 'female', 'male', 'male')
    race = c('black', 'white', 'black', 'white')
    # 55 years of age
    age_years = rep(55, times = 4)
    # total cholesterol 213 mg/dL
    chol_total_mgdl = rep(213, times = 4)
    # HDL–C 50 mg/dL
    chol_hdl_mgdl = rep(50, times = 4)
    # untreated systolic BP 120 mm Hg
    bp_sys_mmhg = rep(120, times = 4)
    bp_meds = rep('no', times = 4)
    # smoker
    smoke_current = rep('yes', times = 4)
    # without diabetes
    diabetes = rep('no', times = 4)

    pcr_probs <- predict_10yr_ascvd_risk(
      sex = sex,
      race = race,
      age_years = age_years,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      smoke_current = smoke_current,
      diabetes = diabetes
    )

    # to match online format
    pcr_perc_rounded <- round(pcr_probs * 100, 1)

    expect_equal(
      pcr_perc_rounded,
      c(5.9, 5.0, 10.3, 10.0)
    )

  }

)

test_that(
  desc = "Yadlowsky supplement examples (with smoker = yes) are correct",
  code = {

    sex = c('female', 'female', 'male', 'male')
    race = c('black', 'white', 'black', 'white')
    # 55 years of age
    age_years = rep(55, times = 4)
    # total cholesterol 213 mg/dL
    chol_total_mgdl = rep(213, times = 4)
    # HDL–C 50 mg/dL
    chol_hdl_mgdl = rep(50, times = 4)
    # untreated systolic BP 120 mm Hg
    bp_sys_mmhg = rep(120, times = 4)
    bp_meds = rep('no', times = 4)
    # nonsmoker
    smoke_current = rep('yes', times = 4)
    # without diabetes
    diabetes = rep('no', times = 4)

    pcr_probs <- predict_10yr_ascvd_risk(
      sex = sex,
      race = race,
      age_years = age_years,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      smoke_current = smoke_current,
      diabetes = diabetes,
      equation_version = 'Yadlowsky_2018'
    )

    # to match online format
    pcr_perc_rounded <- round(pcr_probs * 100, 1)

    expect_equal(
      pcr_perc_rounded,
      c(4.3, 3.2, 6.7, 7.8)
    )

  }

)

test_that(
  desc = "Goff supplement examples (with diabetes = yes) are correct",
  code = {

    sex = c('female', 'female', 'male', 'male')
    race = c('black', 'white', 'black', 'white')
    # 55 years of age
    age_years = rep(55, times = 4)
    # total cholesterol 213 mg/dL
    chol_total_mgdl = rep(213, times = 4)
    # HDL–C 50 mg/dL
    chol_hdl_mgdl = rep(50, times = 4)
    # untreated systolic BP 120 mm Hg
    bp_sys_mmhg = rep(120, times = 4)
    bp_meds = rep('no', times = 4)
    # nonsmoker
    smoke_current = rep('no', times = 4)
    # with diabetes
    diabetes = rep('yes', times = 4)

    pcr_probs <- predict_10yr_ascvd_risk(
      sex = sex,
      race = race,
      age_years = age_years,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      smoke_current = smoke_current,
      diabetes = diabetes
    )

    # to match online format
    pcr_perc_rounded <- round(pcr_probs * 100, 1)

    expect_equal(
      pcr_perc_rounded,
      c(7.0, 3.9, 11.2, 10.1)
    )

  }

)

test_that(
  desc = "Yadlowsky supplement examples (with diabetes = yes) are correct",
  code = {

    sex = c('female', 'female', 'male', 'male')
    race = c('black', 'white', 'black', 'white')
    # 55 years of age
    age_years = rep(55, times = 4)
    # total cholesterol 213 mg/dL
    chol_total_mgdl = rep(213, times = 4)
    # HDL–C 50 mg/dL
    chol_hdl_mgdl = rep(50, times = 4)
    # untreated systolic BP 120 mm Hg
    bp_sys_mmhg = rep(120, times = 4)
    bp_meds = rep('no', times = 4)
    # nonsmoker
    smoke_current = rep('no', times = 4)
    # without diabetes
    diabetes = rep('yes', times = 4)

    pcr_probs <- predict_10yr_ascvd_risk(
      sex = sex,
      race = race,
      age_years = age_years,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      smoke_current = smoke_current,
      diabetes = diabetes,
      equation_version = 'Yadlowsky_2018'
    )

    # to match online format
    pcr_perc_rounded <- round(pcr_probs * 100, 1)

    expect_equal(
      pcr_perc_rounded,
      c(4.9, 3.0, 7.3, 7.4)
    )

  }

)

test_that(
  desc = "Replicate examples from Khan supplemental table S12",
  code = {

    prevent_base_10 <- predict_10yr_ascvd_risk(
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

    expect_equal( round(prevent_base_10, 4), c(0.092, 0.1019) )

    prevent_acr_10 <- predict_10yr_ascvd_risk(
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

    expect_equal( round(prevent_acr_10, 4), c(.0994, .1097) )

    prevent_hba1c_10 <- predict_10yr_ascvd_risk(
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

    expect_equal( round(prevent_hba1c_10, 4), c(.0835, .0937) )

    prevent_sdi_10 <- predict_10yr_ascvd_risk(
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

    expect_equal( round(prevent_sdi_10, 4), c(.1003, .1137) )

    prevent_full_10 <- predict_10yr_ascvd_risk(
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

    expect_equal( round(prevent_full_10, 4), c(.0964,	.1100))

    prevent_full_missing_sdi <- predict_10yr_ascvd_risk(
      sex = c("female", "male"),
      age_years = c(56, 56),
      chol_total_mgdl = c(231, 231),
      chol_hdl_mgdl = c(46, 46),
      bp_sys_mmhg = c(150, 150),
      bmi = c(35, 35),
      egfr_mlminm2 = c(90, 90),
      diabetes = c("yes", "yes"),
      smoke_current = c("no", "no"),
      bp_meds = c("yes", "yes"),
      statin_meds = c("no", "no"),
      equation_version = "Khan_2023",
      acr = c(900, 900),
      hba1c = c(3, 3),
      sdi = c(NA, NA),
      prevent_type = 'full'
    )

    expect_equal( round(prevent_full_missing_sdi, 3), c(.077, .116))


    prevent_full_missing_sdi <- predict_10yr_ascvd_risk(
      sex = c("female", "male"),
      age_years = c(50, 50),
      chol_total_mgdl = c(200, 200),
      chol_hdl_mgdl = c(46, 46),
      bp_sys_mmhg = c(169, 169),
      bmi = c(35, 35),
      egfr_mlminm2 = c(90, 90),
      diabetes = c("no", "no"),
      smoke_current = c("no", "no"),
      bp_meds = c("no", "no"),
      statin_meds = c("no", "no"),
      equation_version = "Khan_2023",
      acr = c(40, 40),
      hba1c = c(3, 3),
      sdi = c(NA, NA),
      prevent_type = 'full'
    )

    expect_equal( round(prevent_full_missing_sdi, 3), c(.026, .041))

  }
)

test_that(
  desc = "boundary overrides work",
  code = {

    sex = c('female', 'female', 'male', 'male')
    race = c('black', 'white', 'black', 'white')
    # 55 years of age
    age_years = rep(85, times = 4)
    # total cholesterol 213 mg/dL
    chol_total_mgdl = rep(213, times = 4)
    # HDL–C 50 mg/dL
    chol_hdl_mgdl = rep(50, times = 4)
    # untreated systolic BP 120 mm Hg
    bp_sys_mmhg = rep(120, times = 4)
    bp_meds = rep('no', times = 4)
    # nonsmoker
    smoke_current = rep('no', times = 4)
    # without diabetes
    diabetes = rep('no', times = 4)

    pcr_probs <- predict_10yr_ascvd_risk(
      sex = sex,
      race = race,
      age_years = age_years,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      smoke_current = smoke_current,
      diabetes = diabetes,
      override_boundary_errors = TRUE
    )

    expect_is(pcr_probs, 'numeric')

  }

)

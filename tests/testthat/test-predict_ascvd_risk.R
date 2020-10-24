
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

    pcr_probs <- pcr_ascvd_2013(
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

    pcr_probs <- pcr_ascvd_2013(
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
      pcr_ascvd_2013(
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
      pcr_ascvd_2013(
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

    pcr_probs <- pcr_ascvd_2018(
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

    pcr_probs <- pcr_ascvd_2013(
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

    pcr_probs <- pcr_ascvd_2018(
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

    pcr_probs <- pcr_ascvd_2013(
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

    pcr_probs <- pcr_ascvd_2018(
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

    pcr_probs <- pcr_ascvd_2013(
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

    pcr_probs <- pcr_ascvd_2018(
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
      c(4.9, 3.0, 7.3, 7.4)
    )

  }

)

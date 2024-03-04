


#' ASCVD Risk Calculator
#'
#' @description This function implements
#'
#' - the Pooled Cohort Risk equations from Goff et al, 2013.
#'
#' - the updated Pooled Cohort Risk equations from Yadlowski et al, 2018
#'
#' - The PREVENT equations from Khan et al, 2023
#'
#' These equations predict 10-year risk of a first atherosclerotic
#'  cardiovascular disease (ASCVD) event, such as a stroke or myocardial
#'  infarction. The 2017 American College of Cardiology and American Heart
#'  Association blood pressure guideline recommends using 10-year predicted
#'  atherosclerotic cardiovascular disease risk to guide the decision to
#'  initiate or intensify antihypertensive medication. The guideline recommends
#'  using the Pooled Cohort risk prediction equations to predict 10-year
#'  atherosclerotic cardiovascular disease risk in clinical practice.
#'
#' @param age_years numeric vector of age values, in years.
#'
#' @param race character vector of race values. Categories should include
#'   only 'black' or 'white'. If the race variable has additional categories
#'   other than 'black' or 'white', then group all non 'black' values into
#'   the 'white' category. This variable is not required if
#'   `equation_version = 'Khan_2023'`
#'
#' @param sex character vector of sex values. Categories should include
#'   only 'male' or 'female'.
#'
#' @param smoke_current character vector of current smoking habits. Categories
#'   should include only 'no' and 'yes'.
#'
#' @param chol_total_mgdl total cholesterol, in mg/dL.
#'
#' @param chol_hdl_mgdl HDL-cholesterol, in mg/dL.
#'
#' @param bp_sys_mmhg systolic blood pressure, in mm Hg.
#'
#' @param bp_meds character vector of blood pressure medication use habits.
#'   Categories should include only 'no' and 'yes'. For example, if currently
#'   using medication to lower blood pressure, the value should be 'yes'.
#'
#' @param statin_meds character vector of statin medication use habits.
#'   Categories should include only 'no' and 'yes'. For example, if currently
#'   using a statin, the value should be 'yes'. This variable is only
#'   required if `equation_version = 'Khan_2023'`
#'
#' @param diabetes character vector of diabetes status. Categories
#'   should include only 'no' and 'yes'. For example, if diabetes is present,
#'   the value should be 'yes'.
#'
#' @param bmi numeric vector of bmi values. Only required if
#'   `equation_version = "Khan_2023"`
#'
#' @param egfr_mlminm2 numeric vector of egfr_mlminm2 values. Only required
#'   if `equation_version = "Khan_2023"`
#'
#' @param acr numeric vector of acr values. Only required if
#'   `equation_version = "Khan_2023"` and `prevent_type` is `"acr"` or
#'   `"full"`.
#'
#' @param hba1c numeric vector of hba1c values. Only required if
#'   `equation_version = "Khan_2023"` and `prevent_type` is `"hba1c"` or
#'   `"full"`.
#'
#' @param sdi numeric vector of sdi values. Only required if
#'   `equation_version = "Khan_2023"` and `prevent_type` is `"sdi"` or
#'   `"full"`.
#'
#' @param race_levels a list of length 2 with names 'black' and 'white'.
#'   values in the list should be character vectors of any length, and
#'   values in the character vectors should indicate what values in
#'   `race` belong to the 'black' and 'white' categories. For example,
#'   `race` may contain values of 'african_american', 'white', and
#'   'hispanic'. In this case, `race_levels` should be
#'   `list(white = c('white', 'hispanic'), black = 'african_american')`.
#'
#' @param sex_levels a list of length 2 with names 'female' and 'male'.
#'   values in the list should be character vectors of any length, and
#'   values in the character vectors should indicate what values in
#'   `sex` belong to the 'female' and 'male' categories (see examples).
#'
#' @param smoke_current_levels a list of length 2 with names 'no' and 'yes'.
#'   values in the list should be character vectors of any length, and
#'   values in the character vectors should indicate what values in
#'   `smoke_current` belong to the 'no' and 'yes' categories (see examples).
#'
#' @param bp_meds_levels a list of length 2 with names 'no' and 'yes'.
#'   values in the list should be character vectors of any length, and
#'   values in the character vectors should indicate what values in
#'   `bp_meds` belong to the 'no' and 'yes' categories (see examples).
#'
#' @param statin_meds_levels a list of length 2 with names 'no' and 'yes'.
#'   values in the list should be character vectors of any length, and
#'   values in the character vectors should indicate what values in
#'   `statin_meds` belong to the 'no' and 'yes' categories (see examples).
#'
#' @param diabetes_levels a list of length 2 with names 'no' and 'yes'.
#'   values in the list should be character vectors of any length, and
#'   values in the character vectors should indicate what values in
#'   `diabetes` belong to the 'no' and 'yes' categories (see examples).
#'
#' @param equation_version a character value of length 1. Valid options are
#'
#'  - 'Goff_2013'
#'
#'  - 'Yadlowsky_2018'
#'
#'  - 'Khan_2023'
#'
#' If 'Goff_2013' (the default option) is selected, the original Pooled
#'   Cohort risk equations are used (See Goff et al., 2013).
#'
#' If 'Yadlowsky_2018' is selected, the equations recommended by Yadlowsky
#'   et al., 2018 are used.
#'
#' If 'Khan_2023' is selected, the equations recommended by Khan
#'   et al., 2023 are used.
#'
#' @param prevent_type a character value of length 1. Only required if
#'   `equation_version = "Khan_2023"`. Valid options are:
#'
#'   - 'base': computes the base PREVENT equation (default).
#'
#'   - 'acr': computes the PREVENT equation using albumin-to-creatinine ratio.
#'
#'   - 'hba1c': computes the PREVENT equation using hemoglobin A1c.
#'
#'   - 'sdi': computes the PREVENT equation using social deprivation index.
#'
#'   - 'full': computes the PREVENT equation using all novel predictors.
#'
#' @param override_boundary_errors a logical vector of length 1. If `FALSE`
#'   (the default), then `predict_10yr_ascvd_risk()` will throw hard errors
#'   if you give it continuous input values that are outside the bounaries
#'   of what the Pooled Cohort risk calculator recommends. If `TRUE`, errors
#'   will not be thrown. Please use with caution.
#'
#'
#' @return a numeric vector with 10-year predicted risk values for ASCVD events.
#'
#' @export
#'
#' @details The 2017 American College of Cardiology (ACC) / American Heart
#'   Association (AHA) blood pressure (BP) guideline recommends using 10-year
#'   predicted atherosclerotic cardiovascular disease (ASCVD) risk to guide
#'   the decision to initiate antihypertensive medication. The guideline
#'   recommends using the Pooled Cohort risk prediction equations (Goff et al, 2013)
#'   to predict 10-year ASCVD risk. The Pooled Cohort risk prediction equations
#'   have been externally validated in several studies and, in some populations,
#'   are known to overestimate 10-year ASCVD risk. In 2018, an updated set of
#'   equations were developed by Yadlowsky et al. using more contemporary data
#'   and statistical methods.
#'
#' @references Goff DC, Lloyd-Jones DM, Bennett G, Coady S, D’agostino RB,
#'   Gibbons R, Greenland P, Lackland DT, Levy D, O’donnell CJ, Robinson JG.
#'   2013 ACC/AHA guideline on the assessment of cardiovascular risk: a report
#'   of the American College of Cardiology/American Heart Association Task
#'   Force on Practice Guidelines. *Journal of the American College of Cardiology*.
#'   2014 Jul 1;63(25 Part B):2935-59. DOI: 10.1016/j.jacc.2014.03.006
#'
#'   Yadlowsky S, Hayward RA, Sussman JB, McClelland RL, Min YI, Basu S.
#'   Clinical implications of revised pooled cohort equations for estimating
#'   atherosclerotic cardiovascular disease risk. *Annals of internal medicine*.
#'   2018 Jul 3;169(1):20-9. DOI: 10.7326/M17-3011
#'
#' @examples
#'
#' # example taken from Goff et al, 2013
#'
#' sex = c('female', 'female', 'male', 'male')
#' race = c('black', 'white', 'black', 'white')
#' # 55 years of age
#' age_years = rep(55, times = 4)
#' # total cholesterol 213 mg/dL
#' chol_total_mgdl = rep(213, times = 4)
#' # HDL cholesterol 50 mg/dL
#' chol_hdl_mgdl = rep(50, times = 4)
#' # untreated systolic BP 120 mm Hg
#' bp_sys_mmhg = rep(120, times = 4)
#' bp_meds = rep('no', times = 4)
#' # nonsmoker
#' smoke_current = rep('no', times = 4)
#' # without diabetes
#' diabetes = rep('no', times = 4)
#'
#' pcr_probs <- predict_10yr_ascvd_risk(
#'   sex = sex,
#'   race = race,
#'   age_years = age_years,
#'   chol_total_mgdl = chol_total_mgdl,
#'   chol_hdl_mgdl = chol_hdl_mgdl,
#'   bp_sys_mmhg = bp_sys_mmhg,
#'   bp_meds = bp_meds,
#'   smoke_current = smoke_current,
#'   diabetes = diabetes
#' )
#'
#' # note that this isn't an exact match of Table 4 in
#' # Goff et al supplement - this is because the table's
#' # coefficients are rounded to a lower decimal count than
#' # the coefficients used in predict_10yr_ascvd_risk()
#' round(100 * pcr_probs, 1)
#'
#' # using a data frame with more granular categories and names
#'
#' some_data <- data.frame(
#'   gender = c('woman', 'woman', 'man', 'male'),
#'   race_3cats = c('AA', 'white', 'AA', 'other'),
#'   # 55 years of age
#'   age_years = rep(55, times = 4),
#'   # total cholesterol 213 mg/dL
#'   chol_total_mgdl = rep(213, times = 4),
#'   # HDL cholesterol 50 mg/dL
#'   chol_hdl_mgdl = rep(50, times = 4),
#'   # untreated systolic BP 120 mm Hg
#'   bp_sys_mmhg = rep(120, times = 4),
#'   bp_meds = rep('No', times = 4),
#'   # nonsmoker
#'   smoke_current = c("no", "former", "no", "never"),
#'   # without diabetes
#'   diabetes = rep('No', times = 4),
#'   stringsAsFactors = FALSE
#' )
#'
#' pcr_probs <- with(
#'   some_data,
#'   predict_10yr_ascvd_risk(
#'     sex = gender,
#'     sex_levels = list(female = 'woman', male = c('man', 'male')),
#'     race = race_3cats,
#'     age_years = age_years,
#'     chol_total_mgdl = chol_total_mgdl,
#'     chol_hdl_mgdl = chol_hdl_mgdl,
#'     bp_sys_mmhg = bp_sys_mmhg,
#'     bp_meds = bp_meds,
#'     smoke_current = smoke_current,
#'     diabetes = diabetes,
#'     race_levels = list(black = 'AA', white = c('white', 'other')),
#'     smoke_current_levels = list(no = c('no', 'former', 'never'), yes = 'Yes'),
#'     bp_meds_levels = list(no = 'No', yes = 'Yes'),
#'     diabetes_levels = list(no = 'No', yes = 'Yes')
#'   )
#' )
#'
#'

predict_10yr_ascvd_risk <- function(
  age_years,
  race = NULL,
  sex,
  smoke_current,
  chol_total_mgdl,
  chol_hdl_mgdl,
  bp_sys_mmhg,
  bp_meds,
  statin_meds = NULL,
  diabetes,
  bmi = NULL,
  egfr_mlminm2 = NULL,
  acr = NULL,
  hba1c = NULL,
  sdi = NULL,
  equation_version = 'Goff_2013',
  prevent_type = 'base',
  override_boundary_errors = FALSE,
  race_levels = list(black = 'black', white = 'white'),
  sex_levels = list(female = 'female', male = 'male'),
  smoke_current_levels = list(no = 'no', yes = 'yes'),
  bp_meds_levels = list(no = 'no', yes = 'yes'),
  statin_meds_levels = list(no = 'no', yes = 'yes'),
  diabetes_levels = list(no = 'no', yes = 'yes')
) {

  ._predict_risk(pred_type = 'ascvd',
                 year = 10,
                 age_years = age_years,
                 race = race,
                 sex = sex,
                 smoke_current = smoke_current,
                 chol_total_mgdl = chol_total_mgdl,
                 chol_hdl_mgdl = chol_hdl_mgdl,
                 bp_sys_mmhg = bp_sys_mmhg,
                 bp_meds = bp_meds,
                 statin_meds = statin_meds,
                 diabetes = diabetes,
                 bmi = bmi,
                 egfr_mlminm2 = egfr_mlminm2,
                 acr = acr,
                 hba1c = hba1c,
                 sdi = sdi,
                 equation_version = equation_version,
                 prevent_type = prevent_type,
                 override_boundary_errors = override_boundary_errors,
                 race_levels = race_levels,
                 sex_levels = sex_levels,
                 smoke_current_levels = smoke_current_levels,
                 bp_meds_levels = bp_meds_levels,
                 statin_meds_levels = statin_meds_levels,
                 diabetes_levels = diabetes_levels)

}

#' @rdname predict_10yr_ascvd_risk
#' @export
predict_10yr_cvd_risk <- function(
    age_years,
    race = NULL,
    sex,
    smoke_current,
    chol_total_mgdl,
    chol_hdl_mgdl,
    bp_sys_mmhg,
    bp_meds,
    statin_meds = NULL,
    diabetes,
    bmi = NULL,
    egfr_mlminm2 = NULL,
    acr = NULL,
    hba1c = NULL,
    sdi = NULL,
    equation_version = 'Khan_2023',
    prevent_type = 'base',
    override_boundary_errors = FALSE,
    race_levels = list(black = 'black', white = 'white'),
    sex_levels = list(female = 'female', male = 'male'),
    smoke_current_levels = list(no = 'no', yes = 'yes'),
    bp_meds_levels = list(no = 'no', yes = 'yes'),
    statin_meds_levels = list(no = 'no', yes = 'yes'),
    diabetes_levels = list(no = 'no', yes = 'yes')
) {

  ._predict_risk(pred_type = 'cvd',
                 year = 10,
                 age_years = age_years,
                 race = race,
                 sex = sex,
                 smoke_current = smoke_current,
                 chol_total_mgdl = chol_total_mgdl,
                 chol_hdl_mgdl = chol_hdl_mgdl,
                 bp_sys_mmhg = bp_sys_mmhg,
                 bp_meds = bp_meds,
                 statin_meds = statin_meds,
                 diabetes = diabetes,
                 bmi = bmi,
                 egfr_mlminm2 = egfr_mlminm2,
                 acr = acr,
                 hba1c = hba1c,
                 sdi = sdi,
                 equation_version = equation_version,
                 prevent_type = prevent_type,
                 override_boundary_errors = override_boundary_errors,
                 race_levels = race_levels,
                 sex_levels = sex_levels,
                 smoke_current_levels = smoke_current_levels,
                 bp_meds_levels = bp_meds_levels,
                 statin_meds_levels = statin_meds_levels,
                 diabetes_levels = diabetes_levels)

}

#' @rdname predict_10yr_ascvd_risk
#' @export
predict_10yr_hf_risk <- function(
    age_years,
    race = NULL,
    sex,
    smoke_current,
    chol_total_mgdl,
    chol_hdl_mgdl,
    bp_sys_mmhg,
    bp_meds,
    statin_meds = NULL,
    diabetes,
    bmi = NULL,
    egfr_mlminm2 = NULL,
    acr = NULL,
    hba1c = NULL,
    sdi = NULL,
    equation_version = 'Khan_2023',
    prevent_type = 'base',
    override_boundary_errors = FALSE,
    race_levels = list(black = 'black', white = 'white'),
    sex_levels = list(female = 'female', male = 'male'),
    smoke_current_levels = list(no = 'no', yes = 'yes'),
    bp_meds_levels = list(no = 'no', yes = 'yes'),
    statin_meds_levels = list(no = 'no', yes = 'yes'),
    diabetes_levels = list(no = 'no', yes = 'yes')
) {

  ._predict_risk(pred_type = 'hf',
                 year = 10,
                 age_years = age_years,
                 race = race,
                 sex = sex,
                 smoke_current = smoke_current,
                 chol_total_mgdl = chol_total_mgdl,
                 chol_hdl_mgdl = chol_hdl_mgdl,
                 bp_sys_mmhg = bp_sys_mmhg,
                 bp_meds = bp_meds,
                 statin_meds = statin_meds,
                 diabetes = diabetes,
                 bmi = bmi,
                 egfr_mlminm2 = egfr_mlminm2,
                 acr = acr,
                 hba1c = hba1c,
                 sdi = sdi,
                 equation_version = equation_version,
                 prevent_type = prevent_type,
                 override_boundary_errors = override_boundary_errors,
                 race_levels = race_levels,
                 sex_levels = sex_levels,
                 smoke_current_levels = smoke_current_levels,
                 bp_meds_levels = bp_meds_levels,
                 statin_meds_levels = statin_meds_levels,
                 diabetes_levels = diabetes_levels)

}

#' @rdname predict_10yr_ascvd_risk
#' @export
predict_10yr_chd_risk <- function(
    age_years,
    race = NULL,
    sex,
    smoke_current,
    chol_total_mgdl,
    chol_hdl_mgdl,
    bp_sys_mmhg,
    bp_meds,
    statin_meds = NULL,
    diabetes,
    bmi = NULL,
    egfr_mlminm2 = NULL,
    acr = NULL,
    hba1c = NULL,
    sdi = NULL,
    equation_version = 'Khan_2023',
    prevent_type = 'base',
    override_boundary_errors = FALSE,
    race_levels = list(black = 'black', white = 'white'),
    sex_levels = list(female = 'female', male = 'male'),
    smoke_current_levels = list(no = 'no', yes = 'yes'),
    bp_meds_levels = list(no = 'no', yes = 'yes'),
    statin_meds_levels = list(no = 'no', yes = 'yes'),
    diabetes_levels = list(no = 'no', yes = 'yes')
) {

  ._predict_risk(pred_type = 'chd',
                 year = 10,
                 age_years = age_years,
                 race = race,
                 sex = sex,
                 smoke_current = smoke_current,
                 chol_total_mgdl = chol_total_mgdl,
                 chol_hdl_mgdl = chol_hdl_mgdl,
                 bp_sys_mmhg = bp_sys_mmhg,
                 bp_meds = bp_meds,
                 statin_meds = statin_meds,
                 diabetes = diabetes,
                 bmi = bmi,
                 egfr_mlminm2 = egfr_mlminm2,
                 acr = acr,
                 hba1c = hba1c,
                 sdi = sdi,
                 equation_version = equation_version,
                 prevent_type = prevent_type,
                 override_boundary_errors = override_boundary_errors,
                 race_levels = race_levels,
                 sex_levels = sex_levels,
                 smoke_current_levels = smoke_current_levels,
                 bp_meds_levels = bp_meds_levels,
                 statin_meds_levels = statin_meds_levels,
                 diabetes_levels = diabetes_levels)

}

#' @rdname predict_10yr_ascvd_risk
#' @export
predict_10yr_stroke_risk <- function(
    age_years,
    race = NULL,
    sex,
    smoke_current,
    chol_total_mgdl,
    chol_hdl_mgdl,
    bp_sys_mmhg,
    bp_meds,
    statin_meds = NULL,
    diabetes,
    bmi = NULL,
    egfr_mlminm2 = NULL,
    acr = NULL,
    hba1c = NULL,
    sdi = NULL,
    equation_version = 'Khan_2023',
    prevent_type = 'base',
    override_boundary_errors = FALSE,
    race_levels = list(black = 'black', white = 'white'),
    sex_levels = list(female = 'female', male = 'male'),
    smoke_current_levels = list(no = 'no', yes = 'yes'),
    bp_meds_levels = list(no = 'no', yes = 'yes'),
    statin_meds_levels = list(no = 'no', yes = 'yes'),
    diabetes_levels = list(no = 'no', yes = 'yes')
) {

  ._predict_risk(pred_type = 'stroke',
                 year = 10,
                 age_years = age_years,
                 race = race,
                 sex = sex,
                 smoke_current = smoke_current,
                 chol_total_mgdl = chol_total_mgdl,
                 chol_hdl_mgdl = chol_hdl_mgdl,
                 bp_sys_mmhg = bp_sys_mmhg,
                 bp_meds = bp_meds,
                 statin_meds = statin_meds,
                 diabetes = diabetes,
                 bmi = bmi,
                 egfr_mlminm2 = egfr_mlminm2,
                 acr = acr,
                 hba1c = hba1c,
                 sdi = sdi,
                 equation_version = equation_version,
                 prevent_type = prevent_type,
                 override_boundary_errors = override_boundary_errors,
                 race_levels = race_levels,
                 sex_levels = sex_levels,
                 smoke_current_levels = smoke_current_levels,
                 bp_meds_levels = bp_meds_levels,
                 statin_meds_levels = statin_meds_levels,
                 diabetes_levels = diabetes_levels)

}

#' @rdname predict_10yr_ascvd_risk
#' @export
predict_30yr_ascvd_risk <- function(
    age_years,
    race = NULL,
    sex,
    smoke_current,
    chol_total_mgdl,
    chol_hdl_mgdl,
    bp_sys_mmhg,
    bp_meds,
    statin_meds = NULL,
    diabetes,
    bmi = NULL,
    egfr_mlminm2 = NULL,
    acr = NULL,
    hba1c = NULL,
    sdi = NULL,
    equation_version = 'Khan_2023',
    prevent_type = 'base',
    override_boundary_errors = FALSE,
    race_levels = list(black = 'black', white = 'white'),
    sex_levels = list(female = 'female', male = 'male'),
    smoke_current_levels = list(no = 'no', yes = 'yes'),
    bp_meds_levels = list(no = 'no', yes = 'yes'),
    statin_meds_levels = list(no = 'no', yes = 'yes'),
    diabetes_levels = list(no = 'no', yes = 'yes')
) {

  ._predict_risk(pred_type = 'ascvd',
                 year = 30,
                 age_years = age_years,
                 race = race,
                 sex = sex,
                 smoke_current = smoke_current,
                 chol_total_mgdl = chol_total_mgdl,
                 chol_hdl_mgdl = chol_hdl_mgdl,
                 bp_sys_mmhg = bp_sys_mmhg,
                 bp_meds = bp_meds,
                 statin_meds = statin_meds,
                 diabetes = diabetes,
                 bmi = bmi,
                 egfr_mlminm2 = egfr_mlminm2,
                 acr = acr,
                 hba1c = hba1c,
                 sdi = sdi,
                 equation_version = equation_version,
                 prevent_type = prevent_type,
                 override_boundary_errors = override_boundary_errors,
                 race_levels = race_levels,
                 sex_levels = sex_levels,
                 smoke_current_levels = smoke_current_levels,
                 bp_meds_levels = bp_meds_levels,
                 statin_meds_levels = statin_meds_levels,
                 diabetes_levels = diabetes_levels)

}

#' @rdname predict_10yr_ascvd_risk
#' @export
predict_30yr_cvd_risk <- function(
    age_years,
    race = NULL,
    sex,
    smoke_current,
    chol_total_mgdl,
    chol_hdl_mgdl,
    bp_sys_mmhg,
    bp_meds,
    statin_meds = NULL,
    diabetes,
    bmi = NULL,
    egfr_mlminm2 = NULL,
    acr = NULL,
    hba1c = NULL,
    sdi = NULL,
    equation_version = 'Khan_2023',
    prevent_type = 'base',
    override_boundary_errors = FALSE,
    race_levels = list(black = 'black', white = 'white'),
    sex_levels = list(female = 'female', male = 'male'),
    smoke_current_levels = list(no = 'no', yes = 'yes'),
    bp_meds_levels = list(no = 'no', yes = 'yes'),
    statin_meds_levels = list(no = 'no', yes = 'yes'),
    diabetes_levels = list(no = 'no', yes = 'yes')
) {

  ._predict_risk(pred_type = 'cvd',
                 year = 30,
                 age_years = age_years,
                 race = race,
                 sex = sex,
                 smoke_current = smoke_current,
                 chol_total_mgdl = chol_total_mgdl,
                 chol_hdl_mgdl = chol_hdl_mgdl,
                 bp_sys_mmhg = bp_sys_mmhg,
                 bp_meds = bp_meds,
                 statin_meds = statin_meds,
                 diabetes = diabetes,
                 bmi = bmi,
                 egfr_mlminm2 = egfr_mlminm2,
                 acr = acr,
                 hba1c = hba1c,
                 sdi = sdi,
                 equation_version = equation_version,
                 prevent_type = prevent_type,
                 override_boundary_errors = override_boundary_errors,
                 race_levels = race_levels,
                 sex_levels = sex_levels,
                 smoke_current_levels = smoke_current_levels,
                 bp_meds_levels = bp_meds_levels,
                 statin_meds_levels = statin_meds_levels,
                 diabetes_levels = diabetes_levels)

}

#' @rdname predict_10yr_ascvd_risk
#' @export
predict_30yr_hf_risk <- function(
    age_years,
    race = NULL,
    sex,
    smoke_current,
    chol_total_mgdl,
    chol_hdl_mgdl,
    bp_sys_mmhg,
    bp_meds,
    statin_meds = NULL,
    diabetes,
    bmi = NULL,
    egfr_mlminm2 = NULL,
    acr = NULL,
    hba1c = NULL,
    sdi = NULL,
    equation_version = 'Khan_2023',
    prevent_type = 'base',
    override_boundary_errors = FALSE,
    race_levels = list(black = 'black', white = 'white'),
    sex_levels = list(female = 'female', male = 'male'),
    smoke_current_levels = list(no = 'no', yes = 'yes'),
    bp_meds_levels = list(no = 'no', yes = 'yes'),
    statin_meds_levels = list(no = 'no', yes = 'yes'),
    diabetes_levels = list(no = 'no', yes = 'yes')
) {

  ._predict_risk(pred_type = 'hf',
                 year = 30,
                 age_years = age_years,
                 race = race,
                 sex = sex,
                 smoke_current = smoke_current,
                 chol_total_mgdl = chol_total_mgdl,
                 chol_hdl_mgdl = chol_hdl_mgdl,
                 bp_sys_mmhg = bp_sys_mmhg,
                 bp_meds = bp_meds,
                 statin_meds = statin_meds,
                 diabetes = diabetes,
                 bmi = bmi,
                 egfr_mlminm2 = egfr_mlminm2,
                 acr = acr,
                 hba1c = hba1c,
                 sdi = sdi,
                 equation_version = equation_version,
                 prevent_type = prevent_type,
                 override_boundary_errors = override_boundary_errors,
                 race_levels = race_levels,
                 sex_levels = sex_levels,
                 smoke_current_levels = smoke_current_levels,
                 bp_meds_levels = bp_meds_levels,
                 statin_meds_levels = statin_meds_levels,
                 diabetes_levels = diabetes_levels)

}

#' @rdname predict_10yr_ascvd_risk
#' @export
predict_30yr_chd_risk <- function(
    age_years,
    race = NULL,
    sex,
    smoke_current,
    chol_total_mgdl,
    chol_hdl_mgdl,
    bp_sys_mmhg,
    bp_meds,
    statin_meds = NULL,
    diabetes,
    bmi = NULL,
    egfr_mlminm2 = NULL,
    acr = NULL,
    hba1c = NULL,
    sdi = NULL,
    equation_version = 'Khan_2023',
    prevent_type = 'base',
    override_boundary_errors = FALSE,
    race_levels = list(black = 'black', white = 'white'),
    sex_levels = list(female = 'female', male = 'male'),
    smoke_current_levels = list(no = 'no', yes = 'yes'),
    bp_meds_levels = list(no = 'no', yes = 'yes'),
    statin_meds_levels = list(no = 'no', yes = 'yes'),
    diabetes_levels = list(no = 'no', yes = 'yes')
) {

  ._predict_risk(pred_type = 'chd',
                 year = 30,
                 age_years = age_years,
                 race = race,
                 sex = sex,
                 smoke_current = smoke_current,
                 chol_total_mgdl = chol_total_mgdl,
                 chol_hdl_mgdl = chol_hdl_mgdl,
                 bp_sys_mmhg = bp_sys_mmhg,
                 bp_meds = bp_meds,
                 statin_meds = statin_meds,
                 diabetes = diabetes,
                 bmi = bmi,
                 egfr_mlminm2 = egfr_mlminm2,
                 acr = acr,
                 hba1c = hba1c,
                 sdi = sdi,
                 equation_version = equation_version,
                 prevent_type = prevent_type,
                 override_boundary_errors = override_boundary_errors,
                 race_levels = race_levels,
                 sex_levels = sex_levels,
                 smoke_current_levels = smoke_current_levels,
                 bp_meds_levels = bp_meds_levels,
                 statin_meds_levels = statin_meds_levels,
                 diabetes_levels = diabetes_levels)

}

#' @rdname predict_10yr_ascvd_risk
#' @export
predict_30yr_stroke_risk <- function(
    age_years,
    race = NULL,
    sex,
    smoke_current,
    chol_total_mgdl,
    chol_hdl_mgdl,
    bp_sys_mmhg,
    bp_meds,
    statin_meds = NULL,
    diabetes,
    bmi = NULL,
    egfr_mlminm2 = NULL,
    acr = NULL,
    hba1c = NULL,
    sdi = NULL,
    equation_version = 'Khan_2023',
    prevent_type = 'base',
    override_boundary_errors = FALSE,
    race_levels = list(black = 'black', white = 'white'),
    sex_levels = list(female = 'female', male = 'male'),
    smoke_current_levels = list(no = 'no', yes = 'yes'),
    bp_meds_levels = list(no = 'no', yes = 'yes'),
    statin_meds_levels = list(no = 'no', yes = 'yes'),
    diabetes_levels = list(no = 'no', yes = 'yes')
) {

  ._predict_risk(pred_type = 'stroke',
                 year = 30,
                 age_years = age_years,
                 race = race,
                 sex = sex,
                 smoke_current = smoke_current,
                 chol_total_mgdl = chol_total_mgdl,
                 chol_hdl_mgdl = chol_hdl_mgdl,
                 bp_sys_mmhg = bp_sys_mmhg,
                 bp_meds = bp_meds,
                 statin_meds = statin_meds,
                 diabetes = diabetes,
                 bmi = bmi,
                 egfr_mlminm2 = egfr_mlminm2,
                 acr = acr,
                 hba1c = hba1c,
                 sdi = sdi,
                 equation_version = equation_version,
                 prevent_type = prevent_type,
                 override_boundary_errors = override_boundary_errors,
                 race_levels = race_levels,
                 sex_levels = sex_levels,
                 smoke_current_levels = smoke_current_levels,
                 bp_meds_levels = bp_meds_levels,
                 statin_meds_levels = statin_meds_levels,
                 diabetes_levels = diabetes_levels)

}

._predict_risk <- function(pred_type,
                           year,
                           age_years,
                           race,
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
                           equation_version,
                           prevent_type,
                           override_boundary_errors,
                           race_levels,
                           sex_levels,
                           smoke_current_levels,
                           bp_meds_levels,
                           statin_meds_levels,
                           diabetes_levels){

  check_input(
    arg_name = 'equation_version',
    arg_value = equation_version,
    expected = list(
      type = 'character',
      length = 1,
      options = c("Goff_2013", "Yadlowsky_2018", "Khan_2023")
    )
  )

  check_input(
    arg_name = 'override_boundary_errors',
    arg_value = override_boundary_errors,
    expected = list(
      type = 'logical',
      length = 1
    )
  )

  if(equation_version %in% c("Goff_2013", "Yadlowsky_2018")){

    if(is.null(race)){
      stop("missing required variable for Pooled Cohort equations: race")
    }

    result <- ._pcr(
      age_years = age_years,
      race = race,
      sex = sex,
      smoke_current = smoke_current,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      diabetes = diabetes,
      equation_version = equation_version,
      override_boundary_errors = override_boundary_errors,
      race_levels = race_levels,
      sex_levels = sex_levels,
      smoke_current_levels = smoke_current_levels,
      bp_meds_levels = bp_meds_levels,
      diabetes_levels = diabetes_levels,
      year = year
    )

  } else if (equation_version == "Khan_2023"){

    missing <- c(is.null(statin_meds), is.null(egfr_mlminm2), is.null(bmi))

    missing_vars <- c("statin_meds", "egfr_mlminm2", "bmi")[missing]

    if(length(missing_vars) > 0){

      plural <- length(missing_vars > 1)

      if(plural){
        missing_vars <- paste(missing_vars, collapse = ', ')
        stop("missing required variables for PREVENT equations: ",
             missing_vars)
      } else {
        stop("missing required variable for PREVENT equations: ",
             missing_vars)
      }
    }

    result <- ._prevent(age_years = age_years,
                        sex = sex,
                        smoke_current = smoke_current,
                        chol_total_mgdl = chol_total_mgdl,
                        chol_hdl_mgdl = chol_hdl_mgdl,
                        bp_sys_mmhg = bp_sys_mmhg,
                        bp_meds = bp_meds,
                        statin_meds = statin_meds,
                        diabetes = diabetes,
                        bmi = bmi,
                        egfr_mlminm2 = egfr_mlminm2,
                        acr = acr,
                        hba1c = hba1c,
                        sdi = sdi,
                        override_boundary_errors = override_boundary_errors,
                        sex_levels = sex_levels,
                        smoke_current_levels = smoke_current_levels,
                        bp_meds_levels = bp_meds_levels,
                        statin_meds_levels = statin_meds_levels,
                        diabetes_levels = diabetes_levels,
                        prevent_type = prevent_type,
                        pred_type = pred_type,
                        year = year)

  }

  result

}

#' @rdname predict_10yr_ascvd_risk
#' @export
predict_5yr_ascvd_risk <- function(
  age_years,
  race,
  sex,
  smoke_current,
  chol_total_mgdl,
  chol_hdl_mgdl,
  bp_sys_mmhg,
  bp_meds,
  diabetes,
  equation_version = 'Goff_2013',
  override_boundary_errors = FALSE,
  race_levels = list(black = 'black', white = 'white'),
  sex_levels = list(female = 'female', male = 'male'),
  smoke_current_levels = list(no = 'no', yes = 'yes'),
  bp_meds_levels = list(no = 'no', yes = 'yes'),
  diabetes_levels = list(no = 'no', yes = 'yes')
) {

  check_input(
    arg_name = 'equation_version',
    arg_value = equation_version,
    expected = list(
      type = 'character',
      length = 1,
      options = c("Goff_2013", "Yadlowsky_2018"))
  )

  if(equation_version == 'Yadlowsky_2018'){
    stop("only 10-year ASCVD risk is available for Yadlowsky 2018",
         call. = FALSE)
  }

  check_input(
    arg_name = 'override_boundary_errors',
    arg_value = override_boundary_errors,
    expected = list(
      type = 'logical',
      length = 1
    )
  )

  ._pcr(
    age_years = age_years,
    race = race,
    sex = sex,
    smoke_current = smoke_current,
    chol_total_mgdl = chol_total_mgdl,
    chol_hdl_mgdl = chol_hdl_mgdl,
    bp_sys_mmhg = bp_sys_mmhg,
    bp_meds = bp_meds,
    diabetes = diabetes,
    equation_version = equation_version,
    override_boundary_errors = override_boundary_errors,
    race_levels = race_levels,
    sex_levels = sex_levels,
    smoke_current_levels = smoke_current_levels,
    bp_meds_levels = bp_meds_levels,
    diabetes_levels = diabetes_levels,
    year = 5
  )

}

._pcr <- function(
  age_years,
  race,
  sex,
  smoke_current,
  chol_total_mgdl,
  chol_hdl_mgdl,
  bp_sys_mmhg,
  bp_meds,
  diabetes,
  equation_version,
  override_boundary_errors,
  race_levels,
  sex_levels,
  smoke_current_levels,
  bp_meds_levels,
  diabetes_levels,
  year
) {

  # coerce categorical data to character values ----

  race = as.character(race)
  sex = as.character(sex)
  smoke_current = as.character(smoke_current)
  bp_meds = as.character(bp_meds)
  diabetes = as.character(diabetes)

  # argument checking ----

  check_call(
    match.call(),
    expected = list(
      'age_years' = list(
        type = 'numeric',
        length = NULL,
        lwr = ifelse(override_boundary_errors, yes = -Inf, no = 40),
        upr = ifelse(override_boundary_errors, yes = Inf,  no = 80)
      ),
      'race' = list(
        type = 'character',
        length = NULL,
        options = race_levels
      ),
      'sex' = list(
        type = 'character',
        length = NULL,
        options = sex_levels
      ),
      'smoke_current' = list(
        type = 'character',
        length = NULL,
        options = smoke_current_levels
      ),
      'chol_total_mgdl' = list(
        type = 'numeric',
        length = NULL,
        lwr = ifelse(override_boundary_errors, yes = -Inf, no = 130),
        upr = ifelse(override_boundary_errors, yes = Inf,  no = 320)
      ),
      'bp_sys_mmhg' = list(
        type = 'numeric',
        length = NULL,
        lwr = ifelse(override_boundary_errors, yes = -Inf, no = 90),
        upr = ifelse(override_boundary_errors, yes = Inf,  no = 200)
      ),
      'bp_meds' = list(
        type = 'character',
        length = NULL,
        options = bp_meds_levels
      ),
      'diabetes' = list(
        type = 'character',
        length = NULL,
        options = diabetes_levels
      ),
      'race_levels' = list(
        type = 'list',
        length = 2,
        names = c('black', 'white')
      ),
      'sex_levels' = list(
        type = 'list',
        length = 2,
        names = c('female', 'male')
      ),
      'smoke_current_levels' = list(
        type = 'list',
        length = 2,
        names = c('no', 'yes')
      ),
      'bp_meds_levels' = list(
        type = 'list',
        length = 2,
        names = c('no', 'yes')
      ),
      'diabetes_levels' = list(
        type = 'list',
        length = 2,
        names = c('no', 'yes')
      )
    )
  )

  check_equal_lengths(
    age_years = age_years,
    race = race,
    sex = sex,
    smoke_current = smoke_current,
    chol_total_mgdl = chol_total_mgdl,
    chol_hdl_mgdl = chol_hdl_mgdl,
    bp_sys_mmhg = bp_sys_mmhg,
    bp_meds = bp_meds,
    diabetes = diabetes
  )

  check_nas(
    age_years = age_years,
    race = race,
    sex = sex,
    smoke_current = smoke_current,
    chol_total_mgdl = chol_total_mgdl,
    chol_hdl_mgdl = chol_hdl_mgdl,
    bp_sys_mmhg = bp_sys_mmhg,
    bp_meds = bp_meds,
    diabetes = diabetes
  )

  # recoding categorical data ----

  race_recode <- race
  race_recode[race %in% race_levels$black] <- 'black'
  race_recode[race %in% race_levels$white] <- 'white'

  sex_recode <- sex
  sex_recode[sex %in% sex_levels$female] <- 'female'
  sex_recode[sex %in% sex_levels$male] <- 'male'

  bp_meds_recode <-
    as.numeric(tolower(bp_meds) %in% tolower(bp_meds_levels$yes))

  bp_meds_recode[is.na(bp_meds)] <- NA_integer_

  smoke_current_recode <-
    as.numeric(tolower(smoke_current) %in% tolower(smoke_current_levels$yes))

  smoke_current_recode[is.na(smoke_current)] <- NA_integer_

  diabetes_recode <-
    as.numeric(tolower(diabetes) %in% tolower(diabetes_levels$yes))

  diabetes_recode[is.na(diabetes)] <- NA_integer_

  ._pcr_fun <- switch(equation_version,
                      'Goff_2013' = ._pcr_Goff_2013,
                      'Yadlowsky_2018' = ._pcr_Yadlowsky_2013)

  ._pcr_fun(
    age_years = age_years,
    race = race_recode,
    sex = sex_recode,
    smoke_current = smoke_current_recode,
    chol_total_mgdl = chol_total_mgdl,
    chol_hdl_mgdl = chol_hdl_mgdl,
    bp_sys_mmhg = bp_sys_mmhg,
    bp_meds = bp_meds_recode,
    diabetes = diabetes_recode,
    year = year
  )

}

._pcr_Goff_2013 <- function(age_years,
                            race,
                            sex,
                            smoke_current,
                            chol_total_mgdl,
                            chol_hdl_mgdl,
                            bp_sys_mmhg,
                            bp_meds,
                            diabetes,
                            year){

  # transforming data for input into PCR equations ----
  ln_age        <- log(age_years)
  ln_chol_total <- log(chol_total_mgdl)
  ln_chol_hdl   <- log(chol_hdl_mgdl)
  ln_sbp        <- log(bp_sys_mmhg)

  # internal data for computation of individual sums ----

  ._data <- data.frame(
    race = race,
    sex = sex,
    ln_age = ln_age,
    ln_age_squared = ln_age^2,
    ln_chol_total = ln_chol_total,
    ln_age_x_ln_chol_total = ln_age * ln_chol_total,
    ln_chol_hdl = ln_chol_hdl,
    ln_age_x_ln_chol_hdl = ln_age * ln_chol_hdl,
    ln_treated_sbp = ln_sbp * bp_meds,
    ln_untreated_sbp = ln_sbp * (bp_meds == 0),
    ln_age_x_ln_treated_sbp = ln_age * ln_sbp * bp_meds,
    ln_age_x_ln_untreated_sbp = ln_age * ln_sbp * (bp_meds == 0),
    smoker = smoke_current,
    ln_age_x_smoker = ln_age * smoke_current,
    diabetes = diabetes,
    stringsAsFactors = FALSE
  )

  ._data$._ID <- seq_len(nrow(._data))

  base_surv <- switch(as.character(year),
    "10" = c(0.95334, 0.96652, 0.89536, 0.91436),
    "5"  = c(0.98194, 0.98898, 0.95726, 0.96254)
  )

  # model coefficients ----

  # coefficients vary by race/sex and are copied from:
  # https://github.com/cerner/ascvd-risk-calculator/blob/master/app/load_fhir_data.js

  race_sex_coefs <- data.frame(
    sex = c('female', 'female', 'male', 'male'),
    race = c('black', 'white', 'black', 'white'),
    base_surv = base_surv,
    coef_mean                      = c(86.6081 , -29.1817, 19.5425, 61.1816),
    coef_ln_age                    = c(17.1141 , -29.799 , 2.469  , 12.344),
    coef_ln_age_squared            = c(0       , 4.884   , 0      , 0),
    coef_ln_chol_total             = c(0.9396  , 13.54   , 0.302  , 11.853),
    coef_ln_age_x_ln_chol_total    = c(0       , -3.114  , 0      , -2.664),
    coef_ln_chol_hdl               = c(-18.9196, -13.578 , -0.307 , -7.99),
    coef_ln_age_x_ln_chol_hdl      = c(4.4748  , 3.149   , 0      , 1.769),
    coef_ln_treated_sbp            = c(29.2907 , 2.019   , 1.916  , 1.797),
    coef_ln_age_x_ln_treated_sbp   = c(-6.4321 , 0       , 0      , 0),
    coef_ln_untreated_sbp          = c(27.8197 , 1.957   , 1.809  , 1.764),
    coef_ln_age_x_ln_untreated_sbp = c(-6.0873 , 0       , 0      , 0),
    coef_smoker                    = c(0.6908  , 7.574   , 0.549  , 7.837),
    coef_ln_age_x_smoker           = c(0       , -1.665  , 0      , -1.795),
    coef_diabetes                  = c(0.8738  , 0.661   , 0.645  , 0.658),
    stringsAsFactors = FALSE
  )

  # risk computation ----

  ._data <- merge(x = ._data,
                  y = race_sex_coefs,
                  by = c('race', 'sex'),
                  all.x = TRUE,
                  all.y = FALSE,
                  sort = FALSE)

  ._data$ind_sum <- with(
    ._data,
    coef_ln_age * ln_age +
      coef_ln_age_squared * ln_age_squared +
      coef_ln_chol_total * ln_chol_total +
      coef_ln_age_x_ln_chol_total * ln_age_x_ln_chol_total +
      coef_ln_chol_hdl * ln_chol_hdl +
      coef_ln_age_x_ln_chol_hdl * ln_age_x_ln_chol_hdl +
      coef_ln_treated_sbp * ln_treated_sbp +
      coef_ln_untreated_sbp * ln_untreated_sbp +
      coef_ln_age_x_ln_treated_sbp * ln_age_x_ln_treated_sbp +
      coef_ln_age_x_ln_untreated_sbp * ln_age_x_ln_untreated_sbp +
      coef_smoker * smoker +
      coef_ln_age_x_smoker * ln_age_x_smoker +
      coef_diabetes * diabetes
  )

  # compute risk using individuals' sum of terms ----

  output <- with(._data, 1 - base_surv^exp(ind_sum - coef_mean))

  # merge() does not maintain order, so re-order here
  output[order(._data$._ID)]


}

._pcr_Yadlowsky_2013 <- function(age_years,
                       race,
                       sex,
                       smoke_current,
                       chol_total_mgdl,
                       chol_hdl_mgdl,
                       bp_sys_mmhg,
                       bp_meds,
                       diabetes,
                       year) {

  sex_coefs <- data.frame(
    sex = c('female', 'male'),
    intercept                  = c(-12.823110, -11.679980),
    coef_age                   = c( 0.106501,   0.064200),
    coef_black                 = c( 0.432440,   0.482835),
    coef_sbp_squared           = c( 0.000056,  -0.000061),
    coef_sbp                   = c( 0.017666,   0.038950),
    coef_bp_meds               = c( 0.731678,   2.055533),
    coef_diabetes              = c( 0.943970,   0.842209),
    coef_smoke_current         = c( 1.009790,   0.895589),
    coef_chol_ratio            = c( 0.151318,   0.193307),
    coef_age_x_black           = c(-0.008580,   0.000000),
    coef_sbp_x_bp_meds         = c(-0.003647,  -0.014207),
    coef_sbp_x_black           = c( 0.006208,   0.011609),
    coef_bp_meds_x_black       = c( 0.152968,  -0.119460),
    coef_age_x_sbp             = c(-0.000153,   0.000025),
    coef_black_x_diabetes      = c( 0.115232,  -0.077214),
    coef_black_x_smoke_current = c(-0.092231,  -0.226771),
    coef_black_x_chol_ratio    = c( 0.070498,  -0.117749),
    coef_black_x_sbp_x_bp_meds = c(-0.000173,   0.004190),
    coef_black_x_sbp_x_age     = c(-0.000094,  -0.000199),
    stringsAsFactors = FALSE
  )

  # transforming data for input into PCR equations
  black <- as.numeric(race == 'black')

  ._data <- data.frame(
    sex = sex,
    age = age_years,
    black = black,
    sbp_squared = bp_sys_mmhg^2,
    sbp = bp_sys_mmhg,
    bp_meds = bp_meds,
    diabetes = diabetes,
    smoke_current = smoke_current,
    chol_ratio = chol_total_mgdl / chol_hdl_mgdl,
    age_x_black = age_years * black,
    sbp_x_bp_meds = bp_sys_mmhg * bp_meds,
    sbp_x_black = bp_sys_mmhg * black,
    bp_meds_x_black = bp_meds * black,
    age_x_sbp = age_years * bp_sys_mmhg,
    black_x_diabetes = black * diabetes,
    black_x_smoke_current = black * smoke_current,
    black_x_chol_ratio = black * chol_total_mgdl / chol_hdl_mgdl,
    black_x_sbp_x_bp_meds = black * bp_sys_mmhg * bp_meds,
    black_x_sbp_x_age = black * bp_sys_mmhg * age_years,
    stringsAsFactors = FALSE
  )

  ._data$._ID <- seq_len(nrow(._data))

  ._data <- merge(x = ._data,
                  y = sex_coefs,
                  by = 'sex',
                  all.x = TRUE,
                  all.y = FALSE,
                  sort = FALSE)

  ._data$ind_sum <- with(
    ._data,
    intercept +
      coef_age * age +
      coef_black * black +
      coef_sbp_squared * sbp_squared +
      coef_sbp * sbp +
      coef_bp_meds * bp_meds +
      coef_diabetes * diabetes +
      coef_smoke_current * smoke_current +
      coef_chol_ratio * chol_ratio +
      coef_age_x_black * age_x_black +
      coef_sbp_x_bp_meds * sbp_x_bp_meds +
      coef_sbp_x_black * sbp_x_black +
      coef_bp_meds_x_black * bp_meds_x_black +
      coef_age_x_sbp * age_x_sbp +
      coef_black_x_diabetes * black_x_diabetes +
      coef_black_x_smoke_current * black_x_smoke_current +
      coef_black_x_chol_ratio * black_x_chol_ratio +
      coef_black_x_sbp_x_bp_meds * black_x_sbp_x_bp_meds +
      coef_black_x_sbp_x_age * black_x_sbp_x_age
  )

  output <- with(._data, 1 / (1 + exp(-ind_sum)))

  # merge() does not maintain order, so re-order here
  output[order(._data$._ID)]


}







#' 10-year ASCVD Risk Calculator
#'
#' This function implements the Pooled Cohort Risk equations from
#'  Goff et al, 2013. These equations predict 10-year risk of a
#'  first ASCVD event (atherosclerotic cardiovascular disease).
#'
#' @param age_years numeric vector of age values, in years.
#'
#' @param race character vector of race values. Categories should include
#'   only 'black' or 'white'. If the race variable has additional categories
#'   other than 'black' or 'white', then group all non 'black' values into
#'   the 'white' category.
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
#' @param diabetes character vector of diabetes status. Categories
#'   should include only 'no' and 'yes'. For example, if diabetes is present,
#'   the value should be 'yes'.
#'
#' @return a numeric vector with 10-year predicted risk values for ASCVD events.
#'
#' @export
#'
#' @references Goff DC, Lloyd-Jones DM, Bennett G, Coady S, D’agostino RB,
#'   Gibbons R, Greenland P, Lackland DT, Levy D, O’donnell CJ, Robinson JG.
#'   2013 ACC/AHA guideline on the assessment of cardiovascular risk: a report
#'   of the American College of Cardiology/American Heart Association Task
#'   Force on Practice Guidelines. Journal of the American College of Cardiology.
#'   2014 Jul 1;63(25 Part B):2935-59. DOI: 10.1016/j.jacc.2014.03.006
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
#' # HDL–C 50 mg/dL
#' chol_hdl_mgdl = rep(50, times = 4)
#' # untreated systolic BP 120 mm Hg
#' bp_sys_mmhg = rep(120, times = 4)
#' bp_meds = rep('no', times = 4)
#' # nonsmoker
#' smoke_current = rep('no', times = 4)
#' # without diabetes
#' diabetes = rep('no', times = 4)
#'
#' pcr_probs <- pcr_ascvd_2013(
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
#' # the coefficients used in pcr_ascvd_2013()
#' round(100 * pcr_probs, 1)
#'
#'
pcr_ascvd_2013 <- function(
  age_years,
  race,
  sex,
  smoke_current,
  chol_total_mgdl,
  chol_hdl_mgdl,
  bp_sys_mmhg,
  bp_meds,
  diabetes,
  race_levels = list(black = 'black', white = 'white'),
  sex_levels = list(female = 'female', male = 'male'),
  smoke_current_levels = list(no = 'no', yes = 'yes'),
  bp_meds_levels = list(no = 'no', yes = 'yes'),
  diabetes_levels = list(no = 'no', yes = 'yes')
) {

  # argument checking ----

  check_call(
    match.call(),
    expected = list(
      'age_years' = list(
        type = 'double',
        length = NULL,
        lwr = 40,
        upr = 80
      ),
      'race' = list(
        type = 'character',
        length = NULL,
        options = unlist(race_levels)
      ),
      'sex' = list(
        type = 'character',
        length = NULL,
        options = unlist(sex_levels)
      ),
      'smoke_current' = list(
        type = 'character',
        length = NULL,
        options = unlist(smoke_current_levels)
      ),
      'chol_total_mgdl' = list(
        type = 'double',
        length = NULL,
        lwr = 130,
        upr = 320
      ),
      'bp_sys_mmhg' = list(
        type = 'double',
        length = NULL,
        lwr = 90,
        upr = 200
      ),
      'bp_meds' = list(
        type = 'character',
        length = NULL,
        options = unlist(bp_meds_levels)
      ),
      'diabetes' = list(
        type = 'character',
        length = NULL,
        options = unlist(diabetes_levels)
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

  check_input(arg_name = 'race_levels',
              arg_value = race_levels,
              expected = list(type = 'list',
                              length = 2,
                              names = c('black', 'white')))

  check_input(arg_name = 'sex_levels',
              arg_value = sex_levels,
              expected = list(type = 'list',
                              length = 2,
                              names = c('female', 'male')))

  check_input(arg_name = 'smoke_current_levels',
              arg_value = smoke_current_levels,
              expected = list(type = 'list',
                              length = 2,
                              names = c('no', 'yes')))

  check_input(arg_name = 'bp_meds_levels',
              arg_value = bp_meds_levels,
              expected = list(type = 'list',
                              length = 2,
                              names = c('no', 'yes')))

  check_input(arg_name = 'diabetes_levels',
              arg_value = diabetes_levels,
              expected = list(type = 'list',
                              length = 2,
                              names = c('no', 'yes')))

  # transforming data for input into PCR equations ----
  ln_age        <- log(age_years)
  ln_chol_total <- log(chol_total_mgdl)
  ln_chol_hdl   <- log(chol_hdl_mgdl)
  ln_sbp        <- log(bp_sys_mmhg)
  bp_treated    <- as.numeric(tolower(bp_meds) %in% tolower(bp_meds_levels$yes))
  bp_untreated  <- as.numeric(tolower(bp_meds) %in% tolower(bp_meds_levels$no))
  smoke_current <- as.numeric(tolower(smoke_current) %in% tolower(smoke_current_levels$yes))
  diabetes      <- as.numeric(tolower(diabetes) %in% tolower(diabetes_levels$yes))

  race_recode <- race
  race_recode[tolower(race) %in% tolower(race_levels$black)] <- 'black'
  race_recode[tolower(race) %in% tolower(race_levels$white)] <- 'white'

  sex_recode <- sex
  sex_recode[tolower(sex) %in% tolower(sex_levels$female)] <- 'female'
  sex_recode[tolower(sex) %in% tolower(sex_levels$male)] <- 'male'

  # internal data for computation of individual sums ----

  ._data <- data.frame(
    race = race_recode,
    sex = sex_recode,
    ln_age = ln_age,
    ln_age_squared = ln_age^2,
    ln_chol_total = ln_chol_total,
    ln_age_x_ln_chol_total = ln_age * ln_chol_total,
    ln_chol_hdl = ln_chol_hdl,
    ln_age_x_ln_chol_hdl = ln_age * ln_chol_hdl,
    ln_treated_sbp = ln_sbp * bp_treated,
    ln_untreated_sbp = ln_sbp * bp_untreated,
    ln_age_x_ln_treated_sbp = ln_age * ln_sbp * bp_treated,
    ln_age_x_ln_untreated_sbp = ln_age * ln_sbp * bp_untreated,
    smoker = smoke_current,
    ln_age_x_smoker = ln_age * smoke_current,
    diabetes = diabetes
  )

  # model coefficients ----

  # coefficients vary by race/sex and are copied from:
  # https://github.com/cerner/ascvd-risk-calculator/blob/master/app/load_fhir_data.js

  race_sex_coefs <- data.frame(
    sex = c('female', 'female', 'male', 'male'),
    race = c('black', 'white', 'black', 'white'),
    base_surv                      = c(0.95334 , 0.96652 , 0.89536, 0.91436),
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
    coef_diabetes                  = c(0.8738  , 0.661   , 0.645  , 0.658)
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

  with(._data, 1 - base_surv^exp(ind_sum - coef_mean))

}


#' @rdname pcr_ascvd_2013
#' @export
pcr_ascvd_2018 <- function(age_years, race, sex, smoke_current,
                                chol_total_mgdl, chol_hdl_mgdl, bp_sys_mmhg,
                                bp_meds, diabetes) {

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
    coef_black_x_sbp_x_age     = c(-0.000094,  -0.000199)
  )

  # transforming data for input into PCR equations
  black    <- as.numeric(race == 'black')
  bp_meds  <- as.numeric(bp_meds == 'yes')
  diabetes <- as.numeric(diabetes == 'yes')
  smoke_current <- as.numeric(smoke_current == 'yes')

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
    black_x_sbp_x_age = black * bp_sys_mmhg * age_years
  )

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

  round(
    with(._data, 1 / (1 + exp(-ind_sum))),
    digits = 4L
  )

}





test_that(
  desc = "miscellaneous special cases work",
  code = {

    expect_invisible(
      check_arg_length(arg_value = c(1,2,3),
                       arg_name = 'a',
                       expected_length = NULL)
    )

    expect_invisible(
      check_arg_bounds(arg_value = 1,
                       arg_name = 'a',
                       bound_lwr = 0,
                       bound_upr = 2)
    )

    expect_error(
      check_arg_bounds(arg_value = 2,
                       arg_name = 'a',
                       bound_lwr = 0,
                       bound_upr = 1),
      "max\\(a\\) is 2 but should be <= 1"
    )

    expect_error(
      check_arg_type(arg_value = 'a',
                     arg_name = 'a',
                     expected_type = 'list'),
      "a should have type <list>"
    )

    expect_error(
      check_arg_is(arg_value = 'a',
                   arg_name = 'a',
                   expected_class = 'list'),
      "a should inherit from class <list>"
    )

    expect_error(
      check_arg_length(arg_value = 1,
                       arg_name = 'a',
                       expected_length = 2),
      'a should have length <2>'
    )

    expect_error(
      check_arg_names(arg_name = 'a',
                      arg_names = c('bad', 'good'),
                      valid_options = c('good', 'better', 'best')),
      'names of a should be <good, better and best>'
    )

    expect_invisible(
      check_input(arg_name = 'a',
                  arg_value = c(1,2,3),
                  expected = list(length = 3,
                                  type = 'numeric',
                                  lwr = 0,
                                  upr = 4,
                                  class = 'numeric'))
    )

    expect_invisible(
      check_input(arg_name = 'a',
                  arg_value = c(first = '1', second = '2', third = '3'),
                  expected = list(length = 3,
                                  type = 'character',
                                  options = c('0', '1', '2', '3', '4'),
                                  class = 'character',
                                  names = c('first', 'second', 'third')))
    )

    expect_error(
      check_equal_lengths(
        a = c(1,2),
        b = c(2)
      ),
      "all input data should have the same length"
    )

    expect_error(
      check_arg_bounds(arg_value = -1,
                       arg_name = 'a',
                       bound_lwr = 0,
                       bound_upr = 1),
      "min\\(a\\) is -1 but should be >= 0"
    )

    expect_invisible(
      check_arg_is_valid(arg_value = ('x'),
                         arg_name = 'a',
                         valid_options = c("x"))
    )

    expect_error(
      check_arg_is_valid(arg_value = ('x'),
                         arg_name = 'a',
                         valid_options = c("y", "z")),
      "a should be <y or z>"
    )


  }
)

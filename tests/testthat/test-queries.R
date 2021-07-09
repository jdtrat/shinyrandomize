
params <- c("person", "favorite_food", "best_cat")
values <- c("jdtrat", "sushi", "tucker")
expected_string <- "?person=jdtrat&favorite_food=sushi&best_cat=tucker"

test_that("format_query_param - multiple pairs - works", {

  query_string <- format_query_param(param = params,
                                       value = values)

  expect_equal(query_string, expected_string)

})

test_that("format_query_param - single pair - works", {

  expect_equal(format_query_param(param = params[1],
                                    value = values[1]),
               paste0("?", params[1], "=", values[1]))

  expect_equal(format_query_param(param = params[2],
                                    value = values[2]),
               paste0("?", params[2], "=", values[2]))

  expect_equal(format_query_param(param = params[3],
                                    value = values[3]),
               paste0("?", params[3], "=", values[3]))
})

test_that("extract_query_params (non interactive) - works", {

  query_values <- vapply(params, function(param) {
    extract_query_param(param = param,
                        active_session = FALSE,
                        query_string = expected_string)
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
    )

  expect_equal(query_values, values)

})

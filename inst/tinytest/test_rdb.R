cat("\n", "Context: Right combination of arguments for rdb", "\n")

# Fetch by dimensions
expect_error(rdb(dimensions = "x", provider_code = NULL, dataset_code = "y"))
expect_error(rdb(dimensions = "x", provider_code = "y", dataset_code = NULL))
expect_error(rdb(dimensions = "x", provider_code = NULL, dataset_code = NULL))
expect_error(rdb(dimensions = c("x1", "x2"), provider_code = "y", dataset_code = "z"))
expect_error(rdb(dimensions = list(), provider_code = "y", dataset_code = "z"))
expect_error(rdb(dimensions = list("x1", "x2"), provider_code = "y", dataset_code = "z"))
expect_error(rdb(dimensions = list(elt1 = "x1", "x2"), provider_code = "y", dataset_code = "z"))

# Fetch by mask
expect_error(rdb(mask = "IMF", provider_code = NULL, dataset_code = "y"))
expect_error(rdb(mask = "IMF", provider_code = "y", dataset_code = NULL))
expect_error(rdb(mask = "IMF", provider_code = NULL, dataset_code = NULL))
expect_error(rdb(mask = c("x1", "x2"), provider_code = "y", dataset_code = "z"))

# Fetch by mask with wrong provider_code
expect_error(rdb(mask = "x", provider_code = "y", dataset_code = "z"))

# Fetch by ids
expect_error(rdb(ids = character()))

# Nothing provided
expect_error(rdb())

# api_link length is greater than one or equal to zero
expect_error(rdb(api_link = c("url1", "url2")))
expect_error(rdb(api_link = character()))

# api_link is not a character string
expect_error(rdb(api_link = 0))

# use_readLines is NULL
expect_error(rdb(api_link = "url", use_readLines = NULL))

# use_readLines length is greater than one or equal to zero
expect_error(rdb(api_link = "url", use_readLines = c(TRUE, FALSE)))
expect_error(rdb(api_link = "url", use_readLines = logical()))

# use_readLines is not a logical
expect_error(rdb(api_link = "url", use_readLines = "TRUE"))

# curl_config is not a curl_handle object or a named list
expect_error(rdb(api_link = "url", curl_config = TRUE))
expect_error(rdb(api_link = "url", curl_config = list(1)))

# filters is not a valid list
expect_error(rdb(api_link = "url", filters = TRUE))
expect_error(rdb(api_link = "url", filters = "filters"))
expect_error(rdb(api_link = "url", filters = list()))
expect_error(rdb(api_link = "url", filters = list(code = "interpolate", parameter = NULL)))
expect_error(rdb(api_link = "url", filters = list(code = "interpolate", parameters = list())))
expect_error(rdb(api_link = "url", filters = list(list(code = "interpolate", parameters = list(frequency = "quarterly", method = "spline")), list(code = "aggregate", parameters = list()))))

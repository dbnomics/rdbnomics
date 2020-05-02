cat("\n", "Context: Right arguments for rdb_providers", "\n")

# Code is logical and of length one
expect_error(rdb_providers(code = logical()))
expect_error(rdb_providers(code = "TRUE"))

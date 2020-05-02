cat("\n", "Context: Right arguments for rdb_last_updates", "\n")

# All is logical and of length one
expect_error(rdb_last_updates(all = logical()))
expect_error(rdb_last_updates(all = "TRUE"))

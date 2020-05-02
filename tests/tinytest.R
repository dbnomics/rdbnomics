if (requireNamespace("tinytest", quietly = TRUE)) {
  home <- length(unclass(utils::packageVersion("rdbnomics"))[[1]]) == 4
  tinytest::test_package("rdbnomics", at_home = home)
}
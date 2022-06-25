library(plumber)
library(keyring)

path <- "C:/Users/Quentin GOLLENTZ/Documents/PROJET PERSO/bureaudevote/script/"


keyring::key_set_with_value("plumber_baseblanc3", password = plumber::random_cookie_key())

pr(paste0(path,"getApi/getFilterDemocratie/plumber.R")) %>%
  pr_cookie(
    keyring::key_get("plumber_baseblanc3"),
    name = "counter"
  ) %>%
  pr_filter("sessionCounter", function(req) {
    count <- 0
    if (!is.null(req$session$counter)){
      count <- as.numeric(req$session$counter)
    }
    req$session$counter <- count + 1
    print(req$session$counter)
    forward()
      }) %>%
  pr_run(port = 5762)


list.files()
r <- plumber::plumb("/home/goldentzgrahamz/script/api/test.R")
r$run(host = "192.168.1.21")

library(rtweet)
library(dplyr)
library(twitteR)

my_app <- rtweet_app()

auth_get()
auth_setup_default()

auth_as("my_auth")
useRs <- search_users("#rstats", n = 200)
timeline <-get_timeline("goldentzgrahamz")
my_app <- rtweet_app()
auth_as(my_app)



my_auth_2 <- rtweet_app(bearer_token = "AAAAAAAAAAAAAAAAAAAAAOWXngEAAAAAnEst3ZgYc1E%2FsK0WI3PnCyRHzjE%3DMcXni6eWMue6N0cifNC1d0QDfB0MiJCykMmk63Arlku78fcOl4")
my_auth <- rtweet_user()

library(rtweet)
library(dplyr)
my_app <- rtweet_app()

auth_get()
auth_setup_default()

auth_as("my_auth")

timeline <-get_timeline("goldentzgrahamz")

function_destroy <- function(x){
  post_destroy(x)}
posts_list <-timeline$id_str

lapply(posts_list,function_destroy)


auth_setup_default(my_auth_2)
auth_setup_default()
df <- search_tweets("#rstats", token = my_auth_2)
function_destroy <- function(x){
  post_destroy(x)}
posts_list <-timeline$id_str

lapply(posts_list,function_destroy)

token <- create_token(
  app = "rtweet_token",
  consumer_key = "r4wW00kQR8ePUD069k5i06EBl",
  consumer_secret = "nxZOMB0QnK3axoBYYfbeSqwZTSxDlyZuVN8pfP3WM64qO5kyIx")
setup_twitter_oauth("r4wW00kQR8ePUD069k5i06EBl","nxZOMB0QnK3axoBYYfbeSqwZTSxDlyZuVN8pfP3WM64qO5kyIx")

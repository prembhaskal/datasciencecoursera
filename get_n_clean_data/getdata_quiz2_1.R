# quiz2 github authorisation.
# help link https://github.com/hadley/httr/blob/master/demo/oauth2-github.r

library(httr)
library(jsonlite)

gitendpoints <- oauth_endpoints("github")
my_app <- oauth_app(appname = "github", key = "5ead3aa7d96fe08f04c5", secret = "5805df56b6b0b7711c750643ada28785e9dbf433")

github_token <- oauth2.0_token(endpoint = gitendpoints, app = my_app)

gtoken <- config(token = github_token)
resp <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(resp)
siteContent <- content(resp)
con -
siteData <- fromJSON(toJSON(siteContent))
colnames(siteData)
siteData[siteData$name == "datasharing", c("url", "name", "created_at")]

library(jsonlite)

system("wget -O nflstats.json 'http://api.fantasy.nfl.com/v1/players/stats?statType=seasonStats&season=2010&week=1&format=json'")
system("cat nflstats.json")

df1 <- fromJSON('nflstats.json')
class(df1)
class(df1$players)
head(df1$players)
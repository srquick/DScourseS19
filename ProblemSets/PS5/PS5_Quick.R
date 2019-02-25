library(rvest)
library(stringr)
library(magrittr)

# Karl Lagerfeld Vcard from Wikipedia
webpage	<- read_html("https://en.wikipedia.org/wiki/Karl_Lagerfeld")
table <- webpage %>%
  html_nodes("table.vcard") %>%
  html_table(header=F)
table <- table[[1]]
ftable <- as.data.frame(table)

## *Insert farmer meme that says "It's not much, but it's honest work"*
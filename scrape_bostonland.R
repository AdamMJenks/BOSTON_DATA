library(rvest)
library(XML)

## Addresses
addresses <- read_html("http://www.bostonplans.org/work-with-us/bpda-owned-land?viewall=1") %>% 
  html_nodes('caption')
parsed <- sapply(addresses, xmlParse)
addresses_text <- lapply(parsed, function(x) xmlToList(x)$a$text)

area <- read_html("http://www.bostonplans.org/work-with-us/bpda-owned-land?viewall=1") %>% 
  html_nodes('h2')

parsed2 <- sapply(area, xmlParse)
text_values <- lapply(parsed2, function(x) xmlToList(x)$text)

neighborhood <- text_values[seq(1, length(text_values), by=3)]
type <- text_values[seq(2, length(text_values), by=3)]
sqft <- text_values[seq(3, length(text_values), by=3)]

the_df <- data.frame(address = as.character(addresses_text), neighborhood = as.character(neighborhood), type=as.character(type), sqft=as.character(sqft))

the_df$sqft <- as.numeric(gsub("\n|,|sq\\.ft", "", as.character(the_df$sqft)))

write.csv(the_df, file="data/land_parcels.csv", row.names = FALSE)

install.packages("tidyverse")
install.packages("rvest")

# posit clod -> R na nuvem

# Web Scrapping 

library(tidyverse)
library(rvest)

# https://rpubs.com/pedrofranklin/web-scraping

url <- "https://www.imdb.com/chart/top/"

html <- read_html(url)

html|>
  # pega os elementos de h3
  html_elements("h3") |>
  html_text2()



library(stringr)

titulos <- html |> 
  html_elements("ul.ipc-metadata-list") |>
  html_elements("li") |>
  html_elements("h3") |>
  html_text2() |>
  str_replace("\\d+\\.\\s","")

v <- c("21", "ola", ".3aw7")
str_detect(v, "\\d")
str_view(v, "\\d")
str_view(v, "\\d+")
str_view(v, ".")
str_view(v, "\\.\\d")


anos <- html |> 
  html_elements("ul.ipc-metadata-list") |>
  html_elements("li") |>
  html_elements("div.sc-be6f1408-7") |>
  html_element("span.sc-be6f1408-8") |>
  html_text2() 


anos

data <- data.frame(titulos, anos)
data

# filme, diretor e ano
# tarefa

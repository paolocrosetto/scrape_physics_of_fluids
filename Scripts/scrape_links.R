## this file scrapes the AIP website for the journal "Physics of Fluids"
## to get a list of all links for all articles published 2004-24

######
## 0. Libraries
library(tidyverse)
library(rvest)
library(httr)



######
## 0. Functions

## get a list of articles in one issue
article_links <- function(y, m) {
  
  print(paste0("Volume: ", y, "; Issue: ", m))
  
  url <- paste0("https://pubs.aip.org/aip/pof/issue/", y, "/", m)
  
  pg <- read_html(url)
  
  out <- pg %>% 
    html_nodes(".item-title a") %>% 
    html_attr("href")
    
  tibble(year = y, 
         month = m, 
         link = out)
}

## scrape the page
get_info <- function(link) {
  pg <- read_html(link)
  
  # authors
  authors <- pg %>% 
    html_nodes(".stats-author-info-trigger") %>% 
    html_text() %>% 
    paste(collapse = ", ")
  
  ## title
  title <- pg %>% 
    html_nodes(".article-title-main") %>% 
    html_text2()
  
  ## date
  date <- pg %>% 
    html_nodes(".article-date") %>% 
    html_text2() %>% 
    mdy()
  
  ## tat
  tat <- pg %>% 
    html_nodes(".history-entry") %>% 
    html_text2()
  
  received <- tat[1] %>% 
    str_remove("Received:\n") %>% 
    mdy()
  
  accepted <- tat[2] %>% 
    str_remove("Accepted:\n") %>% 
    mdy()
  
  turnaorund <- accepted - received
  
  ## put into data frame
  out <- tibble(title, date, authors, received, accepted) %>% 
    mutate(turnaorund = accepted - received)
  
  out
}


#####
## 1. preliminaries

## set user agent to appear as a browser to the website
ua <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36 OPR/114.0.0.0"
httr::set_config(httr::user_agent(ua))


#####
## 2. Scrape init

## initialize empty table
links <- tibble(year = numeric(), 
                month = numeric(), 
                link = character())


######
## 3. Scrape run

## visualize the months with 0 links
links %>% janitor::tabyl(year, month)

## this loop ensures that only issues that were not scraped are scraped
## this also means that once the run has ended, you can check if some pages where skipped, got a 403 / 404 response
## and then you can run it again; it will scrape only those not already scraped
for (y in seq(16,36,1)) {
  for (m in seq(1,12)) {
    
    n <- links %>% 
      filter(year == y, month == m) %>% 
      nrow()
    
    if (n == 0) {
      print(paste0(y, "; ", m, ": scraping"))
      Sys.sleep(3 + 2*runif(1))                  ## waiting period to avoid bot detection
      links <- links %>% bind_rows(article_links(y,m))
      Sys.sleep(2)                              ## waiting period to avoid bot detection
    }
    
    else {
      print(paste0(y, "; ", m, ": skipping"))
    }
  }
  
}


## export list of links to csv
links %>% 
  write_csv("all_articles.csv")

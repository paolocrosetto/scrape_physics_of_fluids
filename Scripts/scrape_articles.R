## this file scrapes the AIP website for the journal "Physics of Fluids"
## given a list of all links for all articles published 2004-24
## it scrapes the content of the page (authors, title, turnaround times)


######
## 0. Libraries
library(tidyverse)
library(rvest)
library(httr)



######
## 0. Functions


## scrape the page
get_info <- function(y, m, paper, link) {
  
  user_agents <- c(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
  )
  
  response <- GET(link,
                  add_headers('User-Agent' = sample(user_agents, 1)))
  
  pg <- read_html(content(response, as = "text"))
  
  # volume and issue, for reference
  volume <- y
  issue <- m
  
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
  
  ## article type
  type <- pg %>% 
    html_nodes(".article-client_type") %>% 
    html_text2()
  
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
  out <- tibble(volume, issue, paper, type, title, date, authors, received, accepted) %>% 
    mutate(turnaorund = accepted - received)
  
  out
}


scrape <- function(links) {
  
  # wait a bit
  Sys.sleep(1.5+runif(1))
  
  print(paste0("Scraping Volume: ", links$year, "; Issue: ", links$month, "; Paper n.", links$paper))
  
  # scrape one article
  out <- get_info(links$year, links$month, links$paper, links$link)
  
  out
}



#####
## 1. preliminaries

## get a dataset of all links to be scraped in shape and ready
links <- read_csv("all_articles.csv")

# good format of links, eliminating unused variable
links <- links %>% 
  mutate(link = paste0("https://pubs.aip.org", link))

# ID of paper within an issue
links <- links %>% 
  group_by(year, month) %>% 
  mutate(paper = row_number()) %>% 
  select(year, month, paper, link)

#####
## 2. First run

## initialize table with first row
scraped <- get_info(links$year[1], links$month[1], links$paper[1], links$link[1])

####
## 3. all other runs

## after the first urn: retrieve from disk
scraped <- read_csv("PoF_scraped.csv") %>% 
  mutate(paper = as.integer(paper), 
         turnaorund = as.duration(turnaorund))


## generate a set of lines not already scraped
indicator <- scraped %>% 
  mutate(indicator = paste(volume,issue, paper, sep = "-" )) %>% 
  pull(indicator)



## links restricted to NOT being already scraped
links <- links %>% 
  mutate(indicator = paste(year,month, paper, sep = "-" )) %>% 
  anti_join(tibble(indicator)) %>% 
  arrange(year, month, paper)

links %>% janitor::tabyl(year, month)

## all other runs
j = 0
for (i in seq(dim(links)[1])) {
  tryCatch({
    
    j = j + 1
    print(j)
    
    # scrape one page and add it to the data
    scraped <- scraped %>% 
      bind_rows(scrape(links[i,]))
  
    # wait a bit
    Sys.sleep(7 + 3*runif(1))
    
    # every 30 calls write to csv, just to be sure
    if (j == 30) {
      scraped %>% 
        write_csv("PoF_scraped_temp.csv")
      j = 0
    }
    
  }, error = function(e) {
    # This block is executed if an error occurs
    message("Error occurred while processing URL: ", links[i,4])
    message("Error details: ", e$message)
  }
  )
}

## export scraped info to csv
scraped %>% 
  write_csv("PoF_scraped.csv")


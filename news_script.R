library(tidyverse)
library(rvest)
library(data.table)

# original URL
ps_url <- 'https://www.project-syndicate.org/section/politics-world-affairs'


# easily modifiable URL
ps_url <- 'https://www.project-syndicate.org/section/politics-world-affairs/commentaries?take=100'

scrap_project_syndicate <- function(section_name, nr_of_articles) {
  
  # url
  ps_url <- paste0('https://www.project-syndicate.org/section/', section_name, '/commentaries?take=',
                   nr_of_articles)
  
  # read html
  ps_page <- read_html(ps_url)
  
  # write page to local directory
  write_html(ps_page, 'ps.html')
  
  ps_titles <- ps_page %>% 
    html_nodes("[class='listing__header']") %>% 
    html_nodes("[class='listing__title']") %>% 
    html_text(trim = T)
  
  # dates
  ps_dates <- ps_page %>% 
    html_nodes('li') %>% 
    html_nodes("[class='listing listing--default ']") %>% 
    html_nodes('p') %>% 
    html_nodes("[class='bl-pubdate vl-divider']") %>% 
    html_text()
  
  # summaries
  ps_text <- ps_page %>% 
    html_nodes('li') %>% 
    html_nodes("[class='listing listing--default ']") %>% 
    html_nodes('p') %>% 
    html_text(trim = T) %>% 
    lapply(strsplit, '\n') %>% 
    lapply(`[[`, i = 1) %>% 
    lapply(`[[`, i = 4) %>% 
    unlist()
  
  # authors
  # to work on: get all authors, not only first
  ps_authors <- ps_page %>% 
    html_nodes('li') %>% 
    html_nodes("[class='listing listing--default ']") %>% 
    html_nodes('p') %>% 
    html_node('a') %>% 
    html_text()
  
  # concat authors with summaries
  ps_summaries <- paste(ps_authors, ps_text)
  
  # links
  ps_links <- ps_page %>% 
    html_nodes('li') %>% 
    html_nodes("[class='listing listing--default ']") %>% 
    html_nodes('a') %>% 
    html_attr('href')
  
  ps_links <- ps_links[grepl("commentary", ps_links) & !grepl("#comments", ps_links)] %>% 
    unique() %>% 
    paste0('https://www.project-syndicate.org', .)
  
  
  table <- data.table(ps_titles, ps_dates, ps_authors, ps_links, ps_summaries)
  
}

# sections on project syndicate
section_names <- list("economics", "politics-world-affairs", "global-health-development", "environment-sustainability",
                      "culture-society", "innovation-technology")


for (i in section_names) {
  table_name <- paste0(i, '_table')
  assign(table_name, scrap_project_syndicate(i, 30))
}


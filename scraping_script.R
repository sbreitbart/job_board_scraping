# USE THIS SITE AS TUTORIAL:
# https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/


# Setup
# install.packages("rvest")
library('rvest')


####################################################################
# ECOLOG Trial: pausing because it requires a password to enter forum
# and I don't know enough html for that
####################################################################

# # Specifying the url for desired website to be scraped
# url <- 'https://eservices.esa.org/Eservices/Contacts/Sign_In.aspx'
# pgsession <- session(url)               ## create session
# pgform    <- html_form(pgsession)[[1]]       ## pull form from session
# 
# 
# filled_form <- set_values(pgform,
#                           `ctl00$Header2$HeaderTop1$tbUsername` = "sbreitbart@wesleyan.edu", 
#                           `ctl00$Header2$HeaderTop1$tbPassword` = "Barcel0na!")
# 
# submit_form(pgsession,filled_form)
# 
# #Reading the HTML code from the website
# webpage <- read_html(url)
# 
# #Using CSS selectors to scrape the rankings section
# rank_data_html <- html_nodes(webpage,'td , .explanation')
# 
# #Converting the ranking data to text
# rank_data <- html_text(rank_data_html)
# 
# #Let's have a look at the rankings
# head(rank_data)

####################################################################
# Natural resources job board
####################################################################

#Specifying the url for desired website to be scraped
url <- 'https://wfscjobs.tamu.edu/?job_category=full-time-positions'
url2 <- 'https://wfscjobs.tamu.edu/?job_category=post-doctoral-appointments'

#Reading the HTML code from the website
webpage <- read_html(url)
webpage2 <- read_html(url2)

#Using CSS selectors to scrape the rankings section
job_data_html <- html_nodes(webpage,
                             '.job-title')

postdoc_data_html <- html_nodes(webpage2,
                            '.job-title')

#Converting the ranking data to text
job_data <- html_text(job_data_html) %>%
   as.list()

postdoc_data <- html_text(postdoc_data_html) %>%
  as.list()

library(stringr)

ds_jobs <- do.call(rbind.data.frame, job_data) %>%
  dplyr::rename(Position = 1) %>%
  filter(str_detect(Position,
                    'Data|data|Quant|quant| R '))

ds_postdocs <- do.call(rbind.data.frame, postdoc_data) %>%
  dplyr::rename(Position = 1) %>%
  filter(str_detect(Position,
                    'Data|data|Quant|quant| R '))

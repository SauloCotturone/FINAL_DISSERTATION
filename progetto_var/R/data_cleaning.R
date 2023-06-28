# Script to perform data cleaning tasks
source('R/libraries.R')


# Cleaning data ---------------------------------------------------------------

db <- read_excel("data/data.xlsx", sheet = 1)
sp <- read_excel("data/data.xlsx", sheet = 2)

str(db)
str(sp)

db <- mutate(db, date = as.Date(date)) # correct date format
sp <- mutate(sp, date = as.Date(date)) # correct date format

sp <- sp %>% 
	mutate(q = quarter(date), y = year(date), yq = str_c(y, q, sep = " q")) %>% 
	group_by(yq) %>% 
	summarise(sp500 = mean(sp500))

db <- data.frame(db, sp500 = sp$sp500)	

write_excel_csv(db, path = "data/data_cleaned.csv")


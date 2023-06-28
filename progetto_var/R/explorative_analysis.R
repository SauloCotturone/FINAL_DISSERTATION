# Script to perform Explorative Data Analysis (EDA)
source('R/libraries.R')



# Import data -------------------------------------------------------------

db <- read_csv("data/data_cleaned.csv")

View(db)
str(db) # db structure

summary(db) # db summary info
names(db) <- c("date", "GDP Deflator", "Federal Funds Rate", "Real GDP per capita", "S&P 500")

db <- db %>% 
	filter(year(date) < 2020)

# Plots on levels ---------------------------------------------------------

# single time series plot
for (var in names(db[,-1])) {
	p <- db %>% 
		pivot_longer(cols = -date, names_to = "ts", values_to = "value") %>%
		filter(ts == var) %>% 
		ggplot(aes(x = date, y = value)) +
		geom_line() +
		labs(title = var) + xlab("") + ylab("")
	print(p)
}
rm(p)

# multiple time series plot
db %>% 
	pivot_longer(cols = -date, names_to = "ts", values_to = "value") %>% 
	ggplot(aes(x = date, y = value), as.numeric = FALSE) +
	geom_line() +
	facet_wrap(~ ts, scales = "free_y", nrow = 4, ncol = 1) +
	xlab("") + ylab("")

# multiple time series plot
db %>% 
	pivot_longer(cols = -date, names_to = "ts", values_to = "value") %>% 
	ggplot(aes(x = date, y = value)) +
	geom_line() +
	facet_wrap(~ ts, scales = "free_y", nrow = 2, ncol = 2) +
	xlab("") + ylab("")



# Identify Structural Changes ---------------------------------------------

# Change points in mean
for (var in names(db[,-1])) {
	
	cp <- cpt.mean(db[[var]]) # compute change points in mean
	cp_obs <- db %>% 
		pivot_longer(cols = -date, names_to = "ts", values_to = "value") %>%
		filter(ts == var) %>% 
		slice(cpts(cp)) # select only break observations
	
	p <- db %>% 
		pivot_longer(cols = -date, names_to = "ts", values_to = "value") %>%
		filter(ts == var) %>% 
		ggplot(aes(x = date, y = value)) +
		geom_line() +
		geom_vline(data =	cp_obs, aes(xintercept = date), col = "red", 
							 cex = 0.8, alpha = 0.7, linetype = "dashed") +
		geom_text(data = cp_obs, aes(x = date, label = year(date), y = 2), 
							col = "red", hjust = 0) +
		labs(title = var) + xlab("") + ylab("")
	print(p)
	
}

# Structural changes
for (var in names(db[,-1])) {
	
	bp <- breakpoints(db[[var]] ~ 1, data = db) # compute structural breaks
	bp_obs <- db %>% 
		pivot_longer(cols = -date, names_to = "ts", values_to = "value") %>%
		filter(ts == var) %>% 
		slice(bp$breakpoints) # select only break observations
	
	p <- db %>% 
		pivot_longer(cols = -date, names_to = "ts", values_to = "value") %>%
		filter(ts == var) %>% 
		ggplot(aes(x = date, y = value)) +
		geom_line() +
		geom_vline(data =	bp_obs, aes(xintercept = date), col = "red", 
							 cex = 0.8, alpha = 0.7, linetype = "dashed") +
		geom_text(data = bp_obs, aes(x = date, label = year(date), y = 2), 
							col = "red", hjust = 0) +
		labs(title = var) + xlab("") + ylab("")
	print(p)

}



# ACF & Partial ACF on Levels ---------------------------------------------

# ACF & Partial ACF plot
for (var in names(db[,-1])) {
	par(mfrow = c(1, 2))
	acf(db[[var]], main = "")
	pacf(db[[var]], main = "")
	title(var, outer = TRUE, line = -2)
}



# Stationarity Tests ------------------------------------------------------

# Augmented Dickey-Fuller Test on Levels
adf.test(db$`GDP Deflator`) # non-stationary
adf.test(db$`Federal Funds Rate`) # stationary
adf.test(db$`Real GDP per capita`) # non-stationary
adf.test(db$`S&P 500`) # non-stationary

kpss.test(db$`Federal Funds Rate`) # non-stationary


# Augmented Dickey-Fuller Test on First Differences
adf.test(diff(db$`GDP Deflator`)) # stationary
adf.test(diff(db$`Federal Funds Rate`)) # stationary
adf.test(diff(db$`Real GDP per capita`)) # stationary
adf.test(diff(db$`S&P 500`)) # stationary

kpss.test(diff(db$`Federal Funds Rate`)) # stationary



# Plots on First Difference -----------------------------------------------

# first difference db
d_db <- data.frame(date = db$date[-1], 
									 d_gdpdef = diff(db$`GDP Deflator`),
									 d_ffr = diff(db$`Federal Funds Rate`),
									 d_rgdp = diff(log(db$`Real GDP per capita`)),
									 d_sp500 = diff(log(db$`S&P 500`)))
names(d_db) <- c("date", "Δ GDP Deflator", "Δ Federal Funds Rate", "Δ Real GDP per capita", "Δ S&P 500")

# first difference means
d_db_mean <- d_db %>% 
	pivot_longer(cols = -date, names_to = "ts", values_to = "value") %>% 
	group_by(ts) %>% 
	summarise(value = mean(value))


# single time series plot
for (var in names(d_db[,-1])) {
	p <- d_db %>% 
		pivot_longer(cols = -date, names_to = "ts", values_to = "value") %>%
		filter(ts == var) %>% 
		ggplot(aes(x = date, y = value)) +
		geom_line() +
		labs(title = var) + xlab("") + ylab("") 
	print(p)
}
rm(p)

# multiple time series plot
d_db %>% 
	pivot_longer(cols = -date, names_to = "ts", values_to = "value") %>% 
	ggplot(aes(x = date, y = value)) +
	geom_line() +
	geom_hline(data =	d_db_mean, aes(yintercept = value), col = "blue", 
						 size = 0.5, alpha = 0.7) +
	facet_wrap(~ ts, scales = "free_y", nrow = 4, ncol = 1) +
	xlab("") + ylab("")

# multiple time series plot
d_db %>% 
	pivot_longer(cols = -date, names_to = "ts", values_to = "value") %>% 
	ggplot(aes(x = date, y = value)) +
	geom_line() +
	geom_hline(data =	d_db_mean, aes(yintercept = value), col = "blue", 
						 size = 0.5, alpha = 0.7) +
	facet_wrap(~ ts, scales = "free_y", nrow = 2, ncol = 2) +
	xlab("") + ylab("")



# ACF & Partial ACF on First Difference -----------------------------------

# ACF & Partial ACF plot
for (var in names(d_db[,-1])) {
	par(mfrow = c(1, 2))
	acf(d_db[[var]], main = "")
	pacf(d_db[[var]], main = "")
	title(var, outer = TRUE, line = -2)
}



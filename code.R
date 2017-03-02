library(dplyr)
library(magrittr)
library(rvest)

setwd("E:/Dropbox/Duke/STA101")

load(url("https://stat.duke.edu/~mc301/data/movies.Rdata"))

movies %<>% 
	# Remove TV movies.
	filter(title_type != "TV Movie") %>%
	# Remove long and short runtimes.
	filter(runtime < 250) %>%
	filter(runtime >= 60)




# Add clean studios.
movies %<>% mutate(studio_lower = tolower(studio)) %>%
	mutate(studio_main =
				 	ifelse(grepl("warner", studio_lower) | 
				 				 	grepl("hbo", studio_lower) |
				 				 	grepl("orion", studio_lower) |
				 				 	grepl("line", studio_lower), "warner",
				 				 ifelse(grepl("fox", studio_lower), "fox",
				 				 			 ifelse(grepl("disney", studio_lower) |
				 				 			 			 	grepl("buena", studio_lower) | 
				 				 			 			 	grepl("miramax", studio_lower) | 
				 				 			 			 	grepl("hollywood", studio_lower) | 
				 				 			 			 	grepl("touchstone", studio_lower) | 
				 				 			 			 	grepl("touchstone", studio_lower), "disney",
				 				 			 			 ifelse(grepl("columbia", studio_lower) | 
				 				 			 			 			 	grepl("sony", studio_lower), "sony",
				 				 			 			 			 ifelse(grepl("paramount", studio_lower), "paramount",
				 				 			 			 			 			 ifelse(grepl("lions", studio_lower) | 
				 				 			 			 			 			 			 	grepl("lionsgate", studio_lower), "lionsgate",
				 				 			 			 			 			 			 ifelse(grepl("mgm", studio_lower) | 
				 				 			 			 			 			 			 			 	grepl("mgm/ua", studio_lower) | 
				 				 			 			 			 			 			 			 	grepl("artists", studio_lower), "mgm",
				 				 			 			 			 			 			 			 ifelse(grepl("weinstein", studio_lower), "weinstein",
				 				 			 			 			 			 			 			 			 ifelse(grepl("ifc", studio_lower), "ifc",
				 				 			 			 			 			 			 			 			 			 ifelse(grepl("magnolia", studio_lower) | 
				 				 			 			 			 			 			 			 			 			 			 	grepl("magnolia/magnet", studio_lower) |
				 				 			 			 			 			 			 			 			 			 			 	grepl("magnet", studio_lower), "magnolia",  
				 				 			 			 			 			 			 			 			 			 			 ifelse(grepl("universal", studio_lower) | 
				 				 			 			 			 			 			 			 			 			 			 			 	grepl("focus", studio_lower) |
				 				 			 			 			 			 			 			 			 			 			 			 	grepl("usa", studio_lower) |
				 				 			 			 			 			 			 			 			 			 			 			 	grepl("gramercy", studio_lower), "universal", "other"))))))))))))

movies %<>% select(-studio_lower)

# Extract info from IMDB.
span <- list()
for(i in 1:nrow(movies)) {
	print(movies$title[i])
	html_page <- read_html(movies$imdb_url[i])
	print(html_page %>% html_node("h1") %>% html_text())
	movie <- html_page %>% html_node("#titleDetails") %>% html_text() %>% strsplit("\n")
	span[[i]] <- movie[[1]]
}

# Extract Gross.
gross <- rep(NA, length(span))
for(i in 1:length(span)) {
	current <- span[[i]]
	text <- NA
	for(j in 1:length(current)) {
		if(grepl("Gross", current[j])) { text <- current[j] }
	}
	text <- trimws(text)
	text <- strsplit(text, "Gross:")[[1]][2]
	text <- trimws(text)
	text <- gsub("[^0-9]","",text)
	gross[i] <- as.numeric(text)
}

# Extract Budget.
budget <- rep(NA, length(span))
for(i in 1:length(span)) {
	current <- span[[i]]
	text <- NA
	for(j in 1:length(current)) {
		if(grepl("Budget", current[j])) { text <- current[j] }
	}
	text <- trimws(text)
	text <- strsplit(text, "Budget:")[[1]][2]
	text <- trimws(text)
	text <- gsub("[^0-9]","",text)
	budget[i] <- as.numeric(text)
}

# Export data.
movies <- cbind(movies, gross, budget)
movies %<>% filter(!is.na(gross))
movies %<>% filter(!is.na(budget))
save(movies, file="movies_new.RData")


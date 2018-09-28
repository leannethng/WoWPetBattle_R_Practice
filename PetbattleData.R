# Load packages --------
library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
source("config.R")

#Getting the data ------------

petsData <- jsonlite::fromJSON(site, simplifyVector = FALSE, simplifyMatrix = TRUE	
)

#Turning the data into a table from a list ------------

#Remvoving a level of hierarchy
petsData <- petsData$pets

#Turning it into a tibble
petsDataTable <- petsData %>% {
    tibble(
        #choosing the columns
        name = map_chr(., "name"),
        CanBattle = map_lgl(., "canBattle"),
        family = map_chr(., "family"),
        #Using unlist to get into the list for this one
        strongAgainst = unlist(map(.,"strongAgainst"))
    )
}

head(petsDataTable)

table(petsDataTable$CanBattle)
table(petsDataTable$strongAgainst)


#Plotting practice ------
barplot(height = table(petsDataTable$family),
        names.arg = row.names(petsDataTable$family),
        xlab = "Type",
        ylab = "Pet amount",
        main = "Amount of pets by type",
        col = "mistyrose",
        ylim = c(0, 250)
        )

                        


#Extra workings ---------

#Getting the data
#pets <- GET("")

#Pulling data
petName <- petsData %>% map_chr("name")
typeof(petName)
petsData %>% map_int(c("stats", "health"))


data <- bind_rows(petsData, .id = 'play')

names(had) <- sapply(had, function(x) x$name)
sapply(had, function(x) x$watchers)

#head(petsData, n = 1L)

#petsDataframe <- as.data.frame(petsData)
#t(petsDataframe)
#typeof(petsDataframe)

#head(petsDataframe)
summary(petsData$pets)

# petsDataframe <- data_frame()
#for (i in petsData$pets) {
#    petsDataframe <- rbind(petsData$pets)
#}

typeof(petsDataframe)
head(petsDataframe$pets[1]) 


#head(petsData$name, n = 3L)



# head(repo_pets, n = 3L)

# Apply function across all list elements to extract the name and address of each repo


repo_pets <- lapply(petsDataframe, function(x = 1:5) {
    result <- data.frame()
    #df <- data.frame()
    for (i in x){
        df <- rbind(name = i["name"],
                    family = i["family"],
                    canBattle = i["canBattle"])
    }
    #print(i)
    result
}) 

head(repo_pets)
    #print(i[[1]])

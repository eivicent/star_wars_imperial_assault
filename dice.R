rm(list =ls())
library(rvest)

dice <- function(type, amount, detail = F){
  
  aux <- switch(type, 
                Red		= matrix(c(c(1,0,0),c(2,0,0),c(2,0,0),c(2,1,0),c(3,0,0),c(3,0,0)), byrow = T, ncol = 3),
                Blue	= matrix(c(c(0,1,2),c(1,0,2),c(2,0,3),c(1,1,3),c(2,0,4),c(1,0,5)), byrow = T, ncol = 3),
                Green	= matrix(c(c(0,1,1),c(1,1,1),c(2,0,1),c(1,1,2),c(2,0,2),c(2,0,3)), byrow = T, ncol = 3),
                Yellow	= matrix(c(c(0,1,0),c(1,2,0),c(2,0,1),c(1,1,1),c(0,1,2),c(1,0,2)), byrow = T, ncol = 3)
  )
  
  aux2 <- sample.int(n = nrow(aux), size = amount, replace = T)
  
  if(detail){
    results <- data.frame(aux[aux2, ])
    names(results) <- c("dmg", "bolt", "dist")
    summary <- colMeans(results)
    output <- list(results = results, summary = summary)
  } else {    
    summary <- data.frame(colMeans(aux[aux2, ]))
    names(summary) <- c("dmg", "bolt", "dist")
    output <- list(summary = summary)
  }
  
  
  return(output)
  
}
pair_of_dices <- function(dice1, dice2, amount_of_pairs){
  
  aux1 <- dice(dice1, amount_of_pairs, detail = T)
  aux2 <- dice(dice2, amount_of_pairs, detail = T)
  
  results <- aux1$results + aux2$results
  summary <- colMeans(results)
  
  output <- list(results = results, summary = summary)
  return(output)
  
}
basic_weapon_information <- function(item){
  
  href <- item["href"]
  title <- item["title"]
  
  items_web <- read_html(paste0("https://imperial-assault.fandom.com",href))
  
  item_attributes <- items_web %>% html_nodes(".pi-font")
  item_attributes_text <- item_attributes %>% html_text
  
  name <- title
  tier <- item_attributes_text[1]
  type <- item_attributes_text[3]
  upgrades <- item_attributes_text[4]
  cost <- item_attributes_text[5] 
  item_dices <- item_attributes %>% html_nodes(xpath = "//*[(@title = 'Dice')]/img") %>% html_attrs() %>% 
    sapply(function(x) sub(".png","",x["data-image-name"]))
  
  if(length(item_dices)==0){item_dices <-  c(NA,NA,NA)}
  
  output <- data.frame("name" = name, "tier" = tier, "class" = type, 
                       "upgrades" = upgrades, "cost" = cost, 
                       "dice1" = item_dices[1], 
                       "dice2" = item_dices[2],
                       "dice3" = item_dices[3],
                       stringsAsFactors = F)
  
  return(output)
}
retrieve_list_of_weapons <- function(type){
  
  if(type %in% c("Ranged", "Melee")){
  
    list_of_weapons_web <- read_html(paste0("https://imperial-assault.fandom.com/wiki/Category:",type))
    
    list_of_weapons <- list_of_weapons_web %>% 
      html_nodes(".category-page__member-link") %>% 
      html_attrs()
    
    output <- t(simplify2array(list_of_weapons))[,c("href","title")]
    return(output)
  } else {
    stop("Input can only be 'Ranged' or 'Melee'")
  }
  
}

out <- pair_of_dices("Red", "Red", 10000)
weapon_types <- c("Ranged", "Melee")

list_of_weapons <- sapply(weapon_types,retrieve_list_of_weapons)
list_of_weapons_data <- lapply(list_of_weapons, 
                               function(x) apply(x, 1, basic_weapon_information))

basic_info_all_weapons <- bind_rows(lapply(list_of_weapons_data, function(x) bind_rows(x)),.id = "type")





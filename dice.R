rm(list =ls())
library(tidyverse)
library(rvest)

dice <- function(type, amount = 1){
  
  aux <- switch(type, 
                Red		= matrix(c(c(1,0,0),c(2,0,0),c(2,0,0),c(2,1,0),c(3,0,0),c(3,0,0)), byrow = T, ncol = 3),
                Blue	= matrix(c(c(0,1,2),c(1,0,2),c(2,0,3),c(1,1,3),c(2,0,4),c(1,0,5)), byrow = T, ncol = 3),
                Green	= matrix(c(c(0,1,1),c(1,1,1),c(2,0,1),c(1,1,2),c(2,0,2),c(2,0,3)), byrow = T, ncol = 3),
                Yellow	= matrix(c(c(0,1,0),c(1,2,0),c(2,0,1),c(1,1,1),c(0,1,2),c(1,0,2)), byrow = T, ncol = 3)
  )
  
  aux2 <- sample.int(n = nrow(aux), size = amount, replace = T)
  
  output <- data.frame(aux[aux2, ])
  names(output) <- c("dmg", "bolt", "dist")
  
  return(output)
  
}
summarise_dice_results <- function(matrix_of_dice_data){
  if(nrow(matrix_of_dice_data)>1){
    output <- colMeans(matrix_of_dice_data)
  } else {
    output <- matrix_of_dice_data
  }
  return(output)
}
throw <- function(vector_of_dices, throw_amount){
  
  vector_of_dices <- vector_of_dices[!is.na(vector_of_dices)]
  
  d <- lapply(vector_of_dices, dice, amount = throw_amount)
    output <-  Reduce("+", d)
  
  return(output)
}

item <- list_of_weapons$Melee[4,]
weapon_basic_information <- function(item){
  
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
weapon_effect_information <- function(item){
    
  href <- item["href"]
  title <- item["title"]
  
  items_web <- read_html(paste0("https://imperial-assault.fandom.com",href))
  
  
  item_effects <- items_web %>% 
    html_nodes(xpath = "//*[contains(@data-source, 'effect')]") %>% html_children
  
  effect <- item_effects %>% html_text
  
  type <- item_effects %>% html_nodes(css = "a") %>%  html_attr("title")
  
  
  which(!(type %in% "Surge")) - which(type %in% "Surge")[1:length(which(!(type %in% "Surge")))]
  
  amount_type <- effect %>% str_extract("^(.+?)") %>% str_replace(":","1") %>% as.integer
  
  effect <- item_effects %>% html_text
  effect_type <- effect %>% str_extract("([a-zA-Z]+)")
  effect_amount <- effect %>% str_extract("(\\d)") %>% as.integer
  
  extra_effects <- data.frame(type = type, 
                              type_amount = amount_type , 
                              effect = effect_type, 
                              effect_amount = effect_amount)

return(extra_effects)

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

weapon_types <- c("Ranged", "Melee")
list_of_weapons <- sapply(weapon_types,retrieve_list_of_weapons)
list_of_weapons_data <- lapply(list_of_weapons, 
                               function(x) apply(x, 1, weapon_basic_information))

list_of_weapons_effects <- lapply(list_of_weapons, 
                               function(x) apply(x, 1, weapon_effect_information))

basic_info_all_weapons <- bind_rows(lapply(list_of_weapons_data, function(x) bind_rows(x)),.id = "type")

item <- list_of_weapons[[1]][2,]




itemdices <- c(item$dice1, item$dice2, item$dice3)


dice_combinations <- basic_info_all_weapons %>% 
  distinct(dice1,dice2,dice3) %>%
  filter(!is.na(dice1)) 

dices_key <- unite(dice_combinations, key, dice1:dice3, na.rm=T, remove = F)

test <- apply(dice_combinations, 1, throw, throw_amount = 10000)
names(test) <- dices_key$key

output <- dices_key %>% cbind(t(sapply(test, summarise_dice_results)))
output %>% arrange(dmg)


# test.plot <- bind_rows(test, .id = "key") %>% gather(metric, value, -key)
# ggplot(test.plot, aes(colour = metric, x =value)) +
  # geom_density() +
  # facet_wrap(.~key)





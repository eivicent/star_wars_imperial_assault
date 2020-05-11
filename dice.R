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
  
  output <- aux[aux2, ] %>% matrix(nrow = amount) %>% data.frame
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
  
  type <- item_effects %>% html_node(css = "a") %>%  html_attr("title")
  # type_aux <- item_effects %>% html_nodes(css = "a") %>%  html_attr("title")
  
  amount_type <- effect %>% str_extract("^(.+?)") %>% str_replace(":","1") %>% as.integer
  
  effect <- item_effects %>% html_text
  effect_type <- effect %>% str_extract("([a-zA-Z]+)")
  effect_amount <- effect %>% str_extract("(\\d)") %>% as.integer
  
  extra_effects <- data.frame(name = title,
                              active_type = type, 
                              active_amount = amount_type , 
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
free_extra_stats <- function(unitary_throw, free_dmg, free_ac){
    unitary_throw["dmg"] <- unitary_throw["dmg"] + free_dmg
    unitary_throw["dist"] <- unitary_throw["dist"] + free_ac
  return(unitary_throw)
}
extra_dmg <- function(effect_list_weapon){
  aux <- effect_list_weapon %>% filter(active_type == "Damage" |
                                         (is.na(active_type) & effect == "Damage"))
  output <- ifelse(nrow(aux) > 0,sum(aux$effect_amount),0)
  return(output)
}
extra_accuracy <- function(effect_list_weapon){
  aux <- effect_list_weapon %>% filter(active_type == "Accuracy" |
                                         (is.na(active_type) & effect == "Accuracy"))
  output <- ifelse(nrow(aux) > 0,sum(aux$effect_amount),0)
  return(output)
}
surge_to_dmg <- function(unitary_throw, effect_list_weapon){
  
  aux <- effect_list_weapon %>% filter(active_type == "Surge" &
                                         is.na(effect))
  if(nrow(aux) > 0){
    for(jj in 1:nrow(aux)){
      surge_enough <- ifelse(unitary_throw$bolt >= aux$active_amount,T,F)
    
      unitary_throw[surge_enough, "bolt"] <- unitary_throw[surge_enough, "bolt"] - aux$active_amount[jj]
      unitary_throw[surge_enough,"dmg"] <- unitary_throw[surge_enough, "dmg"] + aux$effect_amount[jj]
    }
  }
  return(unitary_throw)
}


############## RETRIEVING WEAPON INFORMATION ##############

weapon_types <- c("Ranged", "Melee")
list_of_weapons <- sapply(weapon_types,retrieve_list_of_weapons)
list_of_weapons_data <- lapply(list_of_weapons, 
                               function(x) apply(x, 1, weapon_basic_information))

list_of_weapons_effects <- lapply(list_of_weapons, 
                               function(x) apply(x, 1, weapon_effect_information))

basic_info_all_weapons <- bind_rows(lapply(list_of_weapons_data, function(x) bind_rows(x)),.id = "type")
effect_info_all_weapons <- bind_rows(lapply(list_of_weapons_effects, function(x) bind_rows(x)),.id = "type") %>%
  mutate(active_amount = ifelse(active_type =="Surge" & 
                                  is.na(active_amount) & is.na(effect),1, 
                                ifelse(active_type == "Surge" & is.na(active_amount),
                                       2,
                                       active_amount)))

effect_info_all_weapons_summary <- effect_info_all_weapons %>% 
  # Filtering extra dmg or accuracy that adds to dices
  filter(!is.na(active_type) & !(active_type %in% c("Damage", "Accuracy"))) %>%
  mutate(active_type = ifelse(active_type == "Surge", 
                              ifelse(is.na(effect), "surge_dmg", 
                                     "surge_extra"), "others")) %>%
  count(name, active_type) %>%
  spread(active_type, n)


############# ANALYZING WEAPONS #############

attack_simulation <- list()
for(ii in 1:nrow(basic_info_all_weapons)){
  
  basic_info_all_weapons[ii,]
  
  effect_list_weapon <- effect <- effect_info_all_weapons %>% filter(name == basic_info_all_weapons$name[ii])

  
  dices <- basic_info_all_weapons[ii,] %>% select(dice1:dice3)
  if(sum(is.na(dices))<3){
    aux <- throw(dices, 100000)
    attack_simulation[[ii]] <- aux %>%
    free_extra_stats(free_dmg = extra_dmg(effect), free_ac = extra_accuracy(effect)) %>%
    surge_to_dmg(effect)
  } else {
    attack_simulation[[ii]] <- matrix(c(0,0,0), nrow=1,
                                      dimnames = list(c(""),c("dmg", "bolt", "dist")))
  }
  
  cat(ii, "/",nrow(basic_info_all_weapons),"\n")
}
names(attack_simulation) <- basic_info_all_weapons$name


attack_tables <- lapply(attack_simulation, ftable)
attack_summary <- lapply(attack_simulation, summarise_dice_results)

ups <- bind_rows(attack_summary, .id = "name")


aux <- list()
for(ii in 1:length(attack_simulation)){
 aux[[ii]] <- summarise_dice_results(attack_simulation[[ii]])
}


attack_simulation[[29]]












##################### DICES ALONE ###################

dice_combinations <- basic_info_all_weapons %>% 
  distinct(dice1,dice2,dice3) %>%
  filter(!is.na(dice1)) 

dices_key <- unite(dice_combinations, key, dice1:dice3, na.rm=T, remove = F)

test <- apply(dice_combinations, 1, throw, throw_amount = 10000)
names(test) <- dices_key$key

test$Blue_Green %>% ftable(col.vars = "dmg", row.vars = c("bolt","dist"))

output <- dices_key %>% cbind(t(sapply(test, summarise_dice_results)))
output %>% arrange(dmg)


# test.plot <- bind_rows(test, .id = "key") %>% gather(metric, value, -key)
# ggplot(test.plot, aes(colour = metric, x =value)) +
  # geom_density() +
  # facet_wrap(.~key)





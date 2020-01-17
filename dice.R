dice <- function(type, amount, detail = F){
  
  aux <- switch(type, 
                red		= matrix(c(c(1,0,0),c(2,0,0),c(2,0,0),c(2,1,0),c(3,0,0),c(3,0,0)), byrow = T, ncol = 3),
                blue	= matrix(c(c(0,1,2),c(1,0,2),c(2,0,3),c(1,1,3),c(2,0,4),c(1,0,5)), byrow = T, ncol = 3),
                green	= matrix(c(c(0,1,1),c(1,1,1),c(2,0,1),c(1,1,2),c(2,0,2),c(2,0,3)), byrow = T, ncol = 3),
                yellow	= matrix(c(c(0,1,0),c(1,2,0),c(2,0,1),c(1,1,1),c(0,1,2),c(1,0,2)), byrow = T, ncol = 3)
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
out <- pair_of_dices("red", "blue", 10000)

library(rvest)

items_web <- read_html("https://imperial-assault.fandom.com/wiki/Item_Card_-_By_Tier/Product")

items_web %>% html_attr()

ups <- items_web %>% 
  html_node(xpath = "//*[(@id = 'WikiaArticle')]/*[(@id = 'mw-content-text')]/ul") 

ups2 <- ups %>% 
  html_nodes(xpath = ".//a") %>% html_attrs()

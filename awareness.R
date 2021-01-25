#Drivers of individual recognition of different activities

source("core.R")
source("chi_graph.R")

prepare_data <- function(df, v1, v2,v1_label, graph_title) {

  joined = paste(v1,v2,sep="_")
  df[[v1]] <- as.character(df[[v1]])
  
  df[[v1]][df[[v1]]=="I know nothing about these kinds of activities"] <- "Know nothing"
  df[[v1]][df[[v1]]=="I know a little about these kinds of activities"] <- "Know a little"
  df[[v1]][df[[v1]]=="I know a lot about these kinds of activities"] <- "Know a lot"
  
  df$phasmajority[df$phasmajority==1] <- "Majority"
  df$phasmajority[df$phasmajority==0] <- "NOC"
  
  df$pmajority[df$pmajority==1] <- "Governing"
  df$pmajority[df$pmajority==0] <- "Opposition"
  
  df$country <- as.character(df$country)
  df$country[df$country=="E"] <- "England"
  df$country[df$country=="S"] <- "Scotland"
  df$country[df$country=="W"] <- "Wales"

  items = v1_label
  col_order <- c("Know nothing",
                 "Know a little",
                 "Know a lot")
  order_by = "Know a lot"
  
  results <- chi_graph(df, v1,v2,items,graph_title,col_order,order_by)
  save_graph(results,joined,200)
  return(results)
}

pb = "Participatory.budgeting.How.familiar.are.you.with.the.following.kinds.of.activities."                                     
ca = "Citizens.assemblies.How.familiar.are.you.with.the.following.kinds.of.activities."                                         
lf = "Local.forums.How.familiar.are.you.with.the.following.kinds.of.activities."                                                
co = "Co.production.of.services.How.familiar.are.you.with.the.following.kinds.of.activities."    

prepare_data(df, pb, "party_reduced", "Party", "Awareness of participatory budgeting by party")
prepare_data(df, pb, "country", "Nation", "Awareness of Participatory budgeting by nation")
prepare_data(df, ca, "party_reduced", "Party", "Awareness of Citizens' Assembly by party")
prepare_data(df, co, "party_reduced", "Party", "Awareness of co-production of services by party")
prepare_data(df, lf, "pmajority", "pmajority", "Awareness of local forums by relation to adminstration")
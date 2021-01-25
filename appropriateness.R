#Check individual types of activity against other variables

source("core.R")
source("chi_graph.R")



prepare_data <- function(df, v1, v2,v1_label, graph_title) {
  joined = paste(v1,v2,sep="_")
  
  v1 = paste(v1,".Which.areas.do.you.consider.citizen.participation.appropriate.for.Â.",sep="")
  df$v1_as_scale <- df[[v1]]
  df$v1_as_scale[df$v1_as_scale=="Not appropriate"]
  items = v1_label
  col_order <- c("Not appropriate",
                 "No opinion",
                 "Appropriate")
  order_by = "Appropriate"
  
  results <- chi_graph(df, v1,v2,items,graph_title,col_order,order_by)
  results$graph
  results$chi
  save_graph(results,joined,200)
}

ph = "Public.Health"
ch = "Cultural.Programmes" 
tr = "Transport"
ed = "Education" 

v2 = "party_reduced"
#v2 = "dexercise"

prepare_data(df, ch, "party_reduced","Party","Exercises related to cultural programmes are...")
prepare_data(df, ed, "party_reduced","Party","Exercises related to education are...")
prepare_data(df, ph, "party_reduced","Party","Exercises related to public health are...")
prepare_data(df, tr, "party_reduced","Party","Exercises related to transport are...")


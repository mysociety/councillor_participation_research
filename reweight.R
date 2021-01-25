## reweight based on criteria

require(MASS)
require(pscl)
require(car)
require(broom)
require(ggplot2)
library(weights)
library(anesrake)

rm(list=ls())

population = read.csv("council_data_final_no_personal.csv",header=TRUE)
population <- population[!(population$gender==""),]
population <- population[!(population$country=="N"),]
population <- population[!(population$party_reduced=="UKIP"),]
population <- population[!(population$council_type=="CC"),]
population$male <- "female"
population$male[population$gender=="male"] <- "male"
df = read.csv("processed_survey.csv",header=TRUE)
df$male <- "female"
df$male[df$gender=="MALE"] <- "male"

df$male <- as.factor(df$male)
population$male <- as.factor(population$male)
population$council_type <- toupper(as.character(population$council_type))
population$council_type <- as.factor(population$council_type)

droplevels(population$party_reduced)
droplevels(population$council_type)


target <- with(population, list(
  male = wpct(male),
  party_reduced = wpct(party_reduced),
  country = wpct(country),
  part_of_majority = wpct(part_of_majority),
  council_has_majority = wpct(council_has_majority),
  council_type = wpct(council_type)
))


raking <- anesrake(target, df, df$Response.ID)

df$weighted <- raking$weightvec
write.csv(df, file = "survey_with_weights.csv",row.names=FALSE)

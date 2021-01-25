
require(MASS)
require(pscl)
require(car)
require(broom)
require(ggplot2)
require(weights)
rm(list=ls())


weighted_t_test <- function(x,y,w){
  ysplit <- split(x, y)
  wsplit <- split(w, y)
  wtd.t.test(ysplit[[1]], ysplit[[2]], wsplit[[1]], wsplit[[2]])
}

df = read.csv("data//survey_with_weights.csv",header=TRUE)

#restrict to completed current uses

colnames(df)[colnames(df)=="Should.participatory.processes.be.ad.hoc..convened.only.for.specific.purpose..or.permanent..a.recurring.process.on.a.policy.area.."] <- "adhoc"
colnames(df)[colnames(df)=="Should.participatory.processes.be.authoritative..citizen.recommendations.should.be.carried.out..or.consultative..results.are.reviewed.by.council.decision.making.processes.."] <- "consultative"
colnames(df)[colnames(df)=="As.a.Councillor..do.you.feel.that.citizen.participation.activities.overlap.with.your.role.as.an.elected.representative."] <- "overlap"
colnames(df)[colnames(df)=="Has.your.Local.Authority..during.your.tenure.as.a.Councillor..ever.conducted.any.participatory.exercises..beyond.typical.consulations.."] <- "done_exercise"
colnames(df)[colnames(df)=="Demographically.balanced.participants.In.general..how.important.are.the.following.in.terms.of.the.legitimacy.or.validity.of.the.process...1.is.low..5.is.high."] <- "balanced"
colnames(df)[colnames(df)=="Number.of.people.participating.In.general..how.important.are.the.following.in.terms.of.the.legitimacy.or.validity.of.the.process...1.is.low..5.is.high."] <- "numbers"
colnames(df)[colnames(df)=="Independent.conveners.In.general..how.important.are.the.following.in.terms.of.the.legitimacy.or.validity.of.the.process...1.is.low..5.is.high."] <- "independent"
colnames(df)[colnames(df)=="Length.of.exercise.In.general..how.important.are.the.following.in.terms.of.the.legitimacy.or.validity.of.the.process...1.is.low..5.is.high."] <- "length"
colnames(df)[colnames(df)=="Transparency.of.process.In.general..how.important.are.the.following.in.terms.of.the.legitimacy.or.validity.of.the.process...1.is.low..5.is.high."] <- "transparency"
colnames(df)[colnames(df)=="Quality.of.discussion.In.general..how.important.are.the.following.in.terms.of.the.legitimacy.or.validity.of.the.process...1.is.low..5.is.high."] <- "quality"
colnames(df)[colnames(df)=="Would.you.be.supportive.of.a.participatory.process.if.initiated.by.the.current.leadership.of.the.council."] <- "current_leadership"
colnames(df)[colnames(df)=="Does.holding.participatory.exercises.balance.representation.problems.in.one.party.councils."] <- "one_party_council"
colnames(df)[colnames(df)=="In.the.event.of.a.citizen.participation.exercise.drawing.conclusions...making.recommendations.that.conflict.with.your.own.views.as.a.Councillor..which.would.you.give.more.weight.to."] <- "conflict"
colnames(df)[colnames(df)=="Should.a.citizen.participation.exercise.be.conducted.by.your.council..would.you.expect.to.discuss.the.results.as.part.of.your.regular.council.meeting."] <- "discuss_meeting" 


df$conflict_perfer_own <- 0
df$conflict_perfer_own[df$conflict=="The view agreed by the citizens participating in the Participatory Democracy exercise"] <- 1

df$adhoc_scale <- 0
df$adhoc_scale[df$adhoc=="Ad hoc"] <- 3
df$adhoc_scale[df$adhoc=="Mixture / Ad hoc leaning"] <- 2
df$adhoc_scale[df$adhoc=="Mixture / Permanent leaning"] <- 1
df$adhoc_scale[df$adhoc=="Permanent"] <- 0

df$consult_scale <- 0
df$consult_scale[df$consultative=="Consultative"] <- 3
df$consult_scale[df$consultative=="Mixture / Consultative Leaning"] <- 2
df$consult_scale[df$consultative=="Mixture / Authoritative Leaning"] <- 1
df$consult_scale[df$consultative=="Authoritative"] <- 0

df$pmajority <- 0
df$pmajority[df$part_of_majority=="TRUE"] <- 1

df$phasmajority <- 0
df$phasmajority[df$council_has_majority=="TRUE"] <- 1


df$dexercise <- NA
df$dexercise[df$done_exercise=="No"] <- 0
df$dexercise[df$done_exercise=="Unsure"] <- 0
df$dexercise[df$done_exercise=="Yes"] <- 1


df$dmeeting <- 0
df$dmeeting[df$discuss_meeting=="Yes"] <- 1

df$cleadership <- 0
df$cleadership[df$current_leadership=="Yes"] <- 1


df$soverlap <- 0
df$soverlap[df$overlap=="Yes"] <- 1


df$one_party_scale <- 0
df$one_party_scale[df$one_party_council=="Yes"] <- 1
df$one_party_scale[df$one_party_council=="No"] <- -1

df$one_party_ok  <- 0
df$one_party_ok[df$one_party_scale==0]  <- 1


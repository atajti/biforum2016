#--------------------------------------------------------------------#
### CHURN projekt ####################################################
# kik fognak lemorzsolódni? ##########################################
#--------------------------------------------------------------------#
# tinyurl.hu/Rhnd

setwd("/media/munka/Új kötet/ANDEGO/161021_BI_Forum_2016/Churn projekt példa")

library(data.table)

demog <- fread("churn_demografia.csv")
termek <- fread("churn_termékek.csv")

str(demog)
str(termek)

osszes_adat <- merge(demog, termek,
                     by="id",
                     all=TRUE)

str(osszes_adat)

osszes_adat[, `:=`(neme = as.integer(neme=="FERFI"),
                   hazas= as.integer(hazas=="IGEN"),
                   auto = as.integer(auto=="IGEN"),
                   churn = as.integer(churn=="IGEN"),
                   megtakaritas = as.integer(megtakaritas=="IGEN"),
                   biztositas = as.integer(biztositas=="IGEN"),
                   jelzalog = as.integer(jelzalog=="IGEN"),
                   regio = as.factor(regio))]
osszes_adat[, termekszam := sum(megtakaritas, biztositas, jelzalog),
             by=seq(nrow(osszes_adat))]


#--------------------------------------------------------------------#
### Adatok vizsgálata ################################################
#--------------------------------------------------------------------#
lapply(osszes_adat[, names(osszes_adat)[-1], with=FALSE],
       table,
       useNA="ifany")

library(caret)
osszes_adat[, `:=`(neme = as.factor(neme),
                   hazas= as.factor(hazas),
                   auto = as.factor(auto),
                   churn = as.factor(churn),
                   megtakaritas = as.factor(megtakaritas),
                   biztositas = as.factor(biztositas),
                   jelzalog = as.factor(jelzalog),
                   regio = as.factor(regio))]

GGally::ggpairs(osszes_adat[, .(kor, jovedelem, termekszam, regio, churn)],
                aes(color=churn))

szurt_adat <- osszes_adat[jovedelem < 100000,]

#--------------------------------------------------------------------#
### Modellezés #######################################################
#--------------------------------------------------------------------#
szurt_adat[, id := NULL]
inTrain <- createDataPartition(szurt_adat$churn,
                               p=.7,
                               list=FALSE)
training <- szurt_adat[inTrain,]
testing <- szurt_adat[-inTrain,]


glm_model <- train(churn ~ .,
                   data=training,
                   method="glm")
summary(glm_model)
glm_res <- predict(glm_model, newdata=testing)
confusionMatrix(data=glm_res, testing$churn)

ctree2_model <- train(churn ~ .,
                   data=training,
                   method="ctree2")
plot(ctree2_model$finalModel)
ctree2_res <- predict(ctree2_model, newdata=testing)
confusionMatrix(data=ctree2_res, testing$churn)






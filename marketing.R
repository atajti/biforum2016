#--------------------------------------------------------------------#
### MARKETING projekt ################################################
# Szegmentáció #######################################################
#--------------------------------------------------------------------#

options(java.parameters="-Xmx1g")
setwd("/media/munka/Új kötet/ANDEGO/161021_BI_Forum_2016/Marketing projekt példa")
source("multiplot.R")

library(ggfortify)
library(data.table)
library(xlsx)

adat <- setDT(read.xlsx2("dataset_bank.xls",
                         1,
                         stringsAsFactors=FALSE))[,
              lapply(.SD, as.integer)]

#--------------------------------------------------------------------#
### adatok vizsgálata ################################################
#--------------------------------------------------------------------#
plot(table(adat$CNT_TBM))
plot(table(adat$CNT_ATM))
plot(table(adat$CNT_POS))
plot(table(adat$CNT_CSC))

lapply(adat, summary)


#--------------------------------------------------------------------#
### adatok feldolgozása ##############################################
#--------------------------------------------------------------------#

log_adatok <- adat[, lapply(.SD, function(x){log(x/sum(x))}),
                   .SDcols=names(adat)[-1]]



#--------------------------------------------------------------------#
### Klaszterek keresése ##############################################
#--------------------------------------------------------------------#
km_cls2 <- kmeans(log_adatok, 2)
km_cls3 <- kmeans(log_adatok, 3)
km_cls4 <- kmeans(log_adatok, 4)
km_cls5 <- kmeans(log_adatok, 5)
autoplot(km_cls3, data=log_adatok, frame=TRUE)
autoplot(km_cls3, data=log_adatok, frame=TRUE)
autoplot(km_cls3, data=log_adatok, frame=TRUE)

log_adatok[, cls := km_cls4$cluster]

tbm_plot <- ggplot(data=log_adatok,
                   aes(color=factor(cls))) +
            geom_density(aes(x=CNT_TBM))

atm_plot <- ggplot(data=log_adatok,
                   aes(color=factor(cls))) +
            geom_density(aes(x=CNT_ATM))

pos_plot <- ggplot(data=log_adatok,
                   aes(color=factor(cls))) +
            geom_density(aes(x=CNT_POS))

csc_plot <- ggplot(data=log_adatok,
                   aes(color=factor(cls))) +
            geom_density(aes(x=CNT_CSC))


multiplot(tbm_plot, atm_plot, pos_plot, csc_plot, cols=2)




png("klaszter_3.png")
autoplot(km_cls3, data=log_adatok, frame=TRUE)
dev.off()
png("klaszter_4.png")
autoplot(km_cls3, data=log_adatok, frame=TRUE)
dev.off()
png("klaszter_5.png")
autoplot(km_cls3, data=log_adatok, frame=TRUE)
dev.off()

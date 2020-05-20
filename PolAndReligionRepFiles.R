#POL and REl Replication Files
# Emily K. Gade 
# 20 May 2020

rm(list=ls())

library(foreign) 
library(wordcloud) 
library(betareg)
library(stargazer) 
library(xtable)
library(ggplot2) 
library(tidyverse) 
library(reshape2) 
library(scales)
library(pglm)

setwd("~/Desktop/Emory/pub_projects/Published/EKGJonJohn_SenateInsecruityPaper/")


selectdata2<-read.csv("polAndReligDataSEnate.csv", stringsAsFactors = F, header = T)

pdata <- pdata.frame(selectdata2, index=c("URLs", "year"))

##########################
#for POL and RELI Paper - final models:
##########################


## Table 1
realWorld_Relig <-betareg(pdata$frequency_religLWIC~ pdata$frequency_religLWIC_lag1 + pdata$femadec + pdata$terroristattack +
                            pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
                          + pdata$upforElection, data = pdata)


realWorld_Anx <-betareg(pdata$frequency_anxiouty~ pdata$frequency_anxiouty_lag1 + pdata$femadec + pdata$terroristattack +
                          pdata$globalterrorism + pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
                        + pdata$upforElection, data = pdata)


relig_allcontrols_axiety<-betareg(pdata$frequency_religLWIC~ pdata$frequency_religLWIC_lag1 + pdata$frequency_anxiouty + pdata$femadec + pdata$terroristattack +
                                    pdata$globalterrorism + pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
                                  + pdata$upforElection, data = pdata)

### print table 1
stargazer(realWorld_Anx, realWorld_Relig, relig_allcontrols_axiety)


########
#Word Clouds for each list 
#####

#load frequency data 
freqs2chapp<-read.csv("freqs_chapp.csv", header = T, stringsAsFactors = F)
freqs2LWIC<-read.csv("freqs_LWIC_rel.csv", header = T, stringsAsFactors = F)
freqs2dhs<-read.csv("freqs_DHS.csv", header = T, stringsAsFactors = F)
freqs2islam<-read.csv("freqs_islam.csv", header = T, stringsAsFactors = F)
freqs2opt<-read.csv("freqs_opt.csv", header = T, stringsAsFactors = F)
freqs2anx<-read.csv("freqs2anx.csv", header = T, stringsAsFactors = F)

### making clouds
#pdf(file="wordClouds_senate_relig_20May.pdf", paper="letter",width = 7,height = 5)
#par(mfrow=c(2,3))


wordcloud(freqs2chapp$word, freqs2chapp$freq, scale=c(4, .25), 
          random.order = FALSE, random.color = FALSE, 
          colors= c("indianred1","indianred2","indianred3","indianred"))

wordcloud(freqs2LWIC$word, freqs2LWIC$freq, scale=c(4, .4), 
          random.order = FALSE, random.color = FALSE, 
          colors= c("lightsteelblue1","lightsteelblue2","lightsteelblue3","lightsteelblue"))

wordcloud(freqs2dhs$word, freqs2dhs$freq, scale=c(4, .3), 
          random.order = FALSE, random.color = FALSE, 
          colors= c("goldenrod","goldenrod1","goldenrod2","goldenrod3"))

wordcloud(freqs2islam$word, freqs2islam$freq, scale=c(4, .5), 
          random.order = FALSE, random.color = FALSE, 
          colors= c("tomato","tomato1","tomato2","tomato3"))

wordcloud(freqs2anx$word, freqs2anx$freq, scale=c(4, .3), 
          random.order = FALSE, random.color = FALSE, 
          colors= c("seagreen1","seagreen2","seagreen3","seagreen4"))

wordcloud(freqs2opt$word, freqs2opt$freq, scale=c(4, .5), 
          random.order = FALSE, random.color = FALSE, 
          colors= c("cadetblue1","cadetblue","cadetblue3","cadetblue"))


#dev.off()

########
# descriptive stats
######

IVDV<-selectdata2
barplots_senator<- aggregate(list(IVDV$unafilated, IVDV$other, IVDV$jew, IVDV$mormon, IVDV$catholic, IVDV$protestantdemonation, IVDV$Republican),
                             by=list(IVDV$URLs), FUN = sum)
names(barplots_senator)<-c("URLS", "Unafiliated", "other", "jewish", "mormon", "catholic", "protestant", "republican")
barplots_senator[barplots_senator>0]<-1

Unaffil<-table(barplots_senator$Unafiliated, barplots_senator$republican)
Catholic<-table(barplots_senator$catholic, barplots_senator$republican)
Jewish<-table(barplots_senator$jewish, barplots_senator$republican)
Other<-table(barplots_senator$other, barplots_senator$republican)
Protestant<-table(barplots_senator$protestant, barplots_senator$republican)
Mormon<-table(barplots_senator$mormon, barplots_senator$republican)

Unaffil<-Unaffil[2,]
Catholic<-Catholic[2,]
Jewish<-Jewish[2,]
Other<-Other[2,]
Mormon<-Mormon[2,]
Protestant<-Protestant[2,]

religtab<-data.frame(rbind(Unaffil, Catholic, Jewish, Other, Mormon, Protestant))
names(religtab)<-c("Dems", "Repub")
religtab$relig <- row.names(religtab)


mdfr <- melt(religtab, id.vars = "relig")
names(mdfr)<-c("Religion", "Party", "Count")

p <- ggplot(mdfr, aes(Religion, Count, fill = Party)) +
  geom_col() 
p + scale_fill_manual(values=c( "blue", "red")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                        panel.background = element_blank(), axis.line = element_line(colour = "black"))
scale_y_continuous(labels = percent)


#####
Vars<- c("% Very Religious (State)", "% Very Conservative (State)", "% Evangelical (State)", 
         "Conservatism (Senator)", "Jewish Faith (Senator)", "Mormon Faith (Senator)", "Female (Senator)", 
         "Up For Election", "Republican (Senator)")
Range<-list( 
            range(IVDV$veryReligous), range(IVDV$veryconservative), 
            range(na.omit(IVDV$evang_state)),
            range(IVDV$dw1), range(IVDV$jew), range(IVDV$mormon), 
            range(IVDV$Female), range(IVDV$upforElection), range(IVDV$Republican))
Mean <- c(
          mean(IVDV$veryReligous), mean(IVDV$veryconservative), 
          mean(na.omit(IVDV$evang_state)), 
          mean(IVDV$dw1), mean(IVDV$jew), mean(IVDV$mormon), 
          mean(IVDV$Female), mean(IVDV$upforElection), mean(IVDV$Republican))
SD<-  c(
        sd(IVDV$veryReligous), sd(IVDV$veryconservative), sd(na.omit(IVDV$evang_state)), 
        sd(IVDV$dw1), sd(IVDV$jew), sd(IVDV$mormon), sd(IVDV$Female), sd(IVDV$upforElection), sd(IVDV$Republican))

a<-unlist(lapply(Range, `[[`, 1))
b<-unlist(lapply(Range, `[[`, 2))
descr_stats<-data.frame(Vars, a, b,  Mean, SD)
xtable(descr_stats, digits = 2)



#################
# Robustness Checks
#################

###### 
#Bivariate relationships
########
biv_anx <-betareg(pdata$frequency_religLWIC~pdata$frequency_anxiouty)

biv_dhs <-betareg(pdata$frequency_religLWIC~pdata$frequency_dhs)

stargazer(biv_anx, biv_dhs)

######
# with Chapp Words
####


relig_allcontrols_Chapp<-betareg(pdata$frequency_chapp ~ pdata$frequency_religLWIC_lag1 + pdata$frequency_anxiouty + pdata$femadec + pdata$terroristattack +
                                    pdata$globalterrorism +pdata$frequency_dhs + pdata$frequency_opt + pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
                                  + pdata$upforElection, data = pdata)

stargazer(relig_allcontrols_Chapp)


## no islam 


relig_allcontrols_NoIslam<-betareg(pdata$frequency_reli_Lwicnoislam~ pdata$frequency_religLWIC_lag1 + pdata$frequency_anxiouty + pdata$femadec + pdata$terroristattack +
                                     pdata$globalterrorism + pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
                                   + pdata$upforElection, data = pdata)

stargazer(relig_allcontrols_NoIslam)


### with other types of text measures

relig_alltext<-betareg(pdata$frequency_religLWIC~ pdata$frequency_religLWIC_lag1 + pdata$frequency_anxiouty + pdata$frequency_dhs + pdata$frequency_opt + pdata$femadec + pdata$terroristattack +
                                     pdata$globalterrorism + pdata$veryReligous +  pdata$dw1 + pdata$Female + pdata$veryconservative + pdata$veryReligous
                                   + pdata$upforElection, data = pdata)

stargazer(relig_alltext)



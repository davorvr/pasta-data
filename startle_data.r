# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Virag D, Homolak J, Kodvanj I, Babic Perhoc A, Knezovic A, Osmanovic Barilar J, Salkovic-Petrisic M                 #
# Repurposing a digital kitchen scale for neuroscience research: a complete hardware and software cookbook for PASTA  #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


library(ratPASTA)
library(ggplot2)
library(dplyr)
library(devEMF)



### Load either raw data or processed data <<<<<<<<
## Loading raw data
# df <- loadStartleData(addhead = 0.6,addtail = 0.5)
## Loading processed data
# load("data.RData") # df, pl1, pl2, pl3, pl3s, pl4



summary <- summariseStartle(df)
write.csv(summary, "summary.csv")


pl1 <- startlePlot(df, type = 1)
pl2 <- startlePlot(df, type = 2)
pl3 <- startlePlot(df, type = 3) 

pl3s <- pl3 + 
  ggpubr::stat_compare_means(method = "wilcox.test", comparisons = list(c("ctr W/O PI", "stz W/O PI"),c("ctr W PI", "stz W PI"))) + 
  ylab("Value [g]") + theme(axis.title = element_text(size = 9), 
        axis.text = element_text(size = 8, colour = "black"))

emf("pl3s.emf", height = 4.5, width = 4.5)
print(pl3s)
dev.off()



pl4 <- df %>%
  filter(group == "stz 15") %>% mutate(time2 = lubridate::seconds(time2 / 1000)) %>%
  ggplot(aes(x = time2, y = value)) + geom_line() + 
  facet_wrap(~ order) + theme_bw() + ylab("Value [g]") + xlab("Time [seconds]")
  

emf("pl4.emf", height = 6, width = 6)
print(pl4)
dev.off()





load("//donnees.dreal-norm.ad.e2.rie.gouv.fr/services/SMCAP/05_BOeS/5PREL/base_donnees/BDNB_OPEN_SOURCE/bdnb/data/df_temps.RData")
library(units)
library(FactoMineR)
par(mfrow=c(2,2))
plot(results_time$nb_lignes_graphe,results_time$superficie)
plot(results_time$time_diff,results_time$nb_lignes_graphe)
plot(results_time$time_diff,results_time$nb_lignes_graphe_c)
plot(results_time$time_diff,results_time$superficie)

reg=lm(results_time$time_diff~results_time$nb_lignes_graphe_c)
reg2=lm(results_time$time_diff~results_time$superficie)
reg3=lm(results_time$nb_lignes_graphe~results_time$superficie)
summary(reg3)

res_acp=PCA(results_time[-26,-1],scale.unit = TRUE)
summary(res_acp)
plot(res_acp,choix="ind")




g=ggplot2::ggplot(results_time,aes(x=nb_lignes_graphe,y=superficie,text=code))+ggplot2::geom_point()
plotly::ggplotly(g)


par(mfrow=c(1,1))
summary(epci_geo_plus$superficie_buffer)
summary(results_time$time_diff)
boxplot(epci_geo_plus$superficie_buffer)
results_time$time_diff <- as.numeric(as.character(results_time$time_diff))

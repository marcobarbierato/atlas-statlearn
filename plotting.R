par(mar = c(2, 2, 2, 2))
par(mfrow=c(6,5))
for (i in names(no.label)) {
  if (i=='PRI_jet_num') next
  hist(unlist( subset(no.label, select = c(i)), use.names=FALSE ), main=i, xlab='')
}
barplot(prop.table(table(s.train$PRI_jet_num)), main='PRI_jet_num')



data.sig = no.label.drop[s.train$Label=='s',]$DER_mass_MMC
data.back = no.label.drop[s.train$Label=='b',]$DER_mass_MMC
ddd=density(data.sig)
vvv=density(data.back)

plot(ddd$x, ddd$y,type="l",col='red', main='DER_mass_MMC', xlab='Values', ylab='Density', xlim=c(-2, 5))
#par(new=TRUE)
lines(vvv$x, vvv$y,type="l",col='black')
legend(x = "topright",          # Position
       legend = c("Signal", "Background"),  
       col = c('red', 'black'),           
       lwd = 2)     

lines(c(mean(data.sig), mean(data.sig)),c(0,.1), col="red")
lines(c(mean(data.back), mean(data.back)),c(0,.1), col="black")


data.sig = no.label.drop[s.train$Label=='s',]$DER_deltaeta_jet_jet
data.back = no.label.drop[s.train$Label=='b',]$DER_deltaeta_jet_jet

ddd=density(data.sig)
vvv=density(data.back)

plot(vvv$x, vvv$y,type="l",col='black', main='DER_deltaeta_jet_jet', xlab='Values', ylab='Density')
#par(new=TRUE)
lines(ddd$x, ddd$y,type="l",col='red')
legend(x = "topright",          # Position
       legend = c("Signal", "Background"),  
       col = c('red', 'black'),           
       lwd = 2)     

lines(c(mean(data.sig), mean(data.sig)),c(0,.1), col="red")
lines(c(mean(data.back), mean(data.back)),c(0,.1), col="black")



data.sig = no.label.drop[s.train$Label=='s',]$DER_lep_eta_centrality
data.back = no.label.drop[s.train$Label=='b',]$DER_lep_eta_centrality

ddd=density(data.sig)
vvv=density(data.back)

plot(vvv$x, vvv$y,type="l",col='black', main='DER_lep_eta_centrality', xlab='Values', ylab='Density')
#par(new=TRUE)
lines(ddd$x, ddd$y,type="l",col='red')
legend(x = "topright",          # Position
       legend = c("Signal", "Background"),  
       col = c('red', 'black'),           
       lwd = 2)     

lines(c(mean(data.sig), mean(data.sig)),c(0,.1), col="red")
lines(c(mean(data.back), mean(data.back)),c(0,.1), col="black")



data.sig = no.label.drop[s.train$Label=='s',]$PRI_tau_eta
data.back = no.label.drop[s.train$Label=='b',]$PRI_tau_eta

ddd=density(data.sig)
vvv=density(data.back)

plot(ddd$x, ddd$y,type="l",col='black', main='PRI_tau_eta', xlab='Values', ylab='Density')
#par(new=TRUE)
lines(vvv$x, vvv$y,type="l",col='red')
legend(x = "topright",          # Position
       legend = c("Signal", "Background"),  
       col = c('red', 'black'),           
       lwd = 2)     

lines(c(mean(data.sig), mean(data.sig)),c(0,.1), col="red")
lines(c(mean(data.back), mean(data.back)),c(0,.1), col="black")


data.sig = no.label.drop[s.train$Label=='s',]$PRI_tau_phi
data.back = no.label.drop[s.train$Label=='b',]$PRI_tau_phi

ddd=density(data.sig)
vvv=density(data.back)

plot(ddd$x, ddd$y,type="l",col='black', main='PRI_tau_phi', xlab='Values', ylab='Density')
#par(new=TRUE)
lines(vvv$x, vvv$y,type="l",col='red')
legend(x = "topright",          # Position
       legend = c("Signal", "Background"),  
       col = c('red', 'black'),           
       lwd = 2)     

lines(c(mean(data.sig), mean(data.sig)),c(0,.1), col="red")
lines(c(mean(data.back), mean(data.back)),c(0,.1), col="black")

par(mfrow=c(1,1))
data.sig = no.label.drop[s.train$Label=='s',]$PRI_jet_leading_eta
data.back = no.label.drop[s.train$Label=='b',]$PRI_jet_leading_eta

ddd=density(data.sig)
vvv=density(data.back)

plot(vvv$x, vvv$y,type="l",col='black', main='PRI_jet_leading_eta', xlab='Values', ylab='Density')
#par(new=TRUE)
lines(ddd$x, ddd$y,type="l",col='red')
legend(x = "topright",          # Position
       legend = c("Signal", "Background"),  
       col = c('red', 'black'),           
       lwd = 2)     

lines(c(mean(data.sig), mean(data.sig)),c(0,.1), col="red")
lines(c(mean(data.back), mean(data.back)),c(0,.1), col="black")


par(mfrow=c(2,2))

plot(no.label$DER_sum_pt, no.label$DER_PRI_met_sumet, col=s.train$Label, xlab='DER_sum_pt', ylab='DER_PRI_met_sumet')
legend(x = "topright",          # Position
       legend = c("Signal", "Background"),  
       col = c('red', 'black'),           
       lwd = 1    )

       
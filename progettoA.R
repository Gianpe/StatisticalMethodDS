###########
# AIDA data loading
###########

# clean all
rm(list=ls())

# working dir

# to reload the whole dataset at once, run
load(file="aida.RData")
View(aida)


# conteggio dei missing value
summary(aida) #poco leggibile
missing <- sapply(aida, function(x) sum(is.na(x)))
sort(missing)


aida = aida[!is.na(aida$`ATECO 2007code`),]
aida = aida[!is.na(aida$`Number of employeesLast avail. yr`),]
aida = aida[!is.na(aida$`Legal form`),]
aida = aida[!is.na(aida$`Incorporation year`),]

aida = aida[!aida$`Incorporation year`>aida$`Last accounting closing date`,]


# summaries
table(aida$`Registered office address - Region`)
table(aida$`Legal status`)
table(aida$`Legal form`)


aida$"Legal status" = as.character(aida$"Legal status")
aida$"Legal status"[aida$"Legal status"=="Bankruptcy"] = "Failed"
aida$"Legal status"[aida$"Legal status"=="Dissolved"] = "Failed"
aida$"Legal status"[aida$"Legal status"=="Dissolved (bankruptcy)"] = "Failed"
aida$"Legal status"[aida$"Legal status"=="Dissolved (demerger)"] = "Failed"
aida$"Legal status"[aida$"Legal status"=="Dissolved (liquidation)"] = "Failed"
aida$"Legal status"[aida$"Legal status"=="Dissolved (merger)"] = "Failed"
aida$"Legal status"[aida$"Legal status"=="In liquidation"] = "Failed"
aida$"Legal status"[aida$"Legal status"=="Active (default of payments)"] = "Failed"
aida$"Legal status"[aida$"Legal status"=="Active (receivership)"] = "Failed"
table(aida$"Legal status")
View(aida)

aida$"Size"[aida$"Number of employeesLast avail. yr"<=10] = "Micro"
aida$"Size"[aida$"Number of employeesLast avail. yr">10 & aida$"Number of employeesLast avail. yr"<=50 ] = "Small"
aida$"Size"[aida$"Number of employeesLast avail. yr">50] = "Medium"
table(aida$Size)
View(aida)
table(aida$`Last accounting closing date`)


aida$"Age"[aida$"Legal status"== "Active"]= 2019- aida$"Incorporation year"
aida$"Age"[aida$"Legal status"== "Failed"]= aida$"Last accounting closing date"- aida$"Incorporation year"

table(aida$Age)




active= subset(aida ,subset=( aida$"Legal status"=="Active")) 
failed= subset(aida ,subset=( aida$"Legal status"!="Active"))

plot(density(active$Age), main="EtÃ  aziende", xlab="EtÃ ", ylab="N di aziende") 
lines(density((failed$Age)), col="red") 
legend(78, 0.057, legend= c("Active", "Failed"), col= c("black", "red"), lty=1, cex=0.8 )
hist(na.omit(active$Age), main="EtÃ  aziende", xlab="EtÃ ", ylab="N di aziende")
hist(na.omit(failed$Age), main="EtÃ  aziende", xlab="EtÃ ", ylab="N di aziende")

bins = sort(unique(c(active$Age, failed$Age)))
N1i = sapply(bins, function(i) sum(active$Age ==i))
N2i = sapply(bins, function(i) sum(failed$Age==i))
mat = cbind(N1i, N2i)
View(mat)
chisq.test(mat, correct=FALSE)

table(aida$`Legal form`)

plot(density(na.omit(active$Age[which(aida$`Legal form`== 'S.P.A.')])), main="Et? aziende S.P.A.", xlab="Et?", ylab="N di aziende") 
lines(density(na.omit(failed$Age[which(aida$`Legal form`== 'S.P.A.')])), col="red") 
legend(78, 0.057, legend= c("Active", "Failed"), col= c("black", "red"), lty=1, cex=0.8 )

aida$"Size"[aida$"Number of employeesLast avail. yr"<=10] = "Micro"
aida$"Size"[aida$"Number of employeesLast avail. yr">10 & aida$"Number of employeesLast avail. yr"<=50 ] = "Small"
aida$"Size"[aida$"Number of employeesLast avail. yr">50] = "Medium"
table(aida$Size)

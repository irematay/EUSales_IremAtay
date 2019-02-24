# Get the working directory. If needed, you can set the working directory to another folder.
getwd()

# Read the Data files from a directory  
EUSalesData<-read.csv("EuropeanSales.csv",header=T)

attributes(EUSalesData)

#drop the country column
EUSales<-EUSalesData[2:7]
attributes(EUSales)

#draw and look the EUSales data
plot(EUSales)

head(EUSales)
tail(EUSales)
summary(EUSales)

#Correlation for all attributes
corEUSales<-cor(EUSales[,])
corEUSales

library(corrplot)
corrplot(corEUSales)

#Modeling for SalesPerCapita 
m1_salespercapita<-lm(SalesPerCapita ~ Population + GDPperHead + UnemploymentRate + EducationSpending + ComputerSales, data = EUSales)
m2_salespercapita<-lm(SalesPerCapita ~ Population + GDPperHead + UnemploymentRate + EducationSpending , data = EUSales)
m3_salespercapita<-lm(SalesPerCapita ~ Population + GDPperHead + EducationSpending , data = EUSales)
m4_salespercapita<-lm(SalesPerCapita ~ GDPperHead + EducationSpending , data = EUSales)
m5_salespercapita<-lm(SalesPerCapita ~ GDPperHead , data = EUSales)

#Summary for SalesPerCapita Models
summary(m1_salespercapita)
summary(m2_salespercapita)
summary(m3_salespercapita)
summary(m4_salespercapita)
summary(m5_salespercapita)

#Modeling for ComputerSales 
m1_computersales<-lm(ComputerSales ~ SalesPerCapita + Population + GDPperHead + UnemploymentRate + EducationSpending , data = EUSales)
m2_computersales<-lm(ComputerSales ~ SalesPerCapita + Population + GDPperHead + UnemploymentRate , data = EUSales)
m3_computersales<-lm(ComputerSales ~ Population + GDPperHead + UnemploymentRate , data = EUSales)
m4_computersales<-lm(ComputerSales ~ Population + GDPperHead , data = EUSales)
m5_computersales<-lm(ComputerSales ~ Population , data = EUSales)

#Summary for ComputerSales Models
summary(m1_computersales)
summary(m2_computersales)
summary(m3_computersales)
summary(m4_computersales)
summary(m5_computersales)

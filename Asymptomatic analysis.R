
      #####Master file for the analysis of asymptomatic cases#####

#load the libraries
library(meta)
library(readxl)
      
      
##### Prepare the data #####
      
#Get the data
asymptomatic <- read_excel("Z519COVID19Asymptoma_DATA_2020-04-02_1428.xlsx")
#select data from reviews
asymptomatic <-asymptomatic[asymptomatic$review==1,]
#use the names from some variables that see important to me
asymptomatic$setting<-as.factor(asymptomatic$setting)
asymptomatic$age<-as.factor(asymptomatic$age)
asymptomatic$design<-as.factor(asymptomatic$design)
levels(asymptomatic$setting)<-c("Ship","Hospital", "Traveler affected area", "Traveler other", 
                                "Evacuation",
                                "Contact tracing",
                                "Healthcare worker",
                                "Surveillance data")


levels(asymptomatic$age)<-c("Pediatric","Adults","Elderly","Family","Not specified","Other")

levels(asymptomatic$design)<-c("Case study",
                               "Cross-sectional",
                               "Cohort study" ,
                               "Modelling",
                               "Other")


##### Run the analysis #####

pdf("Meta-analyses of proportions using the GLMM method.pdf", paper="USr",width=12,
    pointsize=8)

### Absolute number of asymptomatic positive cases/Absolute number of positive cases (asym_nom/asym_denom)
asym<-metaprop(asym_nom,asym_denom,data=asymptomatic[!is.na(asymptomatic$asym_nom),],studlab=record_no,sm = "PLOGIT" )
forest(asym, sortvar = asym_denom, leftcols = c("record_id", "setting", "age", "design", "asym_denom"),xlab = "P(asymptomatic)")
#forest(update(asym, subset=asym_denom>20), sortvar = TE, leftcols = c( "record_id", "setting", "age", "design", "asym_denom"))#just a subset of the largest studies

### Absolute number of asymptomatic positive cases at the time of testing/Absolute number of positive cases (asym_nom_2/asym_denom_2)
asymattest<-metaprop(asym_nom_2,asym_denom_2,data=asymptomatic[!is.na(asymptomatic$asym_nom_2),],studlab=record_no,sm = "PLOGIT" )
forest(asymattest, sortvar = asym_denom_2, leftcols = c("record_id", "setting", "age", "design", "asym_denom_2"),xlab = "P(asymptomatic at test)")
#forest(update(asymattest, subset=asym_denom_2>20), sortvar = TE, leftcols = c( "record_id", "setting", "age", "design", "asym_denom_2"))#just a subset of the largest studies

###Absolute number who develop symptoms after testing/Absolute number of positive cases (dev_syp_nom/asym_denom_2)
asymaftertest<-metaprop(dev_syp_nom, asym_denom_2,data=asymptomatic[!is.na(asymptomatic$dev_syp_nom),],studlab=record_no,sm = "PLOGIT" )
forest(asymaftertest, sortvar = asym_denom_2, leftcols = c("record_id", "setting", "age", "design",  "asym_denom_2"),xlab = "P(Symptoms only after test)")
#forest(update(asymaftertest, subset=asym_denom_2>20))

dev.off()

rm(list=ls())



       
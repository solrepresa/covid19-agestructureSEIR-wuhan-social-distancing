require(readxl)

### Preprocessing Argentina Data ###

site = "ProvBsAS" 


## 2) Make a replace for contacts.rdata file

#load data
contact_home <- read.csv("data/Argentina_home.txt", dec=",", sep = "\t", check.names = FALSE)
contact_work <- read.csv("data/Argentina_work.txt", dec=",", sep = "\t")
contact_school <- read.csv("data/Argentina_schools.txt", dec=",", sep = "\t")
contact_other <- read.csv("data/Argentina_other_locations.txt", dec=",", sep = "\t")
contact_all <- read.csv("data/Argentina_all_locations.txt", dec=",", sep = "\t")

#transform table to matrix
contact_home <- matrix(as.numeric(unlist(contact_home)), nrow = 16, ncol = 16)
contact_work <- matrix(as.numeric(unlist(contact_work)), nrow = 16, ncol = 16)
contact_school <- matrix(as.numeric(unlist(contact_school)), nrow = 16, ncol = 16)
contact_other <- matrix(as.numeric(unlist(contact_other)), nrow = 16, ncol = 16)
contact_all <- matrix(as.numeric(unlist(contact_all)), nrow = 16, ncol = 16)


contacts_arg <- list( home = contact_home, 
                      work = contact_work, 
                      school = contact_school, 
                      others = contact_other, 
                      all = contact_all)

rm(contact_home, contact_work, contact_school, contact_other, contact_all)




# 3) load Argentina - Case age distribution (02/05/2010)

ageDistribution <- read_excel("data/Arg-Pop-Case-Dist.xls")
ageDistribution <- data.frame(ageDistribution)
ageDistribution <- ageDistribution[-c(3,4,5)]
ageDistribution$national <- ageDistribution$ProvBsAS
ageDistribution <- ageDistribution[c(1,2,4,3)]
ageDistribution <- ageDistribution[complete.cases(ageDistribution),]



# 4) load R0 Argentina

R0_arg <- read_excel("data/DatosEpidemiologicosArgentina.xls", sheet=2, skip = 2)
R0_arg <- data.frame(R0_arg)
R0_plot <- R0_arg[,2:ncol(R0_arg)]
R0_dates <- R0_arg[,1]

rm(R0_arg)

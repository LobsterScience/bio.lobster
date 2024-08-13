options(stringsAsFactors=F)
require(bio.lobster)
require(lubridate)``
require(ggplot2)
require(dplyr)
require(statmod)

setwd("C:/Users/HowseVJ/OneDrive - DFO-MPO/Maturity Writing/DataFiles")


##########------------------ SILVA Data Input ------------------##########
a = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','LobsterMaturityDatabase.csv'))
#########-----------------------------------------------##########

a$Y = a$Lat
a$X = a$Long*-1
a$EID = 1:nrow(a)
a$Date = as.Date(a$Date,"%d/%m/%Y")
a$mon = month(a$Date)
a$year = year(a$Date)

a=subset(a, Sex == 2) #Remove Berried Females
a=subset(a, Cemgland_stage != 'n/a') 
a=subset(a, mon == 5 | mon == 6)#Filter out July and August

#Pleopod Staging
a$Pleopod_mat <-ifelse(a$Cemgland_stage<2,0,1)

## Add LFAs
a$LFA <- ""
a$LFA <-ifelse(a$Location == "Lobster Bay", 34, a$LFA)
a$LFA <-ifelse(a$Location == "Harrigan Cove" | a$Location == "Tangier"| a$Location == "Mushaboom", 32, a$LFA)
a$LFA <-ifelse(a$Location == "Port Mouton", 33,a$LFA)
a$LFA <-ifelse(a$Location == "Canso", "31A", a$LFA)
#a = subset(a,Carapace_mm <121)

 
#Pull out Lobster that are mature
pastMat<-a[a$Pleopod_mat ==1,]
pastMat<-pastMat %>% dplyr::select(LFA, year, Carapace_mm)
pastMat$Datasource<-"DFO-Silva"

pastMat$som50<-ifelse(pastMat$LFA == "31A",77.5,
                      ifelse(pastMat$LFA == 32,92.2, "NA"))
pastMat = subset(pastMat,LFA<33)

##########------------------ DFO Data Input ------------------##########
b = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','matClean.csv'))
##########-----------------------------------------------##########
newMat<-b[b$Pleopod_mat==1,] ##pull out lobster that are mature
#newMat = subset(newMat,Carapace_mm <121)
newMat<-newMat %>% dplyr::select(LFA, year, Carapace_mm)
newMat$Datasource<-"DFO"


newMat$som50<-ifelse(newMat$LFA == 33,90.3,
                     ifelse(newMat$LFA == 35,83.3,
                      ifelse(newMat$LFA == 36,88.8,
                                    ifelse(newMat$LFA ==38, 88.5, "NA"))))

##########------------------ Data Input ------------------##########
c = read.csv(file.path(project.datadirectory('bio.lobster'),'data','Maturity','Maturity_GCIFA.csv'))
##########-----------------------------------------------##########


#### Clean and convert ####
c$Date = as.Date(c$Date,"%d-%b-%y")
c$mon = month(c$Date)
c$year = year(c$Date)
c$EID = 1:nrow(c)

convert_to_decimal_degrees <- function(dmm) {
  dmm <- trimws(dmm)  # Trim whitespace
  parts <- strsplit(dmm, " ")[[1]]  # Split by space
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  decimal_degrees <- degrees + (minutes / 60)
  return(decimal_degrees)
}

c$Latitude_DD <- sapply(c$Latitude, convert_to_decimal_degrees)
c$Longitude_DD <- sapply(c$Longitude, convert_to_decimal_degrees)

c <- c %>%
  rename(Y = Latitude_DD, X = Longitude_DD)
c$X = c$X*-1

GCMat=c
GCMat<-GCMat[GCMat$Pleopod_mat==1,] ## pull out lobster that are mature
#GCMat = subset(GCMat,Carapace_mm <121)
GCIFAMat<-GCMat %>% dplyr::select(LFA, year, Carapace_mm)
GCIFAMat$Datasource<-"GCIFA"


GCIFAMat$som50<-ifelse(GCIFAMat$LFA == "31A",74.4,
                            ifelse(GCIFAMat$LFA =="31B", 77.6, "NA"))




Matresult<-rbind(pastMat, newMat, GCIFAMat)

Matresult$som50<- as.numeric(Matresult$som50)
Matresult$Carapace_mm<-round(Matresult$Carapace_mm)

Matresult$LFA<-as.factor(Matresult$LFA)



matsum<- Matresult%>%
  group_by(LFA)%>%
mutate(centre =som50,lower=min(Carapace_mm), upper=max(Carapace_mm))
matsum<-as.data.frame(matsum)

matsum <- matsum %>%
  mutate(Datasource = as.factor(Datasource))



# Adjust the LFA values
matsum <- matsum %>%
  mutate(LFA_adjusted = case_when(
    LFA == "31A" & Datasource == "DFO-Silva" ~ as.numeric(as.factor(LFA)) - 0.1,
    LFA == "31A" & Datasource == "GCIFA" ~ as.numeric(as.factor(LFA)) + 0.1,
    TRUE ~ as.numeric(as.factor(LFA))
  ))


# Print column names and sample of the dataframe to verify
print(colnames(matsum))
print(head(matsum))

# Ensure that the LFA and LFA_adjusted columns exist
if (!all(c("LFA", "LFA_adjusted") %in% colnames(matsum))) {
  stop("Columns LFA and LFA_adjusted are not present in the dataframe.")
}


##############NOT WORKING ##################

# Create a lookup table for LFA_adjusted and LFA
lookup_table <- matsum %>%
  select(LFA, LFA_adjusted) %>%
  distinct() %>%
  arrange(LFA_adjusted)

# Plot with adjusted LFA values
p <- ggplot(matsum, aes(x = LFA_adjusted, y = Carapace_mm, fill = as.factor(Datasource))) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.2) +
  scale_x_continuous(breaks = lookup_table$LFA_adjusted,
                     labels = lookup_table$LFA) +
  labs(x = "LFA", y = "Carapace (mm)", fill = "Datasource")

print(p)








### THIS DATA ONLY INCLUDE MATURE FEMALES
p<-ggplot(matsum, aes(x=LFA, y = Carapace_mm, fill = as.factor(Datasource)))+
  geom_jitter(width = 0.2, size = 1, alpha = 0.2)+
  
  
  p<-p+geom_boxplot(aes(x=LFA,lower=lower, upper=upper, middle = som50, ymin=lower, ymax=upper),
                    stat="identity",alpha =0.5)+
  labs(x="LFA", "Carapace Length (mm)")+
  scale_fill_brewer(palette="Set2")+
  scale_y_continuous(breaks=(seq(min(Matresult$Carapace_mm), max(Matresult$Carapace_mm), by =5)))





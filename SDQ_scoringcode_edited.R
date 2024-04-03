
library(dplyr)
library(tidyr)
install.packages("car")
library(car)
SDQdata <- read.csv("SDQ_DATA_2024-03-28_1509.csv")
colnames(SDQdata)
SDQ_FullData_2.7.24<- SDQdata%>%replace(is.na(.), 0)


require(car) # Required for the "recode" function


# Recoding variables and then scoring the parent SDQ scores

qobeys <- recode (SDQdata$sdq_parent7, "0=2; 1=1; 2=0; else=NA")
qreflect <- recode (SDQdata$sdq_parent11, "0=2; 1=1; 2=0; else=NA")
qattends <- recode (SDQdata$sdq_parent14, "0=2; 1=1; 2=0; else=NA")
qfriend <- recode (SDQdata$sdq_parent21, "0=2; 1=1; 2=0; else=NA")
qpopular <- recode (SDQdata$sdq_parent25, "0=2; 1=1; 2=0; else=NA")

qqdistres <- recode (SDQdata$sdq_parent28, "0:1=0; 2=1; 3=2; NA=0")
qqimphome <- recode (SDQdata$sdq_parent29a, "0:1=0; 2=1; 3=2; NA=0")
qqimpfrie <- recode (SDQdata$sdq_parent29b, "0:1=0; 2=1; 3=2; NA=0")
qqimpclas <- recode (SDQdata$sdq_parent29c, "0:1=0; 2=1; 3=2; NA=0")
qqimpleis <- recode (SDQdata$sdq_parent29d, "0:1=0; 2=1; 3=2; NA=0")

df.pemotion <- data.frame(SDQdata$sdq_parent3, SDQdata$sdq_parent8, SDQdata$sdq_parent13, SDQdata$sdq_parent16, SDQdata$sdq_parent24)
pnemotion <- apply(df.pemotion, 1, function(x) sum(is.na(x)))
pemotion <- ifelse(pnemotion<3, rowMeans(df.pemotion, na.rm=TRUE), NA)
pemotion <- as.numeric(pemotion) * 5
pemotion <- floor(0.5 + pemotion)

df.pconduct <- data.frame(SDQdata$sdq_parent5, SDQdata$sdq_parent7, SDQdata$sdq_parent12, SDQdata$sdq_parent18, SDQdata$sdq_parent22)
pnconduct <- apply(df.pconduct, 1, function(x) sum(is.na(x)))
pconduct <- ifelse(pnconduct<3, rowMeans(df.pconduct, na.rm=TRUE), NA)
pconduct <- as.numeric(pconduct) * 5
pconduct <- floor(0.5 + pconduct)

df.phyper <- data.frame(SDQdata$sdq_parent2, SDQdata$sdq_parent10, SDQdata$sdq_parent15, SDQdata$sdq_parent21, SDQdata$sdq_parent25)
pnhyper <- apply(df.phyper, 1, function(x) sum(is.na(x)))
phyper <- ifelse(pnhyper<3, rowMeans(df.phyper, na.rm=TRUE), NA)
phyper <- as.numeric(phyper) * 5
phyper <- floor(0.5 + phyper)

df.ppeer <- data.frame(SDQdata$sdq_parent6, SDQdata$sdq_parent11, SDQdata$sdq_parent14, SDQdata$sdq_parent19, SDQdata$sdq_parent23)
pnpeer <- apply(df.ppeer, 1, function(x) sum(is.na(x)))
ppeer <- ifelse(pnpeer<3, rowMeans(df.ppeer, na.rm=TRUE), NA)
ppeer <- as.numeric(ppeer) * 5
ppeer <- floor(0.5 + ppeer)

df.pprosoc <- data.frame(SDQdata$sdq_parent1, SDQdata$sdq_parent4, SDQdata$sdq_parent9, SDQdata$sdq_parent17, SDQdata$sdq_parent20)
pnprosoc <- apply(df.pprosoc, 1, function(x) sum(is.na(x)))
pprosoc <- ifelse(pnprosoc<3, rowMeans(df.pprosoc, na.rm=TRUE), NA)
pprosoc <- as.numeric(pprosoc) * 5
pprosoc <- floor(0.5 + pprosoc)

df.pimpact <- data.frame(SDQdata$sdq_parent28, SDQdata$sdq_parent29a, SDQdata$sdq_parent29b, SDQdata$sdq_parent29c, SDQdata$sdq_parent29d)
pnimpact <- apply(df.pimpact, 1, function(x) sum(is.na(x)))
pimpact <- ifelse(!pnimpact==5, SDQdata$sdq_parent28+SDQdata$sdq_parent29a+SDQdata$sdq_parent29b+SDQdata$sdq_parent29c+SDQdata$sdq_parent29d, NA)
pimpact <- ifelse(SDQdata$sdq_parent26 ==0, 0, pimpact)
pimpact <- as.numeric(pimpact)
pebdtot <- pemotion+pconduct+phyper+ppeer

rm(qobeys, qreflect, qattends, qfriend, qpopular, qqdistres, qqimphome, qqimpfrie, qqimpclas, qqimpleis, pnemotion, pnconduct, pnhyper, pnpeer, pnprosoc, pnimpact, df.pemotion, df.pconduct, df.phyper, df.ppeer, df.pprosoc, df.pimpact)

# Recoding variables and then scoring the youth self-report SDQ scores

robeys <- recode (SDQdata$sdq_student7 , "0=2; 1=1; 2=0; else=NA")
rreflect <- recode (SDQdata$sdq_student11, "0=2; 1=1; 2=0; else=NA")
rattends <- recode (SDQdata$sdq_student14, "0=2; 1=1; 2=0; else=NA")
rfriend <- recode (SDQdata$sdq_student21, "0=2; 1=1; 2=0; else=NA")
rpopular <- recode (SDQdata$sdq_student25, "0=2; 1=1; 2=0; else=NA")

rrdistres <- recode (SDQdata$sdq_student28, "0:1=0; 2=1; 3=2; NA=0")
rrimphome <- recode (SDQdata$sdq_student29a, "0:1=0; 2=1; 3=2; NA=0")
rrimpfrie <- recode (SDQdata$sdq_student29b, "0:1=0; 2=1; 3=2; NA=0")
rrimpclas <- recode (SDQdata$sdq_student29c, "0:1=0; 2=1; 3=2; NA=0")
rrimpleis <- recode (SDQdata$sdq_student29d, "0:1=0; 2=1; 3=2; NA=0")

df.semotion <- data.frame(SDQdata$sdq_student3, SDQdata$sdq_student8, SDQdata$sdq_student13, SDQdata$sdq_student16, SDQdata$sdq_student24)
snemotion <- apply(df.semotion, 1, function(x) sum(is.na(x)))
semotion <- ifelse(snemotion<3, rowMeans(df.semotion, na.rm=TRUE), NA)
semotion <- as.numeric(semotion) * 5
semotion <- floor(0.5 + semotion)

df.sconduct <- data.frame(SDQdata$sdq_student5, SDQdata$sdq_student7, SDQdata$sdq_student12, SDQdata$sdq_student18, SDQdata$sdq_student22)
snconduct <- apply(df.sconduct, 1, function(x) sum(is.na(x)))
sconduct <- ifelse(snconduct<3, rowMeans(df.sconduct, na.rm=TRUE), NA)
sconduct <- as.numeric(sconduct) * 5
sconduct <- floor(0.5 + sconduct)

df.shyper <- data.frame(SDQdata$sdq_student2, SDQdata$sdq_student10, SDQdata$sdq_student15, SDQdata$sdq_student21, SDQdata$sdq_student25)
snhyper <- apply(df.shyper, 1, function(x) sum(is.na(x)))
shyper <- ifelse(snhyper<3, rowMeans(df.shyper, na.rm=TRUE), NA)
shyper <- as.numeric(shyper) * 5
shyper <- floor(0.5 + shyper)

df.speer <- data.frame(SDQdata$sdq_student6, SDQdata$sdq_student11, SDQdata$sdq_student14, SDQdata$sdq_student19, SDQdata$sdq_student23)
snpeer <- apply(df.speer, 1, function(x) sum(is.na(x)))
speer <- ifelse(snpeer<3, rowMeans(df.speer, na.rm=TRUE), NA)
speer <- as.numeric(speer) * 5
speer <- floor(0.5 + speer)

df.sprosoc <- data.frame(SDQdata$sdq_student1, SDQdata$sdq_student4, SDQdata$sdq_student9, SDQdata$sdq_student17, SDQdata$sdq_student20)
snprosoc <- apply(df.sprosoc, 1, function(x) sum(is.na(x)))
sprosoc <- ifelse(snprosoc<3, rowMeans(df.sprosoc, na.rm=TRUE), NA)
sprosoc <- as.numeric(sprosoc) * 5
sprosoc <- floor(0.5 + sprosoc)

df.simpact <- data.frame(SDQdata$sdq_student28, SDQdata$sdq_student29a, SDQdata$sdq_student29b, SDQdata$sdq_student29c, SDQdata$sdq_student29d)
snimpact <- apply(df.simpact, 1, function(x) sum(is.na(x)))
simpact <- ifelse(!snimpact==5, SDQdata$sdq_student28+SDQdata$sdq_student29a+SDQdata$sdq_student29b+SDQdata$sdq_student29c+SDQdata$sdq_student29d, NA)
simpact <- ifelse(SDQdata$sdq_student26==0, 0, simpact)
simpact <- as.numeric(simpact)

sebdtot <- semotion+sconduct+shyper+speer

rm (robeys, rreflect, rattends, rfriend, rpopular, rrdistres, rrimphome, rrimpfrie, rrimpclas, rrimpleis, snemotion, snconduct, snhyper, snpeer, snprosoc, snimpact, df.semotion, df.sconduct, df.shyper, df.speer, df.sprosoc, df.simpact)

# Recoding variables and then scoring the teacher SDQ scores

uobeys <- recode (SDQdata$sdq_q7, "0=2; 1=1; 2=0; else=NA")
ureflect <- recode (SDQdata$sdq_q11, "0=2; 1=1; 2=0; else=NA")
uattends <- recode (SDQdata$sdq_q14, "0=2; 1=1; 2=0; else=NA")
ufriend <- recode (SDQdata$sdq_q21, "0=2; 1=1; 2=0; else=NA")
upopular <- recode (SDQdata$sdq_q25, "0=2; 1=1; 2=0; else=NA")

uudistres <- recode (SDQdata$sdq_q28, "0:1=0; 2=1; 3=2; NA=0")
uuimpfrie <- recode (SDQdata$sdq_q29a, "0:1=0; 2=1; 3=2; NA=0")
uuimpclas <- recode (SDQdata$sdq_q29b, "0:1=0; 2=1; 3=2; NA=0")

df.temotion <- data.frame(SDQdata$sdq_q3, SDQdata$sdq_q8, SDQdata$sdq_q13, SDQdata$sdq_q16, SDQdata$sdq_q24)
tnemotion <- apply(df.temotion, 1, function(x) sum(is.na(x)))
temotion <- ifelse(tnemotion<3, rowMeans(df.temotion, na.rm=TRUE), NA)
temotion <- as.numeric(temotion) * 5
temotion <- floor(0.5 + temotion)

df.tconduct <- data.frame(SDQdata$sdq_q5, SDQdata$sdq_q7, SDQdata$sdq_q12, SDQdata$sdq_q18, SDQdata$sdq_q22)
tnconduct <- apply(df.tconduct, 1, function(x) sum(is.na(x)))
tconduct <- ifelse(tnconduct<3, rowMeans(df.tconduct, na.rm=TRUE), NA)
tconduct <- as.numeric(tconduct) * 5
tconduct <- floor(0.5 + tconduct)

df.thyper <- data.frame(SDQdata$sdq_q2, SDQdata$sdq_q10, SDQdata$sdq_q15, SDQdata$sdq_q21, SDQdata$sdq_q25)
tnhyper <- apply(df.thyper, 1, function(x) sum(is.na(x)))
thyper <- ifelse(tnhyper<3, rowMeans(df.thyper, na.rm=TRUE), NA)
thyper <- as.numeric(thyper) * 5
thyper <- floor(0.5 + thyper)

df.tpeer <- data.frame(SDQdata$sdq_q6, SDQdata$sdq_q11, SDQdata$sdq_q14, SDQdata$sdq_q19, SDQdata$sdq_q23)
tnpeer <- apply(df.tpeer, 1, function(x) sum(is.na(x)))
tpeer <- ifelse(tnpeer<3, rowMeans(df.tpeer, na.rm=TRUE), NA)
tpeer <- as.numeric(tpeer) * 5
tpeer <- floor(0.5 + tpeer)

df.tprosoc <- data.frame(SDQdata$sdq_q1, SDQdata$sdq_q4, SDQdata$sdq_q9, SDQdata$sdq_q17, SDQdata$sdq_q20)
tnprosoc <- apply(df.tprosoc, 1, function(x) sum(is.na(x)))
tprosoc <- ifelse(tnprosoc<3, rowMeans(df.tprosoc, na.rm=TRUE), NA)
tprosoc <- as.numeric(tprosoc) * 5
tprosoc <- floor(0.5 + tprosoc)

df.timpact <- data.frame(SDQdata$sdq_q28, SDQdata$sdq_q29a, SDQdata$sdq_q29b)
tnimpact <- apply(df.timpact, 1, function(x) sum(is.na(x)))
timpact <- ifelse(!tnimpact==3, SDQdata$sdq_q28+SDQdata$sdq_q29a+SDQdata$sdq_q29b, NA)
timpact <- ifelse(SDQdata$sdq_q26==0, 0, timpact)
timpact <- as.numeric(timpact)

tebdtot <- temotion+tconduct+thyper+tpeer

rm (uobeys, ureflect, uattends, ufriend, upopular, uudistres, uuimpfrie, uuimpclas, tnemotion, tnconduct, tnhyper, tnpeer, tnprosoc, tnimpact, df.temotion, df.tconduct, df.thyper, df.tpeer, df.tprosoc, df.timpact)


#merge together

# List of variables to keep
variables_to_keep <- c(  "pemotion", "pconduct", "phyper", "ppeer", "pprosoc", "pimpact",
                         "semotion", "sconduct", "shyper", "speer", "sprosoc", "simpact",
                        "temotion", "tconduct", "thyper", "tpeer", "tprosoc", "timpact")

# Extract the variables to keep from the environment and assign names
variables_data <- lapply(variables_to_keep, function(var_name) {
  var_data <- as.data.frame(get(var_name))  # Convert to data frame
  colnames(var_data) <- var_name  # Assign the variable name as column name
  var_data
})

# Merge the original dataset with the variables to keep
merged_data <- cbind(SDQdata, do.call(cbind, variables_data))


# Write the merged data to an Excel file
write.xlsx(merged_data, 'mergedSDQ_data.xlsx')




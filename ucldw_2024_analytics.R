##########################################################
### UCDLW 2024 - Workshop Registration Analytics ###
##########################################################

# META

# Data source: UCDLW_2024_Registrations.csv
# Dates for analysis: February 2024
# Contributors: UCDLW Committee
# Questions? Script prepared by: 
#         Pamela Reynolds (plreynolds@ucdavis.edu)
#         Kat Koziar (katherine.koziar@ucr.edu, github: kekoziar) 
##########################################################

# MANUAL PREPARATION
# on google sheet
# -- filled in missing data using vlookup
# -- standardized capitalization
# -- fixed emails based on assessment email bounces
# -- fixed workshop dates
##########################################################

# CAVEATS

# -- Not all information captured across all workshops
# -- for role and current institution, some registration surveys
#        allowed multiple selections
# -- To account for multiple values, in most summary statistics, 
#        value was changed to nonControlVocab
# -- Registration counts can often be assumed to be greater than actual attendance data
# -- deduplicating is not exact, ~44 unique registrants used multiple email addresses
#        some are different domains/orgs. 
##########################################################

# LIBRARIES


library(dplyr)
library(tidyverse)
library(data.table) #function %like%
library("openxlsx") #output to excel file

# artifacts of previous year's analytics. unneeded so far
# library(stringr) 
#library(psych)
#library(readxl) # if reading in from xls
##########################################################

# DIRECTORY

# Set working directory and where you want output to be saved locally
setwd("~/Documents/Teaching/UCDLW/UCLDW2023")
path_out = "~/Documents/Teaching/UCDLW/UCLDW2023/output"
##########################################################

# READ IN DATA

my.data<-read.csv("UCLDW_2024_Registrations.csv")
df<-my.data
##########################################################
##########################################################
# CLEANING
##########################################################
# remove punctuation in column names
colnames(df)<-str_trim(colnames(df))
colnames(df)<-str_replace_all(colnames(df), "[:punct:]", "") # removes the ?'s
colnames(df)<-str_replace_all(colnames(df), "\\s+", ".") # the + tackles if there are one or more spaces to trim
colnames(df)<- str_to_lower(colnames(df))
colnames(df)

# changing column names to be more sensible; need to specify that rename is for dplyr if both dplyr and plyr are being used
df<-dplyr::rename(df, workshop=nameofworkshop, 
                      email=emailaddress, 
                      role = areyoua, 
                      location = currentinstitution, 
                      department = whatisyourhomedepartmentororganization, 
                      domain = withwhatdomaindoyoumostidentify, 
                      experience=pleaserateyourpriorexperiencewiththeworkshoptopic,  
                      past.attendee=didyouattendapreviousuclovedataweek, 
                      distribution=howdidyoufirsthearaboutthisworkshop)

df[is.na(df)] <- "unknown"
df[df == ""] <- "unknown"

# a little bit of cleanup
df$past.attendee<-str_to_title(df$past.attendee) 

#will query unique departments later
# but there is no controlled vocab, so this will at least reduce it by a little
df$department<-str_to_lower(df$department) 
df$department<-gsub("&"," and ",df$department)
df$department<-gsub("  "," ",df$department)

# to query email domains (entity) and top-level domains (org)
df$email<-str_to_lower(df$email)          # removes capitalization in some email addresses
df$email.entity<-gsub(".*@","",df$email)  # returns only after the @, proxy for entity (ie ucdavis, ucla)
df$email.org<-gsub(".*\\.","",df$email.entity)  # returns only after the . (need double escape, \\, because . is special)
# email.entity is @* and email.org is .*
df$email.org<-as.factor(df$email.org)  # returns edu, com, gov, etc.
df$email.entity<-as.factor(df$email.entity) # returns ucdavis.edu, etc.

##########################################################
## useful variables for the future
##########################################################

locations <- c("UC ANR",   #useful for more than just sorting
               "UC Berkeley",
               "UC Davis",
               "UC Irvine",
               "UCLA",
               "UC Merced",
               "UC Riverside",
               "UC San Diego",
               "UC San Francisco",
               "UC Santa Barbara",
               "UC Santa Cruz",
               "UCOP",
               "California State University (CSU)",
               "Other University",
               "Government",
               "Industry",
               "Nonprofit",
               "Other")

# to create a column which sorts locations, prioritizing UCs
names_df <- names(df) # because merge resorts columns... 
df_loc <- data.frame(loc = locations,
                     loc_sort = c(1:length(locations)))
df <- merge(df, df_loc, by.x = "location", by.y = "loc", all.x = TRUE)
df <- df[, c(names_df,"loc_sort")]
df$loc_sort[is.na(df$loc_sort)] <- (length(locations)+1) 
rm(df_loc) #just some cleanup...
##########################################################
##########################################################
# METRICS - Totals
# don't need this anymore. export to excel below
# Basic Statistics
#total.sessions<-length(unique(df$workshop))
#total.registrants<-length(df$email)
#uniq.registrants <- length(unique(df$email))
#total.location <- length(unique(df$location))
#uniq.departs <- length(unique(df$department))

# more complex statistics (two levels)
#reg.by.workshop<-df %>% count(hostuc, workshop)
#reg.by.workshop.current<-df %>% count(workshop, location)
#reg.by.workshop.role<-df %>% count(workshop, role)

##########################################################
##########################################################
# CREATE SUMMARY VARIABLES
  
# Domain
#  df$domain<- as.factor(df$domain) #don't need this
  domains <- c("Health sciences",
               "Humanities",
               "Life sciences",
               "Mathematical and computational sciences",
               "Physical sciences",
               "Social sciences", 
               "unknown")

############################
# For domains 
#  some registrants listed more than one domain. 
#  to capture all of the data we use two variables. 
#  
#  This is duplicated/not unique, 
#   so unique registrant that registered for more than one workshop is counted twice
#
#   registrant_domain$total indicates total number of registrant that lists domain
#   registrant_domain$exact indicates total number of registrant that only lists domain
#
############################

## clean up some comma/space issues for multiple domains
df$domain <- str_replace_all(df$domain, ", ", ",")  

registrant_domain <- data.frame(total=vector("numeric", length=length(domains)), 
                                  exact=vector("numeric", length=length(domains)), 
                                  row.names = domains)
  i <- 1
  for(domain in domains){
    registrant_domain$total[i] <- length(df$domain[df$domain %like% domain])
    registrant_domain$exact[i] <- sum(df$domain == domain, na.rm=TRUE)
    i <- i+1
  }



# Registrant Roles
  
  roles <- c("Faculty",
             "Librarian",
             "Medical Resident",
             "Physician / Clinician",
             "Postdoc",
             "Professional Researcher",
             "Staff",
             "Student graduate",
             "Student undergraduate",
             "UC Alumni",
             "Other", 
             "unknown",
             "Non-Controlled Vocabulary")
  
  # cleanup so commas are separators within the list elements 
  role_toReplace <- c("Student, undergraduate", "Student, graduate", "\\|", " , ", ", ")
  role_Replacement <- c("Student undergraduate", "Student graduate", ",", ",", ",")
  
  for (i in 1:(length(role_toReplace))){
    df$role <- str_replace_all(df$role, role_toReplace[i], role_Replacement[i])
  }
  
  registrant_role <- data.frame(total=vector("numeric", length=length(roles)), 
                                  exact=vector("numeric", length=length(roles)), 
                                  row.names = roles)
  i <- 1
  for(role in roles){
    registrant_role$total[i] <- length(df$role[df$role %like% role])
    registrant_role$exact[i] <- sum(df$role == role, na.rm=TRUE)
    i <- i+1
  }
  
  registrant_role$total[length(registrant_role$total)] <- length(df$role[grep(paste(roles, collapse="|"), df$role, invert=TRUE)])
  roles_nonControlledVocab <- unique(df$role[grep(paste(roles, collapse="|"), df$role, invert=TRUE)])

  ## check if roles & domains in df are standardized
  unique(strsplit(paste(as.list(df$role), collapse = ','), ",")[[1]])
  unique(strsplit(paste(as.list(df$domain), collapse = ','), ",")[[1]])
  
#   role <- df$role #  
#  df$role<- as.factor(df$role)
  #levels:   
#  role.undergrad<-sum(df$role== "Student, undergraduate", na.rm=TRUE)
#  role.grad.postdoc<-sum(df$role== "Student, graduate" | df$role== "Postdoc" , na.rm=TRUE)
#  role.faculty<-sum(df$role== "Faculty", na.rm=TRUE)
#  role.staff<-sum(df$role== "Staff" | df$role=="Professional Researcher", na.rm=TRUE)
#  role.other<-sum(df$role=="Alumni" | df$role=="Other" | df$role=="Medical Resident", na.rm=TRUE)


# Overall Summary
#results<-data.frame(total.registrants, uniq.departments, email.academia, email.govt, email.other, domain.math.cs, domain.physical.sci, domain.life.sci, domain.health.sci, domain.soc.sci, domain.humanities, loc.davis, loc.sacramento, loc.other, role.undergrad, role.grad.postdoc, role.faculty, role.staff, role.other, pronoun.he, pronoun.she, pronoun.they, exp.none, exp.some, exp.extensive)
#results





###############################################################

# BASIC DATA VISUALIZATION 

# some new variables for color coding - aren't they pretty?

uc_colors <- c("UC ANR" = "#ab312c",
              "UC Berkeley" = "#003262",
              "UC Davis" = "#00B2E3",
              "UC Irvine" = "#7c109a",
              "UCLA" = "#2774AE",
              "UC Merced" = "#6ba43a",
              "UC Riverside" = "#E4002B",
              "UC San Diego" = "#C69214",
              "UC San Francisco" = "#561038",
              "UC Santa Barbara" = "#9CBEBE",
              "UC Santa Cruz" = "#fdc700",
              "UCOP" = "#BDE3F6",
              "UC Davis Health" = "#79242F",
              "UCD" = "#00B2E3")

##########################################################
### all because ggplot + 
### sorting + 
### color coding the labels +
### adding multiple colors to the column
##### doesn't seem to want to work together.
  #
  # slacker.
workshop_summary<-unique(df[,c("workshop", "hostuc")])
workshop_summary$color <- uc_colors[workshop_summary$hostuc]
workshop_summary <- workshop_summary[order(as.character(workshop_summary$workshop), decreasing=TRUE),]
workshop_colors<-setNames(workshop_summary$color, workshop_summary$workshop)



# ggplot(df, aes(x=location))+geom_bar()
# ggplot(df, aes(x=hostuc))+geom_bar()


##########################################################
# let's make some plots
##########################################################


##########################################################
# workshop, sorted by largest registration, with totals included
df %>%
  count(workshop) %>%
  arrange(desc(n)) %>%
  mutate(workshop = fct_reorder(workshop, n))  %>%
  ggplot(aes(x=n, y=workshop))+
  geom_bar(stat="identity")+
  geom_text(aes(label=n), vjust = 0.5, hjust = -.24)+
  scale_y_discrete(expand = expansion(mult = c(0.05, .05))) +
  scale_x_continuous(expand = expansion(mult = c(0.001, .1))) +
  labs(y="Workshop", x="Registration Count")

##########################################################
# need to sort df for the label colors to match
df$workshop<- factor(df$workshop, levels=c(sort(unique(df$workshop), decreasing=TRUE)))

# workshop totals with color coded registrant location & workshop Host
ggplot(df, aes(y=workshop, fill = location))+
  geom_bar()+
  scale_fill_manual(values=uc_colors) + 
  labs(y="Workshop", x="Registration Count", fill="Registrant Location") +
  theme(axis.text.y = element_text(colour=unname(workshop_colors), face="bold"))
  # colors matching, but wish this can sort in order

##########################################################
### total registration by domain and role ###
ggplot(registrant_domain, 
       aes(y=reorder(rownames(registrant_domain),total), x=total))+
        geom_col()+
        geom_text(aes(label=total), vjust = 0.5, hjust = -.25)+
        scale_y_discrete(expand = expansion(mult = c(0.1, .1))) +
        scale_x_continuous(expand = expansion(mult = c(0.001, .07))) +
        labs(y="Registrant Domain", x="Registration Count")

ggplot(registrant_role, 
  aes(y=reorder(rownames(registrant_role),total), x=total, ))+
  geom_col()+
  geom_text(aes(label=total), vjust = 0.5, hjust = -.25)+
  scale_y_discrete(expand = expansion(mult = c(0.05, .05))) +
  scale_x_continuous(expand = expansion(mult = c(0.001, .07))) +
  labs(y="Registrant Role", x="Registration Count")

###############################################################
##########################################################
# cleaning new df for controlled vocabulary of campus & role
##########################################################

df.controlVocab <- data.frame(df)

df.controlVocab$role[grep(paste(roles, collapse="|"), df.controlVocab$role, invert=TRUE)] <- "nonControlVocab"
df.controlVocab$role[grep(",", df.controlVocab$role, invert=FALSE)] <- "nonControlVocab"

df.controlVocab$location[grep(paste(locations, collapse="|"), df.controlVocab$location, invert=TRUE)] <- "nonControlVocab"
df.controlVocab$location[grep(",", df.controlVocab$location, invert=FALSE)] <- "nonControlVocab"
df.controlVocab$location[grep("Nonprofit|Industry", df.controlVocab$location, invert=FALSE)] <- "Other"

df.controlVocab$location<- factor(df.controlVocab$location, 
                                  levels=c(sort(unique(df.controlVocab$location), decreasing=TRUE)))

### plot registrant campus with count, color coded by registrant's role ###
ggplot(df.controlVocab, aes(y=location, fill = role))+
  geom_bar()+
  scale_fill_manual(values=unname(uc_colors)) + 
#  geom_text(aes(label=total), vjust = 0.5, hjust = -.25)+
#  scale_y_discrete(expand = expansion(mult = c(0.1, .1))) +
#  scale_x_continuous(expand = expansion(mult = c(0.001, .07))) +
  labs(y="Registrant Campus", x="Total Registration Count", fill="Registrant Role")




df.cvDedup <- data.frame(df.controlVocab[!duplicated(df.controlVocab$email),])

ggplot(df.cvDedup, aes(y=location, fill = role))+
  geom_bar()+
  scale_fill_manual(values=unname(uc_colors)) + 
  labs(y="Registrant Campus", x="Deduplicated Registration Count", fill="Registrant Role")



#########################################################
# output to some tables 

############
# writing to xlsx (easier to see in tabs)

df_labels <- names(df)
cols2remove <- c("fullname","email","department","email.entity")
df_labels <- df_labels[!df_labels %in% cols2remove]

## check unique values
for(i in 1:length(df_labels)){
  print(unique(df[,df_labels[i]]))
}

############
wb <- createWorkbook("ucldw24")

############
tmp_label <-"Basic Workshop Info"
addWorksheet(wb, tmp_label)
tmp_df_wide <- df %>% count(df$workshopdate, df$hostuc, df$workshop)
colnames(tmp_df_wide) <- c("Date", "Host UC", "Workshop Title", "Total")
writeData(wb, 
               tmp_label, 
               x = tmp_df_wide)

############
tmp_label <-"Available Variables"
addWorksheet(wb, tmp_label)
writeData(wb, 
               tmp_label, 
               x = as.data.frame(names(df)))


############
# Basic Statistics
############
tmp_label <-"Basic Statistics"
addWorksheet(wb, tmp_label)

startRow <- 1
startCol <- 1
basic_stats <- data.frame(Statistic = c("Total sessions","Total Registrations", 
                                        "Unique Registrants (by email)", "Unique Current Institution",
                                        "Unique Departments (self-reported)"),
                          Number = c(length(unique(df$workshop)),length(df$email),
                                     length(unique(df$email)), length(unique(df$location)), 
                                     length(unique(df$department))))

writeData(wb, tmp_label, startRow = startRow, x = basic_stats)

startRow <- startRow + (length(basic_stats[,1])+2)
writeData(wb, tmp_label, startRow = startRow, x = "Current Institution")

startRow <- startRow + 1

basic_stats <- df %>% group_by(loc_sort) %>% count(location)
colnames(basic_stats) <- c("sort", "Institution", "Duplicated Total")
writeData(wb, tmp_label, startRow = startRow, 
          x = basic_stats
          )
startCol <- startCol+ (length(basic_stats[1,])+1)


df.cvDedup <- data.frame(df.controlVocab[!duplicated(df.controlVocab$email),])

basic_stats <- df[!duplicated(df$email),] %>% group_by(loc_sort) %>% count(location)
colnames(basic_stats) <- c("sort", "Institution", "Deduplicated Total")
writeData(wb, tmp_label, startRow = startRow, startCol = startCol,
          x = basic_stats
)


############
# summary tables
############
tmp_label <-"Registrants"
addWorksheet(wb, tmp_label)
writeData(wb, tmp_label, x = "Deduplicated Registrants")

startRow <- 2;
tmp_df_wide <- df.cvDedup %>% count(df.cvDedup$location, df.cvDedup$role)
colnames(tmp_df_wide) <- c("Registrant Campus", "Registrant Role", "Total")
tmp_df_write <- spread(tmp_df_wide, key = "Registrant Role", value = Total, fill = 0)
tmp_df_write <- tmp_df_write %>% map_df(rev)
writeData(wb, tmp_label, startRow = startRow, x = tmp_df_write)

startRow <- startRow + (length(tmp_df_write$`Registrant Campus`)+3)
writeData(wb, tmp_label, startRow = startRow, x ="Duplicated Registrants")

startRow <- startRow+1;
tmp_df_wide <- df.controlVocab %>% count(df.controlVocab$location, df.controlVocab$role)
colnames(tmp_df_wide) <- c("Registrant Campus", "Registrant Role", "Total")
tmp_df_write <- spread(tmp_df_wide, key = "Registrant Role", value = Total, fill = 0)
tmp_df_write <- tmp_df_write %>% map_df(rev)
writeData(wb, tmp_label, startRow = startRow, x = tmp_df_write)

############
tmp_label <-"Workshop by Domain, cVocab"
addWorksheet(wb, tmp_label)
tmp_df_wide <- df %>% count(df$workshop, df$domain)
colnames(tmp_df_wide) <- c("Workshop Title", "Registrant Domain", "Total")
tmp_df_write <- spread(subset(tmp_df_wide, `Registrant Domain` %in% domains), 
                       key = "Registrant Domain", 
                       value = Total, 
                       fill = 0
)
writeData(wb, 
          tmp_label, 
          x = tmp_df_write)

############
tmp_label <-"Workshop by Domain, other"
addWorksheet(wb, tmp_label)
tmp_df_write <- spread(subset(tmp_df_wide, !(`Registrant Domain` %in% domains)), 
                       key = "Registrant Domain", 
                       value = Total, 
                       fill = 0
)
writeData(wb, 
          tmp_label, 
          x = tmp_df_write)




############################################################
# filter by location
###############################################################
for(uc in locations[1:11]){
  tmp_label <-uc
  addWorksheet(wb, tmp_label)
  
  startRow <- 1;
  writeData(wb, tmp_label, startRow = startRow, x ="Workshop Registrants by Role")
  
  startRow <- startRow + 1;
  tmpdf_subset <- subset(df, location %in% uc)
  
  ### workshop by role
  tmpdf_long <- tmpdf_subset %>% count(tmpdf_subset$workshop, tmpdf_subset$role)
  colnames(tmpdf_long) <- c("Workshop", "Role", "Total")
  
  tmp_df_write <- spread(tmpdf_long, 
                         key = Role, 
                         value = Total, 
                         fill = 0
                         )
  writeData(wb, tmp_label, startRow = startRow, x = tmp_df_write)
  
  startRow <- startRow + (length(tmp_df_write[,1])+3)
  writeData(wb, tmp_label, startRow = startRow, x ="Workshop Registrants by Domain")
  
  startRow <- startRow+1;
  
    
  ### workshop by domain
  tmpdf_long <- tmpdf_subset %>% count(tmpdf_subset$workshop, tmpdf_subset$domain)
  colnames(tmpdf_long) <- c("Workshop", "Domain", "Total")
  tmp_df_write <- spread(tmpdf_long, 
                         key = Domain, 
                         value = Total, 
                         fill = 0
  )
  
  writeData(wb, tmp_label, startRow = startRow, x = tmp_df_write)
}

############################################################
# save excel workbook
###############################################################

if (FALSE) {
  saveWorkbook(wb, file = "ucldw24-statistics-20240326.xlsx", overwrite = TRUE)
}
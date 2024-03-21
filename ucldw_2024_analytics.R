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

# Added column "Host", with levels being the UC code
# Moved UCSB entries for Data-DeID and Data Visualization workshops
# under Pronouns column to new column "Requests", as they were in the incorrect column
# Added NA for any row missing Email Address
# Completed Current Institution based on email address suffix for Reproducibility for Everyone workshop; 
# used NA for email addresses where suffix was not indicative 
# Host for Reproducibility for Everyone listed as Multiple (UCB + UCD graduate student led)
##########################################################

# CAVEATS

# Not all information captured across all workshops
# No registration data from 4 "informal" activities hosted by UCD (R, Python and Julia User Group Meetup, WiDS Datathon)
# Domain: some forms restricted to one entry, whereas others allowed multiple to be selected
# Registration counts can often be assumed to be greater than actual attendance data
##########################################################

# LIBRARIES

library(readxl) # if reading in from xls
library(dplyr)
library(stringr)
library(data.table)
library(psych)
library(tidyverse)
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

# CLEANING

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

# create column for email domain (splitting at the @)
df$email<-str_to_lower(df$email)          # removes capitalization in some email addresses
df$email.entity<-gsub(".*@","",df$email)  # returns only after the @, proxy for entity (ie ucdavis, ucla)
df$email.org<-gsub(".*\\.","",df$email.entity)  # returns only after the . (need double escape, \\, because . is special)
# email.entity is @* and email.org is .*
df$email.org<-as.factor(df$email.org)  # returns edu, com, gov, etc.
df$email.entity<-as.factor(df$email.entity) # returns ucdavis.edu, etc.


##########################################################

# BASIC METRICS

df.table<-as.data.frame(df) #why?

# Total Number Workshops
total.workshops<-length(unique(df$workshop))
total.events<-total.workshops+4  # what are these? what's the difference between events and workshops?
total.workshops # 21
total.events   # 25

# Total Registrations
total.registrants<-length(df$email)
total.registrants  # 2098

# Unique Registrants
uniq.registrants <- length(unique(df$email))   # 1110

# Total Attended
#total.attended<-sum(df$attended == "yes", df$attended == "Yes", df$attended == "YES", df$attended == "attended", df$attended == "Attended", df$attended == "x", df$attended =="X", df$attended == "walk-on",df$attended == "walk-in", na.rm=TRUE)

# Unique "Departments"
  df$department<- as.factor(df$department)
  uniq.departments<-length(unique(df$department))

# Unique Current Institutions
  uniq.location <- length(unique(df$location))

# Registrants per workshop by host
  reg.by.workshop<-df %>% count(hostuc, workshop)

# Registrants per workshop by current institution
  reg.by.workshop.current<-df %>% count(workshop, location)

##########################################################
# CREATE SUMMARY VARIABLES
  
# Domain
  df$domain<- as.factor(df$domain) #don't need this
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

  
#   role <- df$role #  
  df$role<- as.factor(df$role)
  #levels:   
  role.undergrad<-sum(df$role== "Student, undergraduate", na.rm=TRUE)
  role.grad.postdoc<-sum(df$role== "Student, graduate" | df$role== "Postdoc" , na.rm=TRUE)
  role.faculty<-sum(df$role== "Faculty", na.rm=TRUE)
  role.staff<-sum(df$role== "Staff" | df$role=="Professional Researcher", na.rm=TRUE)
  role.other<-sum(df$role=="Alumni" | df$role=="Other" | df$role=="Medical Resident", na.rm=TRUE)


# Overall Summary
results<-data.frame(total.registrants, uniq.departments, email.academia, email.govt, email.other, domain.math.cs, domain.physical.sci, domain.life.sci, domain.health.sci, domain.soc.sci, domain.humanities, loc.davis, loc.sacramento, loc.other, role.undergrad, role.grad.postdoc, role.faculty, role.staff, role.other, pronoun.he, pronoun.she, pronoun.they, exp.none, exp.some, exp.extensive)
results

###############################################################

# BASIC DATA VISUALIZATION

uc_colors = c("UC ANR" = "#ab312c",
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

ggplot(df, aes(x=location))+geom_bar()
ggplot(df, aes(x=hostuc))+geom_bar()
ggplot(df, aes(y=workshop, fill = location))+geom_bar()+scale_fill_manual(values=uc_colors)

ggplot(registrant_domain, 
       aes(y=reorder(rownames(registrant_domain),total), x=total))+
        geom_col()+
        geom_text(aes(label=total), vjust = 0.5, hjust = -.25)+
        scale_y_discrete(expand = expansion(mult = c(0.1, .1))) +
        scale_x_continuous(expand = expansion(mult = c(0.001, .07))) +
        labs(y="Registrant Domain")

ggplot(registrant_role, 
       aes(y=reorder(rownames(registrant_role),total), x=total))+
        geom_col()+
        geom_text(aes(label=total), vjust = 0.5, hjust = -.25)+
        scale_y_discrete(expand = expansion(mult = c(0.05, .05))) +
        scale_x_continuous(expand = expansion(mult = c(0.001, .07))) +
        labs(y="Registrant Role")

###############################################################


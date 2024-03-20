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
               "Social sciences")

############################
# For domains 
#  some registrants listed more than one domain. 
#  to capture all of the data we use two variables. 
#  
#  This is unduplicated/not unique, 
#   so unique registrant that registered for more than one workshop is counted twice
#
#   registrant_domain$total indicates total number of registrant that lists domain
#   registrant_domain$exact indicates total number of registrant that only lists domain
#
############################
  
  i <- 1
  registrant_domain <- data.frame(total=vector("numeric", length=length(domains)), 
                                  exact=vector("numeric", length=length(domains)), 
                                  row.names = domains)
  for(domain in domains){
    registrant_domain$total[i] <- length(df$domain[df$domain %like% domain])
    registrant_domain$exact[i] <- sum(df$domain == domain, na.rm=TRUE)
    i <- i+1
  }



# Registrant Roles
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

ggplot(df, aes(x=workshop))+geom_bar()
ggplot(df, aes(x=hostuc))+geom_bar()
ggplot(df, aes(x=location))+geom_bar()
ggplot(df, aes(x=pronouns))+geom_bar()

###############################################################


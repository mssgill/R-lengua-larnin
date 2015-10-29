###### Working on the Oct 7 data
# MSSG
# 10-7-2015

## Set up for one row, 2 cols in the plot
par(mfrow=c(1,2))

 readdat = 1 # If we need to read in the data
# readdat = 0

if (readdat == 1) {
  ## Invites table
  mi=read.csv("/Users/m/mozmed/language_R_AndDataAnalysisCourse/R-lengua-larnin/10-07-15.intakeinvites.cleaned.csv", colClasses = c(receiver_userid='character') ) 
  
  ## Nodes table
  mn=read.csv("/Users/m/mozmed/language_R_AndDataAnalysisCourse/R-lengua-larnin/10-07-15.nodes.cleaned.csv", colClasses = c(userid='character') )
  
  ## User table
  mu = read.csv("/Users/m/mozmed/language_R_AndDataAnalysisCourse/R-lengua-larnin/10-07-15.users.cleaned.csv", colClasses = c(userid='character') )
  
  ## Userforms table
  muf = read.csv("/Users/m/mozmed/language_R_AndDataAnalysisCourse/R-lengua-larnin/10-07-15.userforms.cleaned.csv" ,  colClasses = c(userid='character') )
  
}
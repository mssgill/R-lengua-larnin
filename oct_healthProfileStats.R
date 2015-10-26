###### Working on the Oct 7 data
# MSSG
# 10-7-2015

## Set up for one row, 2 cols in the plot
par(mfrow=c(1,2))

# readdat = 1 # If we need to read in the data
readdat = 0

if (readdat == 1) {
## Invites table
 mi=read.csv("/Users/m/mozmed/language_R_AndDataAnalysisCourse/R-lengua-larnin/10-07-15.intakeinvites.cleaned.csv")

## Nodes table
 mn=read.csv("/Users/m/mozmed/language_R_AndDataAnalysisCourse/R-lengua-larnin/10-07-15.nodes.cleaned.csv")

## User table
 mu = read.csv("/Users/m/mozmed/language_R_AndDataAnalysisCourse/R-lengua-larnin/10-07-15.users.cleaned.csv")

## Userforms table
muf = read.csv("/Users/m/mozmed/language_R_AndDataAnalysisCourse/R-lengua-larnin/10-07-15.userforms.cleaned.csv")
}

####################### Make plots of merged users and nodes tables
plottype = 'merged'  # To plot stuff from users dataset -- like Gender, birthyr, deathyr
  plottype = 'none'
if (plottype == 'merged'){
  mergeddat = merge(mu,mn,by.x="userid",by.y="userid")  
  md= mergeddat
  mdi = md[md$type == 2,]  # Pick only intake users -- note ending comma
  dim(mdi)                # How many in this table 
  
  ####### Gender table
  {
  freq =   table(mdi$gender)
  ## Get percentages
 pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs
  
  pie(freq,col=rainbow(10))
  pie(freq,col=rainbow(10),labels = pctlabels)
}
  hist(mdi$birthyear, col='green', xlab = 'Birth Year', ylab='Number', main='Birth Year Distribution')
  hist(mdi$birthyear, col='green', xlab = 'Birth Year', ylab='Number', main='Birth Year Distribution')
  
  ######## Birth year
  {
  byear = mdi$birthyear[mdi$birthyear < 2010]
  totnum = length(byear)
  histname =paste("Birth Year before 2010: N = ",as.character(totnum))
  plot(hist(byear), main=histname, xlab="Birth Year", ylab = "Number", col='lawngreen')
  }
  
} # End
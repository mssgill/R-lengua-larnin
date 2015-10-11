###### Working on the Oct 7 data
# MSSG
# 10-7-2015

## Set up for one row, 2 cols in the plot
par(mfrow=c(1,2))

## Invites table
mi=read.csv("/Users/m/mozmed/language_R_AndDataAnalysisCourse/R-lengua-larnin/10-07-15.intakeinvites.cleaned.csv")

## Nodes table
mn=read.csv("/Users/m/mozmed/language_R_AndDataAnalysisCourse/R-lengua-larnin/10-07-15.nodes.cleaned.csv")

## User table
mu = read.csv("/Users/m/mozmed/language_R_AndDataAnalysisCourse/R-lengua-larnin/10-07-15.users.cleaned.csv")

plottype = 'none'
# plottype = 'nodes'  # To plot stuff from nodes dataset
if (plottype == 'nodes'){
  mm = mn
  freq=table(mm$birthyear) ## Birthyr

  dim(table(mm$familyid )) # Tells you how long the table of famID occurrences is -- i.e. how many unique famIDs
  plot(mm$familyid)
  hist(mm$familyid)

  mm2 = mm[mm$familyid > 1e18,] # Subset that has only those members with famID > 1e18
  }

plottype = 'users'  # To plot stuff from users dataset
if (plottype == 'users'){

  freq =   table(mu$type)
## Get percentages
  pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs

  pie(freq,col=rainbow(10))
  pie(freq,col=rainbow(10),labels = pctlabels)
}

# attach(mm)

##### Not used right now
# datestamp=as.POSIXct(as.numeric(email),origin="1970-01-01")

## Copy to another dataset
# mm2 =mm

## Make a freq table
#freq=(table(mm$sender_orgid))


## Get percentages
#pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs


## Make the pie charts
# pie(freq,col=rainbow(10))  # Pie chart of the sender types
# pie(freq,col=rainbow(10),labels = pctlabels) # Pie chart of the percentages

### Dataset of only bounces
#mmb=mm2[mm2$template_name=='bounce',]


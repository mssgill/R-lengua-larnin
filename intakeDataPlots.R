###### Make a couple of pie charts for which clinic is sending
###### out invites the most
# MSSG
# 10-5-2015

mm=read.csv("/Users/m/mozmed/intakeUsageData/mac_jt_071515_sendgrid_events_9-23-15.csv")
attach(mm)

##### Not used right now
# datestamp=as.POSIXct(as.numeric(email),origin="1970-01-01")

## Copy to another dataset
mm2 =mm

## Make a freq table
freq=(table(mm2$sender_orgid))

## Get percentages
pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs

## Set up for one row, 2 cols in the plot
 pie(freq,col=rainbow(10),labels = pct)

## Make the pie charts
pie(freq,col=rainbow(10))  # Pie chart of the sender types
 pie(freq,col=rainbow(10),labels = pctlabels) # Pie chart of the percentages

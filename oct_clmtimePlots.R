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

  #### Make plots from invites table
plottype = 'claimtimes'  # To plot stuff from invites dataset -- Like conversion rate info
  plottype = 'none'
if (plottype == 'claimtimes'){
    
  ############# Dif in times
  # Get the sent times into a vector of timedate objs
{
  
 senttimes =  mi$sent_at                # Get sent times
  senttimesStr = as.character(senttimes) # Convert this list to strs
  senttimesAstime = strptime(senttimesStr, format="%m/%d/%y %H:%M") # This is what converts the string into an
  # actual datetime obj, the strptime function takes the str however it has been
  # formatted, and you tell it the form in quotes. %H stands for hours in 24-hr format, %M is minutes.

  # Do same for claimed times
  clmtimes = mi$claimed_at
  clmtimesStr = as.character(clmtimes)
  clmtimesAstime = strptime(clmtimesStr, format="%m/%d/%y %H:%M")

  
  }  
  # Now take the dif, and plot as histos
{
  td = (clmtimesAstime - senttimesAstime) / 3600 # Divide by 3600 to get hrs
  hrtimes = (as.numeric(na.omit(td))/24)        # Omit the NA's (non-claimed ones), and convert to hours and real nums
  hrtimes = (hrtimes[hrtimes > 0])
  totnum = length(hrtimes)
  histname =paste("Claimed Minus Sent Time: N = ",as.character(totnum))
  hrhist = hist(hrtimes, main= histname, xlab="Time Dif (hours)", ylab = "Number", col='blue')
# First hour  
  firsthrTimes = (hrtimes[hrtimes < 1])  # Pick stuff that is only in the first hr
  totnum = length(firsthrTimes)
  histname =paste("First hr: Clm Minus Sent: N = ",as.character(totnum))
  fhrhist = hist(firsthrTimes, main= histname, xlab="Time Dif (hours)", ylab = "Number", col='green')
}
  
  ## To plot histos of the cume sums over the time period
{
  hrhist$density = cumsum(hrhist$counts)/sum(hrhist$counts)*100
  totnum = length(hrtimes)
  histname =paste("Claimed Minus Sent Time: N = ",as.character(totnum))
  plot(hrhist,freq=F,main= histname, xlab="Time Dif (hours)", ylab = "Percentage", col='red')
  ## First hr cume sum
  fhrhist$density = cumsum(fhrhist$counts)/sum(fhrhist$counts)*100
  totnum = length(firsthrTimes)
  histname =paste("First Hr, Clm Minus Sent: N = ",as.character(totnum))
  plot(fhrhist,freq=F, main=histname, xlab="Time Dif (hours)", ylab = "Percentage", col='cyan')
}  
  # If you *don't* want percentage on the y-axis, but raw number, do:
   {
  hrhist$counts = cumsum(hrhist$counts)
  fhrhist$counts = cumsum(fhrhist$counts)
  
  plot(hrhist)
  plot(fhrhist)
  }
} # End
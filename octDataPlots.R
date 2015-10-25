###### Working on the Oct 7 data
# MSSG
# 10-7-2015

## Set up for one row, 2 cols in the plot
par(mfrow=c(1,2))

readdat = 1 # If we need to read in the data
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

plottype = 'invites' # To plot stuff from invites dataset -- Like clinic name for the full dataset
 plottype = 'none'  
if (plottype == 'invites'){
  freq=table(mi$sender_name) 
  freq
  ## Get percentages
  pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs
  
  pie(freq,col=rainbow(10))
  pie(freq,col=rainbow(10),labels = pctlabels)
  
}

 plottype = 'nodes'  # To plot stuff from nodes dataset -- Like famID number
   plottype = 'none'
if (plottype == 'nodes'){
  mm = mn
  
  dim(table(mm$familyid )) # Tells you how long the table of famID occurrences is -- i.e. how many unique famIDs
  plot(mm$familyid)
  hist(mm$familyid)

  mm2 = mm[mm$familyid > 1e18,] # Subset that has only those members with famID > 1e18
  }

 
plottype = 'users'  # To plot stuff from users dataset -- Like usertype: 0 = famtree, 1 = Admin, 2 = from invites
  plottype = 'none'
if (plottype == 'users'){

  freq =   table(mu$type)
## Get percentages
  pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs

  pie(freq,col=rainbow(10))
  pie(freq,col=rainbow(10),labels = pctlabels)
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
  freq =   table(mdi$gender)
  ## Get percentages
  pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs
  
  pie(freq,col=rainbow(10))
  pie(freq,col=rainbow(10),labels = pctlabels)

  hist(mdi$birthyear, col='green', xlab = 'Birth Year', ylab='Number', main='Birth Year Distribution')
  
  ######## Birth year
  freq =   table(mdi$birthyear)
  ## Get percentages
  pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs

  ### These are too granular, need to be binned differently  
#  pie(freq,col=rainbow(10))
#  pie(freq,col=rainbow(10),labels = pctlabels)
  
  
  ######## Death year
  freq =   table(mdi$deathyear)
  ## Get percentages
  pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs
  
  pie(freq,col=rainbow(10))
  pie(freq,col=rainbow(10),labels = pctlabels)
  
  }

  #### Make plots from invites table
plottype = 'claimtimes'  # To plot stuff from invites dataset -- Like conversion rate info
  plottype = 'none'
if (plottype == 'claimtimes'){
    
  ############# Dif in times
  # Get the sent times into a vector of timedate objs
  senttimes =  mi$sent_at                # Get sent times
  senttimesStr = as.character(senttimes) # Convert this list to strs
  senttimesAstime = strptime(senttimesStr, format="%m/%d/%y %H:%M") # This is what converts the string into an
  # actual datetime obj, the strptime function takes the str however it has been
  # formatted, and you tell it the form in quotes. %H stands for hours in 24-hr format, %M is minutes.

  # Do same for claimed times
  clmtimes = mi$claimed_at
  clmtimesStr = as.character(clmtimes)
  clmtimesAstime = strptime(clmtimesStr, format="%m/%d/%y %H:%M")
  
  # Now take the dif -- because they are both timedate objs, the minus operator gives the time dif back in seconds
  td = (clmtimesAstime - senttimesAstime) / 3600 # Divide by 3600 to get hrs
  hrtimes = (as.numeric(na.omit(td))/24)        # Omit the NA's (non-claimed ones), and convert to hours and real nums
  hrhist = hist(hrtimes)                            # Histogram this
  firsthrTimes = (hrtimes[hrtimes < 1])  # Pick stuff that is only in the first hr
  fhrhist = hist(firsthrTimes)                      # Histogram this
  
  # To plot histos of the cume sums over the time period
  hrhist$counts = cumsum(hrhist$counts)
  fhrhist$counts = cumsum(fhrhist$counts)
  
  plot(hrhist)
  plot(fhrhist)

  ######## Make the pie chart of sender name, including unclaimed
    freq=table(mi$sender_name) 
    ## Get percentages
    pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs
    
    pie(freq,col=rainbow(10))
    pie(freq,col=rainbow(10),labels = pctlabels)
    
  ######## Now make the pie chart of sender name, those that are claimed
  
  freq=table(mi$sender_name[ clmtimesStr!="NULL"]) 
  
  ## Get percentages
  pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs
  
  pie(freq,col=rainbow(10))
  pie(freq,col=rainbow(10),labels = pctlabels)
  
  ######### Now just get folks after Sep 1
  sep1 = strptime("9/1/15", format="%m/%d/%y")
  postSept1_Senders = mi$sender_name[clmtimesAstime > sep1]  
  freq=table(postSept1_Senders)
  ## Get percentages
  pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs
  
  pie(freq,col=rainbow(10))
  pie(freq,col=rainbow(10),labels = pctlabels)

  ######### Now just get folks BEFORE Sep 1
  sep1 = strptime("9/1/15", format="%m/%d/%y")
  preSept1_Senders = mi$sender_name[clmtimesAstime < sep1]  
  freq = table(preSept1_Senders)
  ## Get percentages
  pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs
  
  pie(freq,col=rainbow(10))
  pie(freq,col=rainbow(10),labels = pctlabels)
  
  ############## Get delay to appt date-time
  
  # Do same for claimed times
  appttimes = mi$appt_datetime
  appttimesStr = as.character(appttimes)
  appttimesAstime = strptime(appttimesStr, format="%m/%d/%y %H:%M")
  
  # Now take the dif -- because they are both timedate objs, the minus operator gives the time dif back in seconds
  td = (appttimesAstime - clmtimesAstime) / 3600 # Divide by 3600 to get hrs
  hrtimes = (as.numeric(na.omit(td))/24)        # Omit the NA's (non-claimed ones), and convert to hours and real nums
  hrhist = hist(hrtimes ,main="Appt Times Minus Claim Times", 
                xlab="Time Dif (hours)", ylab = "Number", col = 'blue')                              # Histogram this
  firsthrTimes = (hrtimes[hrtimes < 1])  # Pick stuff that is only in the first hr
  fhrhist = hist(firsthrTimes, main="Appt Times Minus Claim Times -- First hr", 
                 xlab="Time Dif (hours)", ylab = "Number", col = 'blue')                      # Histogram this
  
  # To plot histos of the cume sums over the time period
  hrhist$counts = cumsum(hrhist$counts)
  fhrhist$counts = cumsum(fhrhist$counts)
  
  plot(hrhist)
  plot(fhrhist)
  
  
  }

  ############################ Make plots of merged userforms and invites table -- like conversion rate info
 plottype = 'completedforms'  
#  plottype = 'none'
 if (plottype == 'completedforms'){
 
   ############### Get the merged dataset
    {   mi$rid = as.numeric(as.character(mi$receiver_userid))  # This will give: Warning message:
   # In eval(expr, envir, enclos) : NAs introduced by coercion -- from the Nulls when converted (i believe)
   
   mergeddat = merge(muf,mi,by.x="userid",by.y="rid")  
   mufi= mergeddat
   mdi = mufi[mufi$type == 'intake' && mufi$iscomplete == 1,]  # Pick only intake users -- note ending comma
   md = mdi
     dim(md)                # How many in this table 
}  
    ############# Get difs in sent, claim, submission times
     if (1==0)  {   # Get the sent times into a vector of timedate objs
   senttimes =  md$sent_at                # Get sent times
   senttimesStr = as.character(senttimes) # Convert this list to strs
   senttimesAstime = strptime(senttimesStr, format="%m/%d/%y %H:%M") # This is what converts the string into an
   # actual datetime obj, the strptime function takes the str however it has been
   # formatted, and you tell it the form in quotes. %H stands for hours in 24-hr format, %M is minutes.
   
   # Do same for claimed times
   clmtimes = md$claimed_at
   clmtimesStr = as.character(clmtimes)
   clmtimesAstime = strptime(clmtimesStr, format="%m/%d/%y %H:%M")
   
   # Do same for claimed times
   subtimes = md$submitted_at
   subtimesStr = as.character(subtimes)
   subtimesAstime = strptime(subtimesStr, format="%m/%d/%y %H:%M")
   
   # Do same for appt date-time
   appttimes = mi$appt_datetime
   appttimesStr = as.character(appttimes)
   appttimesAstime = strptime(appttimesStr, format="%m/%d/%y %H:%M")
     }
    ################ Plot claimTimes - sentTimes
     if (1==0)  { # Now take the dif -- because they are both timedate objs, the minus operator gives the time dif back in seconds
   td = (clmtimesAstime - senttimesAstime) / 3600 # Divide by 3600 to get hrs
   hrtimes = (as.numeric(na.omit(td))/24)        # Omit the NA's (non-claimed ones), and convert to hours and real nums
   hrtimes = (hrtimes[hrtimes >0])  # Pick stuff that is only in the first hr
   hrhist = hist(hrtimes, main="Claim Times Minus Sent Times", 
                 xlab="Time Dif (hours)", ylab = "Number", col='red')                            # Histogram this
   firsthrTimes = (hrtimes[hrtimes < 1])  # Pick stuff that is only in the first hr
   firsthrTimes = (firsthrTimes[firsthrTimes >0])  # Pick stuff that is only in the first hr
   fhrhist = hist(firsthrTimes, 
                  main="Claim Times Minus Sent Times -- First Hour",
                  xlab="Time Dif (hours)", ylab = "Number", col='purple')                      # Histogram this
   }
    ################ Plot submissionTimes - claimTimes 
     if (1==0)  {  # Now take the dif -- because they are both timedate objs, the minus operator gives the time dif back in seconds
   td = ( subtimesAstime - clmtimesAstime) / 3600 # Divide by 3600 to get hrs
   hrtimes = (as.numeric(na.omit(td))/24)        # Omit the NA's (non-claimed ones), and convert to hours and real nums
   hrtimes = (hrtimes[hrtimes >0])  # Pick stuff that is only in the first hr
   hrhist = hist(hrtimes, main="Claim Times Minus Submission Times", 
                 xlab="Time Dif (hours)", ylab = "Number", col = 'blue')                            # Histogram this
   firsthrTimes = (hrtimes[hrtimes < 1])    # Pick stuff that is only in the first hr
   firsthrTimes = (firsthrTimes[firsthrTimes >0])  # Pick stuff that is only in the first hr
   fhrhist = hist(firsthrTimes, main="Claim Times Minus Submission Times -- First Hour",
                  xlab="Time Dif (hours)", ylab = "Number", col='lightgreen')                      # Histogram this
 } 
     ################ Plot apptTimes - claimTimes 
     if (1==1)  {     # Now take the dif -- because they are both timedate objs, the minus operator gives the time dif back in seconds
   td = (appttimesAstime - clmtimesAstime) / 3600 # Divide by 3600 to get hrs
   hrtimes = (as.numeric(na.omit(td))/24)        # Omit the NA's (non-claimed ones), and convert to hours and real nums
   hrtimes = (hrtimes[hrtimes >0])  # Pick stuff that is only in the first hr
   hrhist = hist(hrtimes ,main="Appt Times Minus Claim Times", 
                 xlab="Time Dif (hours)", ylab = "Number", col = 'blue')                              # Histogram this
   firsthrTimes = (hrtimes[hrtimes < 1 ])  # Pick stuff that is only in the first hr
   firsthrTimes = (firsthrTimes[firsthrTimes >0])  # Pick stuff that is only in the first hr
   fhrhist = hist(firsthrTimes, main="Appt Times Minus Claim Times -- First hr", 
                  xlab="Time Dif (hours)", ylab = "Number", col = 'blue')                      # Histogram this
   
   # To plot histos of the cume sums over the time period
   hrhist$counts = cumsum(hrhist$counts)
   fhrhist$counts = cumsum(fhrhist$counts)
   
   plot(hrhist)
   plot(fhrhist)
}  

}
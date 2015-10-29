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
   {  senttimes =  mi$sent_at                # Get sent times
  senttimesStr = as.character(senttimes) # Convert this list to strs
  senttimesAstime = strptime(senttimesStr, format="%m/%d/%y %H:%M") # This is what converts the string into an
  # actual datetime obj, the strptime function takes the str however it has been
  # formatted, and you tell it the form in quotes. %H stands for hours in 24-hr format, %M is minutes.

  # Do same for claimed times
  clmtimes = mi$claimed_at
  clmtimesStr = as.character(clmtimes)
  clmtimesAstime = strptime(clmtimesStr, format="%m/%d/%y %H:%M")
} 
  
  ######## Make the pie chart of sender name, including unclaimed
   {  freq=table(mi$sender_name) 
    ## Get percentages
    pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs
    
    pie(freq,col=rainbow(10))
    pie(freq,col=rainbow(10),labels = pctlabels)
  } 
  ######## Now make the pie chart of sender name, those that are claimed
   {
  freq=table(mi$sender_name[ clmtimesStr!="NULL"]) 
  
  ## Get percentages
  pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs
  
  pie(freq,col=rainbow(10))
  pie(freq,col=rainbow(10),labels = pctlabels)
    }
  ######### Now just get folks after Sep 1
  {
  sep1 = strptime("9/1/15", format="%m/%d/%y")
  postSept1_Senders = mi$sender_name[clmtimesAstime > sep1]  
  freq=table(postSept1_Senders)
  ## Get percentages
  pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs
  
  pie(freq,col=rainbow(10))
  pie(freq,col=rainbow(10),labels = pctlabels)
}
  ######### Now just get folks BEFORE Sep 1
  {
  sep1 = strptime("9/1/15", format="%m/%d/%y")
  preSept1_Senders = mi$sender_name[clmtimesAstime < sep1]  
  freq = table(preSept1_Senders)
  ## Get percentages
  pctlabels <- round((freq/sum(freq)*100),1)  # To 3 sig figs
  
  pie(freq,col=rainbow(10))
  pie(freq,col=rainbow(10),labels = pctlabels)
  }
  
   }  
   
} # End
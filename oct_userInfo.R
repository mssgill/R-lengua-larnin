###### Working on the Oct 7 data
# MSSG
# 10-7-2015

## Set up for one row, 2 cols in the plot
par(mfrow=c(1,2))

######### Load up the needed data files
readdat = 1 # If we need to read in the data
# readdat = 0
path="/Users/m/mozmed/language_R_AndDataAnalysisCourse/R-lengua-larnin/"
loadcode = paste(path, "oct_loadDataSets.R", sep='')
if (readdat == 1) source(loadcode)

  # if(!exists("mu$userid", mode="function")) # Another way, haven't tested it much though, so for now we just set the flag by hand
  
 plottype = 'users'  # To plot stuff from users dataset -- Like usertype: 0 = famtree, 1 = Admin, 2 = from invites
  plottype = 'none'
if (plottype == 'users'){

  # User type
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
  
}
  
########### Invites to completed funnel yields
if (plottype == 'merged'){
    
  mi$rid = as.character(mi$receiver_userid)  
  mergeddat = merge(muf,mi,by.x="userid",by.y="rid")  
  mufi= mergeddat
  md = mufi[mufi$type == 'intake' ,]  # Pick only intake users -- note ending comma
  mdcomplete = mufi[mufi$type == 'intake' && mufi$iscomplete == 1,]  # Pick only intake users -- note ending comma
  dim(md)                # How many in this table 
  
  ## Numbers for the funnel:
  
  dim(mi)                           # Invited
  table(mufi$claimed_at != "NULL")  # Claimed
  table(mufi$submitted_at != "NULL") # Submitted
  table(mufi$iscomplete)            # Complete
} # End
  
###########  Number of invites sent
  { 
  numresends=hist(table((ms$invite_uuid[ms$template_type=='invite' & ms$event=='delivered' & ms$email!='support@mozart.md'])  ),breaks=5,col='blue', main="Number of invites sent", xlab='Number of sends', ylab='Number')
  numresends$density = (numresends$counts)/sum(numresends$counts)*100
  totnum = sum(numresends$counts)
  histname =paste("Num invites sent, Total: N = ",as.character(totnum))
  plot(numresends,freq=F, main=histname, xlab="Num of sends", ylab = "Percentage", col='green')
  }

  msi = merge(ms,mi, by.x='email', by.y ='receiver_email')
  
  ########### Completion by domain
  {
  # Get the vecs for each email
  edomain_yahoo = grep('yahoo.com',mufi$receiver_email)
  edomain_gmail = grep('gmail.com',mufi$receiver_email)
  edomain_comcast = grep('comcast.net',mufi$receiver_email)
  edomain_aol = grep('aol.com',mufi$receiver_email)
  edomain_att = grep('att.net',mufi$receiver_email)
  
  # Make the vector
  l = array(1:6)
  
  # Assign vals to it
  l[2] = length(edomain_gmail)
  l[3] = length(edomain_yahoo)
  l[4] = length(edomain_comcast)
  l[5]  = length(edomain_aol)
  l[6] = length(edomain_att)

  tot = dim(mufi)[1]
  l[1] = tot-(l[2]+l[3]+l[4]+l[5]+l[6])
  
  # Make the plot
  pie(l)

  ########### Completed ones
  # Make the vector
  comp = array(1:6)
  
  comp[2] =  table(mufi$iscomplete[edomain_gmail])[2]
  comp[3] =  table(mufi$iscomplete[edomain_yahoo])[2]
  comp[4] =  table(mufi$iscomplete[edomain_comcast])[2]
  comp[5] =  table(mufi$iscomplete[edomain_aol])[2]
  comp[6] =  table(mufi$iscomplete[edomain_att])[2]
  
  comptot =  table(mufi$iscomplete)[2]
  comp[1] = comptot-(comp[2]+comp[3]+comp[4]+comp[5]+comp[6])
  pie(comp)
  }
  
  
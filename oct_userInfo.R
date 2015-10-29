###### Working on the Oct 7 data
# MSSG
# 10-7-2015

## Set up for one row, 2 cols in the plot
par(mfrow=c(1,2))

readdat = 1 # If we need to read in the data
# readdat = 0

######### Load up the needed data files
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
  
########### Merge userforms and invites tables 
if (plottype == 'merged'){
    
  mi$rid = as.character(mi$receiver_userid)  # This will give: Warning message:
  # In eval(expr, envir, enclos) : NAs introduced by coercion -- from the Nulls when converted (i believe)
  # But is needed to properly do the merge in the next line, by converting a factor to a double
  
  mergeddat = merge(muf,mi,by.x="userid",by.y="rid")  
  mufi= mergeddat
  md = mufi[mufi$type == 'intake' ,]  # Pick only intake users -- note ending comma
  mdcomplete = mufi[mufi$type == 'intake' && mufi$iscomplete == 1,]  # Pick only intake users -- note ending comma
  dim(md)                # How many in this table 
  
} # End
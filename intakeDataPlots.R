mm=read.csv("/Users/m/mozmed/intakeUsageData/mac_jt_071515_sendgrid_events_9-23-15.csv")
attach(mm)
datestamp=as.POSIXct(as.numeric(email),origin="1970-01-01")

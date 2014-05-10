plot2 <- function() {
  
  ## Read the data, assume the data is in default folder
  pc <- read.csv("household_power_consumption.txt", sep=';')
  
  ## Append the Date, Time Col
  pc[, 10] <-  paste(pc[,1], pc[,2], sep=':') 
  
  ## Transform the above col to Date\Time
  mydate <- strptime(pc[, 10],format='%d/%m/%Y:%H:%M:%S')
  
  ## Append the Date\Time to form a new Data Frame
  z2 <- cbind(pc, mydate)
  
  ## Filter the '?' to form a new Data Frame 
  z3 <- z2[
    (z2$Global_active_power!="?") & 
      (z2$Global_reactive_power!="?") &  
      (z2$Voltage!="?")  & 
      (z2$Global_intensity!="?") & 
      (z2$Sub_metering_1!="?") &  
      (z2$Sub_metering_2!="?") &  
      ( !(is.na(z2$Sub_metering_3)) )
    ,]
  
  ## Filter the date range to 2007-Feb-01, 02 only
  df <- z3[  (z3$mydate <= strptime("02/02/2007:23:59:00",format='%d/%m/%Y:%H:%M:%S')) &  
               (strptime("01/02/2007:00:00:00",format='%d/%m/%Y:%H:%M:%S') <=  z3$mydate)
             ,  ]
  
  ## Convert the string to numeric col 
  df[, 3] <- as.numeric(as.character(df[, 3]))
  df[, 4] <- as.numeric(as.character(df[, 4]))
  df[, 5] <- as.numeric(as.character(df[, 5]))
  df[, 6] <- as.numeric(as.character(df[, 6]))
  df[, 7] <- as.numeric(as.character(df[, 7]))
  df[, 8] <- as.numeric(as.character(df[, 8]))
  df[, 9] <- as.numeric(as.character(df[, 9]))
  
  ## Save the graph to PNG 
  png("plot2.png", width = 480, height = 480)
  plot(df$mydate, df$Global_active_power, type="n", ylab="Global Active Power (kilowatts)", xlab=""  )
  lines(df$mydate, df$Global_active_power )
  dev.off()
  
}
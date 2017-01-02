data = read.csv("household_power_consumption.txt", header = TRUE, sep = ";", stringsAsFactors = FALSE)

#convert the Date variable to a date class

data$Date <-  as.Date(data$Date, format="%d/%m/%Y")


#subset to dates  2007-02-01 and 2007-02-02 and remove the raw data
#frame to clear disk memory
dataSmall = data[data$Date == "2007-02-01" |data$Date =="2007-02-02",]
rm(data)

#create an array that concatenates the day and time
dateAndTime <- paste(as.character(dataSmall$Date), dataSmall$Time)

#convert the new variable to a time class
dataSmall$Time <- strptime(dateAndTime, "%Y-%m-%d %H:%M:%S")
summary(dataSmall$Time)
rm(dateAndTime)

#convert variables 3-9 to numeric
numericArray <- apply(dataSmall[,3:9], 2, as.numeric)
vector <- 3:9

#create a temp dataset to duplicate dataSmall's data
dataTest <- dataSmall

#run a for loop to replace the values of col 3-9 by its numeric
#counter parts stored in 'numericArray'
for(i in seq_along(vector)){
    dataTest[,vector[i]] <- numericArray[,i]
}

#rm variabels and data frames that will no longer be used
rm(numericArray)
rm(vector)
rm(i)
rm(dataSmall)
rm(dataTest)
epc <- dataTest

########Plot 1 - Histogram #########
#Plot a red coloured histogram of the "Global Active Power" (kilowatts)
hist(epc$Global_active_power, col = "red",
     xlab = "Global Active Power (kilowatts)")

#now save the plot in PNG format
#Note: I decided not to use the 'dev.copy' function since Prof. Peng #mentioned it might not be an exact copy of what was displayed in the
#screen device.
png("plot1.png")
hist(epc$Global_active_power, col = "red",
     xlab = "Global Active Power (kilowatts)",
     main= "Global Active Power")

dev.off()
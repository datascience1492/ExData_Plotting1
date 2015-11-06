# Read the lines between date1 and date2 from my_path:
my_path <- "household_power_consumption.txt"
date1 <- as.Date("2007-02-01")
date2 <- as.Date("2007-02-02")

my_file <- file(my_path,"r")
my_data <- NULL

repeat {
  my_line <- readLines(my_file,1)
  if(!is.na(as.Date(unlist(strsplit(my_line,";"))[1],"%d/%m/%Y"))) break
}

while(as.Date(unlist(strsplit(my_line,";"))[1],"%d/%m/%Y") < date1) {
  my_line <- readLines(my_file,1)
}

my_i <- 0
repeat {
  my_i <- my_i+1
  if(length(my_line)==0 | as.Date(unlist(strsplit(my_line,";"))[1],"%d/%m/%Y") > date2) break;
  my_data[my_i] <- my_line
  my_line <- readLines(my_file,1)
}

close(my_file)
rm(my_i,my_file,my_line,date1,date2)
# At this point, my_data is a vector whose elements are the lines between date1 and date2

# Transform it into a data frame:
my_rows <- length(my_data)
my_data <- unlist(strsplit(my_data,";"))
my_cols <- as.integer(length(my_data)/my_rows)
dim(my_data) <- c(my_cols,my_rows)
my_data <- data.frame(t(my_data))

# Convert columns to the right classes:
my_data[,1] <- as.Date(my_data[,1],"%d/%m/%Y")
for(my_i in 3:my_cols) my_data[,my_i] <- as.numeric(as.character((my_data[,my_i])))

# Give it names from the first line of the file if it makes sense:
my_names <- unlist(strsplit(readLines(my_path,1),";"))
if(length(my_names)==my_cols) names(my_data) <- my_names
rm(my_cols,my_rows,my_names,my_path,my_i)
# At this point, my_data contains the data frame we need

# Create histogram and save as PNG:
png("plot1.png", height=480, width=480)
hist(my_data$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")
dev.off()
# Done
# OSM and ISM Generator: extract all individual similarity matrices (ISMs) 
# and generate the overall similarity matrix(OSM) by summing up all ISMs
OsmIsmGenerator <- function(path, scenario.name, number.of.icons, icon.list) {

	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}

	# Construct the zip folder path and list all zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	# Initialize osm with the 1st ISM
	participant1 <- unzip(paste(zip.path, files[1], sep = ""))
	
	# Get the participant number for the first participant
	first.p.number <- substring(files[1], 1, nchar(files[1]) - 4)
	
	# Construct the full file name for ISM file (mtrx file)
	first.ism <- paste("./", first.p.number, "/", substring(files[1],1,8), ".mtrx", sep = "")
	
	# Read in the ISM from the 1st participant and exclude the non-ism info from the .mtrx file
	first.matrix <- read.delim(first.ism, header = FALSE, sep = " ", stringsAsFactors = F)
	first.matrix <- data.matrix(first.matrix[1:number.of.icons, ])
	
	# Export the first ISM
	write.table(first.matrix, file = paste(path, "ism/", "participant", 
                                        substr(files[1], 1, nchar(files[1]) - 4),
                                        ".mtrx",sep = ""), sep = " ", row.names = F, col.names = F)
	
	write.table(first.matrix, file = paste(path, "matrices/", "participant", 
                                        substr(files[1], 1, nchar(files[1]) - 4),
                                        ".mtrx",sep = ""), sep = " ", row.names = F, col.names = F)
	
	# Summing up all ISMs for OSM and export each ISM
	osm <- first.matrix
	
	# Process the ISMs of the rest of participants
	for(i in 2:length(files)) {
		# Unzip the participant's zip file
		participant.i <- unzip(paste(zip.path, files[i], sep = ""))
		
		# Get the participant number
		participant.number <- substring(files[i], 1, nchar(files[i]) - 4)
		
		# Construct the full file name for .mtrx file
		matrix.i.name <- paste("./", participant.number, "/", substring(files[i],1,8), ".mtrx", sep = "")
		
		# Read in the ISM from a participant and exclude the non-ism info from the .mtrx file
		matrix.i <- read.delim(matrix.i.name, header = F, sep = " ", stringsAsFactors = F)
		matrix.i <- data.matrix(matrix.i[1:number.of.icons, ])
		
		# Export the ISM as .mtrx for KlipArt and .csv for catanalysis
		write.table(matrix.i, file = paste(path, "ism/", "participant", 
						substr(files[i], 1, nchar(files[i]) - 4),
						".mtrx", sep = ""), sep = " ", 
						row.names = F, col.names = F)
		
		write.table(matrix.i, file = paste(paste(path, scenario.name, "-klipart/", sep = ""), "matrices/", "participant", 
						substr(files[i], 1, nchar(files[i]) - 4), 
						".mtrx", sep = ""), sep = " ",
						row.names = F, col.names = F)
		
		write.table(matrix.i, file = paste(path, "matrices/", "participant", 
						substr(files[i], 1, nchar(files[i]) - 4), 
						".mtrx", sep = ""), sep = " ",
						row.names = F, col.names = F)
		
		# Add the ISM to OSM
		osm <- osm + matrix.i
	}
	
	# Export OSM
	write.table(osm, file = paste(path, "matrices/", "total.mtrx", sep = ""), 
			sep = " ", row.names = F,  col.names = F)
	
	write.table(osm, file = paste(paste(path, scenario.name, "-klipart/", sep = ""), "matrices/", "total.mtrx", sep = ""), 
			sep = " ", row.names = F,  col.names = F)
	
	osm <- cbind(icon.list, osm)
	write.table(osm, file = paste(path, "osm.csv", sep = ""), 
			sep = ",", row.names = F,  col.names = F)

	# Deletes all unzipped files as they are no longer needed
	folder.names <- substr(files, 1, nchar(files)-4)
  	for(folder in folder.names) {
    	unlink(paste(getwd(), "/", folder, sep = ""), recursive = TRUE)
  	}

}

OsmIsmGenerator(path, scenario.name, number.of.icons, icon.list)




# AssignmentGetter: generate the assignment.csv for KlipArt
AssignmentGenerator <- function(path, scenario.name) {

	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}

	# Create an empty dataframe
	df <- data.frame()
	
	#List all zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	for(i in 1:length(files)) {
		# Read in the assignment.csv file
		participant.i <- unzip(paste(zip.path, files[i], sep = ""))
		
		# Get the participant number for the first participant
		participant.number <- substring(files[i], 1, nchar(files[i]) - 4)
		
		# Construct the full file name for assignment.csv file
		participant.assignment <- paste("./", participant.number, "/", substring(files[i],1,8), 
				"assignment.csv", sep = "")
		
		assignment <- read.delim(participant.assignment, header = F, sep = ",", stringsAsFactors = F)
		df <- rbind(df, assignment)
	}
	# Export the assignment.csv
	write.table(df, file = paste(paste(path, scenario.name, "-klipart/", sep = ""), "assignment.csv", sep = ""),
			sep = ",", row.names = F,  col.names = F)

	# Deletes all unzipped files as they are no longer needed
	folder.names <- substr(files, 1, nchar(files)-4)
  	for(folder in folder.names) {
    	unlink(paste(getwd(), "/", folder, sep = ""), recursive = TRUE)
  	}
}

AssignmentGenerator(path, scenario.name)




# Participant info: collect demographic info and basic experiment info (# of groups created
# and time spent in seconds)
ParticipantInfoGenerator <- function(path, scenario.name) {
	
	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}

	# Read in the zip file
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	# Read in to demographic info for the 1st participant
	participant1 <- unzip(paste(zip.path, files[1],sep = ""))
	
	# Get the participant number for the first participant
	first.p.number <- substring(files[1], 1, nchar(files[1]) - 4)
	
	# Construct the full file name for the participant.csv file for the 1st participant
	first.demo <- paste("./", first.p.number, "/", substring(files[1],1,8), "participant.csv", sep = "")
	
	# Read in the participant.csv for the 1st participant
	demo1 <- read.delim(first.demo, header = F, sep = ",",stringsAsFactors = F)
	
	# Aggregate eduction background for participant who use comma(s) in their eduction 
	# background (e.g., geography, education, business)
	while(length(demo1) > 13) {
		demo1[7] <- paste(demo1[7], demo1[8], sep = ",")
		demo1 <- demo1[-8]
	}

	colnames(demo1) <- 1:13
	
	# Initialize the dataframe for demographic info
	demographic <- demo1
	
	# Add demographic info from the rest of participants to the dataframe "demographic"
	for(i in 2:length(files)) {
		participant.i <- unzip(paste(zip.path, files[i], sep=""))
		
		# Get the participant number for the first participant
		participant.number <- substring(files[i], 1, nchar(files[i]) - 4)
		
		# Construct the full file name for participant.csv file
		participant.demo <- paste("./", participant.number, "/", substring(files[i],1,8), 
				"participant.csv", sep = "")
		
		# Read in the participant.csv
		demo <- read.delim(participant.demo, header = F, sep = ",", stringsAsFactors = F)
		while(length(demo) > 13) {
			demo[7] <- paste(demo[7], demo[8], sep = ",")
			demo <- demo[-8]
		}
		colnames(demo) <- 1:13
		demographic <- rbind(demographic, demo)
	}
	
	# Create two vectors to store the # of groups created and time spent (in seconds)
	groups.created <- c()
	time.spent <- c()
	
	for(i in 1:length(files)) {
		# Read in the assignment.csv file
		participant.i <- unzip(paste(zip.path, files[i], sep = ""))
		
		# Get the participant number for the first participant
		participant.number <- substring(files[i], 1, nchar(files[i]) - 4)
		
		# Construct the full file name for assignment.csv file
		participant.groups <- paste("./", participant.number, "/", substring(files[i],1,8), 
				"assignment.csv", sep = "")
		
		groups <- read.delim(participant.groups, header = F, sep = ",", stringsAsFactors = F)
		
		# Count the number of rows in the assignment file and convert it to the # of groups created
		groups <- length(unique(groups[, 2]))
		
		# Append the # of groups created to the vector "groups.created"
		groups.created <- append(groups.created, groups)
	}
	
	
	for(i in 1:length(files)) {
		# Read in the log file
		participant.i <- unzip(paste(zip.path, files[i], sep=""))
		
		# Get the participant number for the first participant
		participant.number <- substring(files[i], 1, nchar(files[i]) - 4)
		
		# Construct the full file name for .log file
		participant.log <- paste("./", participant.number, "/", substring(files[i],1,8), ".log", sep = "")
		
		# Read in the log file
		log <- read.delim(participant.log, header = F, sep=",", stringsAsFactors = F)
		
		# Get the time spent
		time <- log[nrow(log), ]
		time <- substr(time, 33, nchar(time))
		
		# Append the time spent to the vector "time.spent"
		time.spent <- append(time.spent, time)
	}
	
	# Append two vectors (i.e., two columns) - groups.created and time.spent to the demographic dataframe
	demographic <- cbind(demographic, groups.created)
	demographic <- cbind(demographic, time.spent)
	
	# Export the demographic dataframe as a csv file
	write.table(demographic, file = paste(path, "participant.csv", sep = ""),
			sep = ",", row.names = F,  col.names = F)
	
	# Export the participant.csv for KlipArt
	write.table(demographic, file = paste(paste(path, scenario.name, "-klipart/", sep = ""), "participant.csv", sep = ""), sep = ",", row.names = F,  col.names = F)

	# Deletes all unzipped files as they are no longer needed
	folder.names <- substr(files, 1, nchar(files)-4)
  	for(folder in folder.names) {
    	unlink(paste(getwd(), "/", folder, sep = ""), recursive = TRUE)
  	}
}

ParticipantInfoGenerator(path, scenario.name)




# DescriptionGenerator: extract the linguistic labels (both long and short) from all participants and store in a single csv file
DescriptionGenerator <- function(path, scenario.name) {

	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}
	
	# Construct the path for the zip folder and list all the zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	# Unzip the zip file from the 1st participant
	participant1 <- unzip(paste(zip.path, files[1], sep =""))
	
	# Get the participant number for the first participant
	participant.number <- substring(files[1], 1, nchar(files[1]) - 4)
	
	# Construct the full file name for the batch.csv file
	batch <- paste("./", participant.number, "/", substring(files[1],1,8), "batch.csv", sep = "")
	
	description1 <- read.csv(batch, header = F, stringsAsFactors = F)
	
	# Aggregate participants' long descriptions when they use comma in the descriptions.
	while(length(description1) > 4) {
		description1[, 4] <- paste(description1[, 4], description1[,5], sep = ",")
		description1 <- description1[-5]
	}
	
	# Create dummy column names for the dataframe (will not be included when exported)
	colnames(description1) <- 1:4
	
	# Initialize a dataframe for all descriptions
	description <- description1
	
	# Read in the batch.csv for the rest of participants and extract the descriptions
	for(i in 2:length(files)) {
		
		# Read in the zip files
		participant.i <- unzip(paste(zip.path, files[i], sep = ""))
		description.i <- read.csv(sort(participant.i)[5], header = F, stringsAsFactors = F)
		
		# Aggregate participants' long descriptions when they use comma in the descriptions.
		while(length(description.i) > 4) {
			description.i[4] <- paste(description.i[, 4], description.i[, 5], sep = ",")
			description.i <- description.i[-5]
		}
		
		# Create dummy column names for the dataframe (will not be included when exported)
		colnames(description.i) <- 1:4
		
		# Combine descriptions from all participant into a dataframe (row-bind)
		description <- rbind(description, description.i)
	}
	
	# Export the description dataframe as a csv file
	write.table(description, file = paste(path, "description.csv", sep = ""), 
			sep = ",", row.names = F,  col.names = F)
	
	# Export batch.csv for Klipart
	write.table(description, file=paste(paste(path, scenario.name, "-klipart/", sep = ""), "batch.csv", sep = ""), sep = ",", col.names=  F, row.names = F)

	# Deletes all unzipped files as they are no longer needed
	folder.names <- substr(files, 1, nchar(files)-4)
  	for(folder in folder.names) {
    	unlink(paste(getwd(), "/", folder, sep = ""), recursive = TRUE)
  	}
}

DescriptionGenerator(path, scenario.name)




# Overview
# set the scenario here and file name
OverviewGenerator <- function(scenario.name, participant.info.path, number.of.participants) {
	output <- paste(scenario.name, "_overview.pdf", sep = "")
	data <- read.csv(participant.info.path, header=F, stringsAsFactors = F)
	
	male <- 0
	female <- 0
	for (i in 1:nrow(data)) {
		if (data[i, 3] == "male") {
			male <- male+1
		} else {
			female <- female+1
		}
	}
	
	aveage <- round(mean(data[, 2]), 2)
	max <- max(data[, 2])
	min <- min(data[, 2])
	
	
	pdf(file = output, onefile = T, width=10, height=25)
	
	layout(matrix(c(1,1,2,2,3,3,4,5), 4, 2, byrow = TRUE))
	
	plot.new()
	title(paste("Total participants: ", number.of.participants, ";", sep = ""), line = -18, cex = 20)
	title(paste("Male: ", male, ", Female: ", female, sep = ""), line = -20, cex = 20)
	title(paste("Average age: ", aveage, " (max: ", max, ", min: ", min, ")", sep = ""), line = -22, cex = 20)
	boxplot(data[, 14],
			horizontal = TRUE, 
			notch = FALSE,  # Notches for CI for median
			col = "slategray3",
			boxwex = 0.5,  # Width of box as proportion of original
			whisklty = 1,  # Whisker line type; 1 = solid line
			staplelty = 0,  # Staple (line at end) type; 0 = none
			outpch = 16,  # Symbols for outliers; 16 = filled circle
			outcol = "slategray3",  # Color for outliers
			main = "Groups Created")
	boxplot(data[, 15],
			horizontal = TRUE, 
			#notch = TRUE,  # Notches for CI for median
			col = "slategray3",
			boxwex = 0.5,  # Width of box as proportion of original
			whisklty = 1,  # Whisker line type; 1 = solid line
			staplelty = 0,  # Staple (line at end) type; 0 = none
			outpch = 16,  # Symbols for outliers; 16 = filled circle
			outcol = "slategray3",  # Color for outliers
			main = "Grouping Time")
	
	groupscount <- data.frame(table(data[, 14]))
	
	a <- groupscount$Var1
	b <- c()
	for (i in 1:length(a)) {
		b[i] <- toString(a[i])
	}
	
	groupmean <- mean(data[, 14])
	groupsd <- round(sd(data[, 14]), 2)
	
	barplot(groupscount$Freq, names.arg = b, 
			main = paste("Groups Created (mean = ", groupmean, ", ", "sd = ", groupsd, ")", sep = ""),
			xlab = "Number of groups created", ylab="Frequency")
	
	hist(data[, 15], col = "grey", main = paste("Grouping Time", " (mean = ", round(mean(data[, 15]), 2), "s", "," , " sd = ", round(sd(data[, 15]), 2), "s", ")", sep = ""), xlab = "Time spent on grouping in second")
	title(scenario.name, outer = T, line = -2, cex.main = 2, col.main = "blue")
	
	dev.off()
}

OverviewGenerator(scenario.name, paste(path, "/participant.csv", sep=""), number.of.participants)




















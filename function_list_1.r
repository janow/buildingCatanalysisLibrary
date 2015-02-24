## Kevin Sparks 2/24/15
# Make sure to run "function_list_pre.R" before attempting to run the executables at the
# bottom of this file. 















# Participant info: collect demographic info and basic experiment info (# of groups created
# and time spent in seconds)
ParticipantInfo <- function(path) {
	
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
	write.table(demographic, file = paste(klipart.path, "participant.csv", sep = ""), sep = ",", row.names = F,  col.names = F)
}















# DescriptionGetter: extract the linguistic labels (both long and short) from all participants and store in a single csv file
DescriptionGetter <- function(path) {
	
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
	write.table(description, file=paste(klipart.path, "batch.csv", sep = ""), sep = ",", col.names=  F, row.names = F)
}















# OsmViz: generates a heatmap based on the OSM.
# No dendrograms are generated and the icons are in alphabetical order
# Jinlong: It is intended to be a raw heat map without dendrograms. 
# The cluster heatmap function is right below this function
OsmViz <- function(path) {
	
	# Read in the osm.csv file and format the row/column names
	d <- read.csv(paste(path, "osm.csv", sep = ""), header = F)
	dm <- as.matrix(d[, -1])
	dimnames(dm) <- list(d[, 1], d[, 1])
	
	# The export of the heatmap is realized as a pngfile. Other options are ....??
	# Jinlong: other options includes jpg, bmp, png, etc. but each has its own function with
	# slightly different arguments and different default values for arguments
	# Drawing the heatmap and export as a tiff file

	# png(filename = paste(path, "heat_map.png", sep = ""),width = 2000, height = 2000, units = "px",
	# 		pointsize = 5,compression = "none", bg = "white", res = 600)
	png(filename = paste(path, "heat_map.png", sep = ""),width = 2000, height = 2000, units = "px",
			pointsize = 5, bg = "white", res = 600)
	heatmap.2(as.matrix(ParticipantCounter(path) - dm), Rowv = F, Colv = "Rowv", dendrogram = "none", 
			margin = c(3, 3), cexRow = 0.5, cexCol = 0.5, revC = F, trace = "none", key = F)
	dev.off()
}















# ClusterHeatmap: generates a cluster heatmap based on the OSM
ClusterHeatmap <- function(path) {
	
	# Read in the osm.csv file and format the row/column names
	d <- read.csv(paste(path, "osm.csv", sep = ""), header = F)
	dm <- as.matrix(d[, -1])
	dimnames(dm) <- list(d[, 1], d[, 1])
	
	# Generate the dendrogram using wards method
	cluster <- hclust(method = "ward", as.dist(ParticipantCounter(path) - dm))
	dend <- as.dendrogram(cluster)
	
	# Drawing the cluster heatmap and export as a png file
	png(filename = paste(path, "cluster_heatmap.png", sep = ""), width = 2000, height = 2000, units = "px",
			pointsize = 5, bg = "white", res = 600)
	heatmap.2(as.matrix(ParticipantCounter(path) - dm), Rowv = dend, Colv = dend, 
			margin = c(3,3), cexRow = 0.5, cexCol = 0.5, dendrogram = "both", 
			revC = T, trace = "none", key = T)
	dev.off()
}















# General cluster analysis
GeneralClusterAnalysis  <- function(path) {
	d <- read.csv(paste(path, "osm.csv", sep = ""), header = F)
	dm <- as.matrix(d[, -1])
	dimnames(dm) <- list(d[, 1], d[, 1])
	# Old code: dm = as.matrix(d)
	# Jinlong: I'm pretty sure the code above won't work for this function
	
	# Participants minus osm generates dissimilarity
	ave <- hclust(method = "average", as.dist(ParticipantCounter(path) - dm))
	comp <- hclust(method = "complete", as.dist(ParticipantCounter(path) - dm))
	ward <- hclust(method = "ward", as.dist(ParticipantCounter(path) - dm))
	
	# compute and save cophenectic matrices
	
	coph.ave <- as.matrix(cophenetic(ave)) 
	coph.comp <- as.matrix(cophenetic(comp)) 
	coph.ward <- as.matrix(cophenetic(ward)) 
	
	write.table(coph.ave, file = paste(path, "coph_matrix_ave.mtrx", sep = ""), sep = " ", row.names = F, col.names = F)
	write.table(coph.comp, file = paste(path, "coph_matrix_comp.mtrx", sep = ""), sep = " ", row.names = F, col.names = F)
	write.table(coph.ward, file = paste(path, "coph_matrix_ward.mtrx", sep = ""), sep = " ", row.names = F, col.names = F)
	
	# Export the dendrograms as a tiff files
	dend.ave <- as.dendrogram(ave)
	dend.comp <- as.dendrogram(comp)
	dend.ward <- as.dendrogram(ward)
	
	# png(filename = paste(path, "dendrogram_ave.png", sep =""),
	# 		width = 2000, height=2000, units="px",
	# 		pointsize=5, compression = "none", bg = "white", res = 600)
	png(filename = paste(path, "dendrogram_ave.png", sep =""),
			width = 2000, height=2000, units="px",
			pointsize=5, bg = "white", res = 600)
	plot(dend.ave)
	dev.off()
	
	# png(filename = paste(path, "dendrogram_comp.png", sep =""),
	# 		width = 2000, height=2000, units="px",
	# 		pointsize=5, compression = "none", bg = "white", res = 600)
	png(filename = paste(path, "dendrogram_comp.png", sep =""),
			width = 2000, height=2000, units="px",
			pointsize=5, bg = "white", res = 600)
	plot(dend.comp)
	dev.off()
	
	# png(filename = paste(path, "dendrogram_ward.png", sep =""),
	# 		width = 2000, height=2000, units="px",
	# 		pointsize=5, compression = "none", bg = "white", res = 600)
	png(filename = paste(path, "dendrogram_ward.png", sep =""),
			width = 2000, height=2000, units="px",
			pointsize=5, bg = "white", res = 600)
	plot(dend.ward)
	dev.off()
}















# Detailed cluster analysis
DetailedClusterAnalysis <- function(path, k, title = "") {
	d <- read.csv(paste(path, "osm.csv", sep = ""), header = F)
	dm <- as.matrix(d[, -1])
	dimnames(dm) <- list(d[, 1], d[, 1])
	# Old code: dm = as.matrix(d)
	# Jinlong: I'm pretty sure the code above won't work for this function
	
	# Participants minus osm generates dissimilarity
	ave <- hclust(method = "average", as.dist(ParticipantCounter(path) - dm))
	comp <- hclust(method = "complete", as.dist(ParticipantCounter(path) - dm))
	ward <- hclust(method = "ward", as.dist(ParticipantCounter(path) - dm))
	
	# load code of A2R function
	# Explain what this function is doing!
	source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
	
	# Create a color scheme from rainbow color scheme
	pre.colors <- rainbow(k)
	colors <- pre.colors[1: k]
	
	pdf(file = paste(path, "dendrograms_", k, "_cluster.pdf", sep=""),
			width = 6, height = 2.5, 
			bg = "white", pointsize = 0.5)
	
	A2Rplot(ave, k = k, boxes = F, col.up = "gray50", col.down = colors, 
			main = paste(title, " Average Linkage ", k, " clusters", sep = ""))
	A2Rplot(comp, k = k, boxes = F, col.up = "gray50", col.down = colors, 
			main = paste(title, " Complete Linkage ", k, " clusters", sep = ""))
	A2Rplot(ward, k = k, boxes = F, col.up = "gray50", col.down = colors, 
			main = paste(title, " Ward's Method ", k, " clusters", sep = ""))
	
	dev.off()
	
}
















# Cluster validation
ClusterValidation <- function(path, k, title="") {
	
	ism <- list.files(paste(path, "ism/", sep=""))
	r <- sample(1:100, size=ParticipantCounter(path), replace=TRUE)
	ism.list <- data.frame(ism, r)
	ism.list <- ism.list[order(r), ]
	
	if(ParticipantCounter(path)%%2 == 0) {
		split <- ParticipantCounter(path)/2
	} else {
		split <- (ParticipantCounter(path)-1)/2
	}
	
	# Split the participants
	group1 <- ism.list[1:split, 1]
	group2 <- ism.list[(split+1):ParticipantCounter(path), 1]
	
	# read in group1 matrix
	matrix1 <- read.delim(paste(path, "ism/", group1[1], sep=""), header=F, sep=" ", stringsAsFactors=F)
	osm1 <- data.matrix(matrix1)
	
	for (i in 2:length(group1)) {
		matrix.i <- read.delim(paste(path, "ism/", group1[i], sep=""), header=F, sep=" ", stringsAsFactors=F)
		matrix.i <- data.matrix(matrix.i)
		osm1 <- osm1 + matrix.i
	}
	
	# read in group2 matrix
	matrix2 <- read.delim(paste(path, "ism/", group2[1], sep=""), header=F, sep=" ", stringsAsFactors=F)
	osm2 <- data.matrix(matrix2)
	
	for (i in 2:length(group2)) {
		matrix.i <- read.delim(paste(path, "ism/", group2[i], sep=""), header=F, sep=" ", stringsAsFactors=F)
		matrix.i <- data.matrix(matrix.i)
		osm2 <- osm2 + matrix.i
	}
	
	d1 <- data.frame(IconListGetter(path), osm1)
	d1m <- as.matrix(d1[, -1])
	dimnames(d1m) <- list(d1[, 1], d1[, 1])
	
	d2 <- data.frame(IconListGetter(path), osm2)
	d2m <- as.matrix(d2[, -1])
	dimnames(d2m) <- list(d2[, 1], d2[, 1])
	
	ave1 <- hclust(method = "average", as.dist(ParticipantCounter(path)-d1m))
	ave2 <- hclust(method = "average", as.dist(ParticipantCounter(path)-d2m))
	
	comp1 <- hclust(method = "complete", as.dist(ParticipantCounter(path)-d1m))
	comp2 <- hclust(method = "complete", as.dist(ParticipantCounter(path)-d2m))
	
	ward1 <- hclust(method = "ward", as.dist(ParticipantCounter(path)-d1m))
	ward2 <- hclust(method = "ward", as.dist(ParticipantCounter(path)-d2m))
	
	# load code of A2R function
	source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
	
	# Define colors
	pre.colors <- rainbow(k)
	colors <- pre.colors[1:k]
	
	# colored dendrograms
	pdf(file= paste(path, "cluster_validation.pdf", sep=""),onefile=T,width=12, height=4)
	A2Rplot(ave1, k = k, boxes = FALSE, col.up = "gray50", col.down = colors, main=paste(title, " Group 1 Average Linkage", sep=""))
	A2Rplot(ave2, k = k, boxes = FALSE, col.up = "gray50", col.down = colors, main=paste(title, " Group 2 Average Linkage", sep=""))
	
	A2Rplot(comp1, k = k, boxes = FALSE, col.up = "gray50", col.down = colors, main=paste(title, " Group 1 Complete Linkage", sep=""))
	A2Rplot(comp2, k = k, boxes = FALSE, col.up = "gray50", col.down = colors, main=paste(title, " Group 2 Complete Linkage", sep=""))
	
	A2Rplot(ward1, k = k, boxes = FALSE, col.up = "gray50", col.down = colors, main=paste(title, " Group 1 Ward's Method", sep=""))
	A2Rplot(ward2, k = k, boxes = FALSE, col.up = "gray50", col.down = colors, main=paste(title, " Group 2 Ward's Method", sep=""))
	
	dev.off()
}















# Overview
# set the scenario here and file name
OverviewGetter <- function(path) {
	output <- paste(scenario.name, "_overview.pdf", sep = "")
	data <- read.csv(paste(path,"participant.csv", sep = ""), header=F, stringsAsFactors = F)
	
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
	
	
	pdf(file = paste(path, output, sep = ""), onefile = T, width=10, height=25)
	
	layout(matrix(c(1,1,2,2,3,3,4,5), 4, 2, byrow = TRUE))
	
	plot.new()
	title(paste("Total participants: ", np, ";", sep = ""), line = -18, cex = 20)
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















# Participant similarity analysis
ParticipantSimilarity <- function(path) {

	# List all ISMs
	isms <- list.files(paste(path, "ism/", sep = ""))
	all.isms <- list()
	
	np <- length(isms)
	
	# Read in all ISMs and store them in a list named all.isms
	for (i in 1:length(isms)) {
		aism <- read.delim(paste(paste(path, "ism/", sep = ""), isms[i], sep = ""),
				header = F, sep = " ", stringsAsFactors = F)
		all.isms <- c(all.isms, list(aism))
	}
	
	# Calculate participant similarity matrices of all pairs of partcipants based on the hamming distance of their ISMs (dm) and Jaccard index (dm_jaccard)
	dm <- matrix(0, ncol = np, nrow = np)
	dm.jac <- matrix(0, ncol = np, nrow = np)
	dm.rand <- matrix(0, ncol = np, nrow = np)
	for (i in 1:np) {
		for (j in 1:np) {
			dm[i, j] <- sum(abs(all.isms[[i]] - all.isms[[j]]))
			
			m11 <- sum(all.isms[[i]] * all.isms[[j]])
			m01 <- sum(abs(1-all.isms[[i]]) * all.isms[[j]])
			m10 <- sum(all.isms[[i]] * abs(1-all.isms[[j]]))
			m00 <- sum(abs(1-all.isms[[i]]) * abs(1-all.isms[[j]]))
			
			dm.jac[i, j] <- m11 / (m01+m10+m11) 
			
			dm.rand[i, j] <- (m11 + m00) / (m01+m10+m00+m11)
		}
	}
	
	# Extract the participant number of all participants and store them in a vector named names
	names <- c()
	for (i in 1:length(isms)) {
		name <- isms[i]
		names <- append(names, substr(name, 12, nchar(name) - 5))
	}
	
	# Assign participants numbers as the row&column names of the participant similarity matrix (dm)
	colnames(dm) <- names
	rownames(dm) <- names
	colnames(dm.jac) <- names
	rownames(dm.jac) <- names
	colnames(dm.rand) <- names
	rownames(dm.rand) <- names
	
	write.table(dm, file = paste(path, "participant_similarity_hamming.csv", sep = ""), sep = " ",
			row.names = T, col.names = T)
	
	write.table(dm.jac, file = paste(path, "participant_similarity_jaccard.csv", sep = ""), sep = " ",
			row.names = T, col.names = T)
	
	write.table(dm.rand, file = paste(path, "participant_similarity_rand.csv", sep = ""), sep = " ",
			row.names = T, col.names = T)
	
}

















# Visualize the frequency that each icon is being selected as group prototype
PrototypeFreq <- function(path) {
	
	# Construct the path for the zip folder and list all the zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	# Create a dataframe to store the prototype frequency
	freq <- data.frame(icon = IconListGetter(path), 
			icon_index = 0:(length(IconListGetter(path))-1), 
			count = rep(0, length(IconListGetter(path)))
	)
	
	for(p in files) {
		participant <- unzip(paste(zip.path, p, sep =""))
		participant.number <- substring(p, 1, nchar(p) - 4)
		prototype_file <- paste("./", participant.number, "/", substring(p, 1, 8), "gprototypes.csv", sep = "")
		prototype <- read.csv(prototype_file, header = F, stringsAsFactors = F)
		prev <- -1 # workaround to deal with old prototype files
		for(j in 1:nrow(prototype)) {
			if(ncol(prototype) < 4 || (!is.na(prototype[j, 2]) && !is.na(prototype[j, 3]) && !is.na(as.numeric(as.numeric(prototype[j, 3]))) && prototype[j, 2] != prev)) {
				prev <- as.numeric(prototype[j, 2])
				freq[as.numeric(prototype[j, 3]) + 1, 3] <- freq[as.numeric(prototype[j, 3]) + 1, 3] + 1
			}
		}
	}
	
	# Export batch.csv for Klipart
	write.table(freq, file = paste(path, "prototype.csv", sep = ""), sep = ",", col.names = F, row.names = F)
	return(freq)
}















# multi-dimensional scaling
MdsScaling <- function(path) {
	d <-  read.csv(paste(path, "osm.csv", sep = ""), header = F)
	dm <- as.matrix(d[, -1])
	dimnames(dm) <- list(d[, 1], d[, 1])
	dm.dist <- dist(dm, method = "euclidean")
	mds <- cmdscale(dm.dist)
	col <- rainbow(50)
	tiff(filename = paste(path, "mds.tiff", sep = ""), width = 3, height =3, units = "in", pointsize = 5, compression = "none", bg = "white", res = 600)
	plot(min(mds[, 1], mds[, 2]):max(mds[, 1],mds[, 2]), min(mds[, 1], mds[, 2]):max(mds[, 1], mds[, 2]), type = "n", xlab = "", ylab = "", main = "Multidimensional Scaling")
	for(i in 1:nrow(mds)) {
		points(mds[i, 1], mds[i, 2], type = "p", cex = 1.5)
	}
	dev.off()
	return(mds)
}














# Function list Pt. 1 - Executables
#####################################################################################################
#####################################################################################################
#####################################################################################################

ParticipantInfo(path)

prototypes <- PrototypeFreq(path)

OsmViz(path)

ClusterHeatmap(path)

GeneralClusterAnalysis(path)

OverviewGetter(path)

DescriptionGetter(path)

ParticipantSimilarity(path)
# sluggish. clogged up R. Had to kill R

mds <- MdsScaling(path)
# no text labels on plot. Just points.

ClusterValidation(path, 3, "geo terms")
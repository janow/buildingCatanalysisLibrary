## Kevin Sparks 8/12/15

# Instruction
# 1. Load the packages below (gplots, vegan, clusterval, grid, R2HTML)
# 2. Set the currecnt working directory to an experiment folder
# 3. Define a zip.path variable as a string to the location of the participants zip directory that holds all the zipped participants folders
# 4. Define a scenario.name variable as a string as the name of the experiment
# 5. Load the functions seen below
# 6. Run the EXEs at the bottom


require(gplots)
require(vegan)
require(clusteval)
require(grid)
require(R2HTML)

# Setting the basic path and scenario name variables, as well as setting the working directory to the path (this isn't necessary, just convenient).
setwd("/Users/Sparks/Google Drive/Alex/R_PackageCreation/catLibTests")
zip.path <- "/Users/Sparks/Google Drive/Alex/R_PackageCreation/catLibTests/zip"
scenario.name <- "scenario_name_here"








################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################





# These "read functions" access the local drive to read in necessary data. 
# Some of these functions are used to create variables that are useful for analysis functions.




# ParticipantCounter: count the number of participants
# Parameters
# zip.path: string, path to participants zip directory
ParticipantCounter <- function(zip.path) {

	files <- list.files(zip.path)
	
	#Get the total number of participants (zip files)
	np <- length(files)
	
	#Return the total number of participants as an integer
	return(np)
}





#IconCounter: count the number of icons(items) used in the experiment
# Parameters
# zip.path: string, path to participants zip directory
IconCounter <- function(zip.path) {

	# Checks if "/" exists after zip.path. If not, one is added
	if(substr(zip.path, nchar(zip.path), nchar(zip.path)) != "/") {
		zip.path <- paste(zip.path, "/", sep = "")
	}

	files <- list.files(zip.path)

	#Get the participant number for the first participant
	first.p.number <- substring(files[1], 1, nchar(files[1]) - 4)
	
	#Unzip the file
	first.p <- unzip(paste(zip.path, files[1], sep = ""))
	
	#Construct the full file name for icons.csv
	icons.csv <- paste("./", first.p.number, "/", substring(files[1],1,8), "icons.csv", sep = "")
	
	#Read in icons.csv
	icons <- read.csv(icons.csv, header = F)
	
	#Get the number of icons used in the experiment
	n.icons <- nrow(icons)

	#Delete the no longer needed unzipped participant folder 
	unlink(substring(paste(getwd(), "/", files[1], sep = ""), 1, nchar(paste(getwd(), "/", files[1], sep = "")) - 4), recursive = TRUE)
	
	#Return the number of icons
	return(n.icons)
}





# IconNamesGetter: get a list of icon names
# It also saves the icon.csv needed for KlipArt
# Parameters
# zip.path: string, path to participants zip directory
# scenario.name: string, name of the experiment
IconNamesGetter <- function(zip.path, scenario.name) {

	# Checks if "/" exists after zip.path. If not, one is added
	if(substr(zip.path, nchar(zip.path), nchar(zip.path)) != "/") {
		zip.path <- paste(zip.path, "/", sep = "")
	}
	
	# List all the zip files 
	files <- list.files(zip.path)
	
	# Unzip the zip file from the 1st participant
	first.p <- unzip(paste(zip.path, files[1], sep = ""))
	
	# Get the participant number for the first participant
	first.p.number <- substring(files[1], 1, nchar(files[1]) - 4)
	
	# Construct the full file name for icons.csv
	icons.csv <- paste("./", first.p.number, "/", substring(files[1],1,8), "icons.csv", sep = "")
	
	# Read in icons.csv
	icons <- read.csv(icons.csv, header = F, stringsAsFactors = F)
	
	# Reorder icon names by icon index
	icons <- icons[order(icons[, 1]), ]
	
	# Extract the icon names from the table (excluding the path name and file extensions)
	icon.list <- icons[, 2]
	for(i in 1:length(icon.list)) {
		end <- regexpr("\\.[^\\.]*$", icon.list[i])[1]
		icon.list[i] <- substr(icon.list[i], 9, end - 1)
	}
	
	# Extract the icon names with file type (e.g. .jpg) for KlipArt
	icon.list.klipart <- icons
	for(j in 1:nrow(icon.list.klipart)) {
		icon.list.klipart[j, 2] <- substr(icon.list.klipart[j, 2], 9, nchar(icon.list.klipart[j, 2]))
	}
	colnames(icon.list.klipart) <- c("index", "icon_names")
	
	# Sort the icon list by index
	icon.list.klipart <- icon.list.klipart[order(icon.list.klipart$index), ]

	# create klipart folder if not already created
	klipart.path <- paste(getwd(), "/", scenario.name, "-klipart/", sep = "")
	dir.create(klipart.path, showWarnings = FALSE)
	
	# Export the list as a csv file
	write.table(icon.list.klipart, file = paste(klipart.path, "icon.csv", sep = ""),
			sep = ",", row.names = F,  col.names = F)

	#Delete the no longer needed unzipped participant folder 
	unlink(substring(paste(getwd(), "/", files[1], sep = ""), 1, nchar(paste(getwd(), "/", files[1], sep = "")) - 4), recursive = TRUE)
	
	#Return the icon list as a vector
	return(sort(icon.list))
	
}





# ExtractIsms: Unzipps the participant folders and copies each participants individual similarity matrix into the "ism" folder (the "ism" folder is created by this function if not yet already created).
# Also writes files to klipart folder (creates a klipart folder is one is not already created)
# Parameters
# zip.path: string, path to participants zip directory
# scenario.name: string, name of the experiment
# number.of.icons: integer, the total number of icons in the experiment created by IconCounter
ExtractIsms <- function(zip.path, scenario.name, number.of.icons) {

	# Checks if "/" exists after zip.path. If not, one is added
	if(substr(zip.path, nchar(zip.path), nchar(zip.path)) != "/") {
		zip.path <- paste(zip.path, "/", sep = "")
	}

	# List all zip files
	files <- list.files(zip.path)

	# create ism folder if not already created
	ism.path <- paste(getwd(), "/ism/", sep = "")
	dir.create(ism.path, showWarnings = FALSE)

	# create klipart folder if not already created
	klipart.path <- paste(getwd(), "/", scenario.name, "-klipart/", sep = "")
	dir.create(klipart.path, showWarnings = FALSE)
	dir.create(paste(klipart.path, "matrices/", sep=""), showWarnings = FALSE)

	# create matrices folder if not already created
	# matrices.path <- paste(getwd(), "/matrices/", sep = "")
	# dir.create(matrices.path, showWarnings = FALSE)


	# Process the ISMs of the rest of participants
	for(i in 1:length(files)) {
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
		write.table(matrix.i, file = paste(ism.path, "participant", 
						substr(files[i], 1, nchar(files[i]) - 4),
						".mtrx", sep = ""), sep = " ", 
						row.names = F, col.names = F)
		
		write.table(matrix.i, file = paste(klipart.path, "matrices/", "participant", 
						substr(files[i], 1, nchar(files[i]) - 4), 
						".mtrx", sep = ""), sep = " ",
						row.names = F, col.names = F)
		
		# write.table(matrix.i, file = paste(getwd(), "/matrices/", "participant", 
		# 				substr(files[i], 1, nchar(files[i]) - 4), 
		# 				".mtrx", sep = ""), sep = " ",
		# 				row.names = F, col.names = F)

	}


	# Deletes all unzipped files as they are no longer needed
	folder.names <- substr(files, 1, nchar(files)-4)
  	for(folder in folder.names) {
    	unlink(paste(getwd(), "/", folder, sep = ""), recursive = TRUE)
  	}

}





# ReadIsms: Takes all the isms from the ism folder (the ism folder is populated via ExtractIsms) and reads them into R and returns a list of isms
# Parameters
# ism.path: string, path to the ism directory (created by ExtractIsms)
# ism.list: character vector, list of all the isms you want included (if you want to include all of them, list.files(ism.path) will work)
# number.of.icons: integer, the total number of icons in the experiment (created by IconCounter)
ReadIsms <- function(ism.path, ism.list, number.of.icons) {

  # Checks if "/" exists after ism.path. If not, one is added
  if(substr(ism.path, nchar(ism.path), nchar(ism.path)) != "/") {
    ism.path <- paste(ism.path, "/", sep = "")
  }

  # creates an empty list to store imported isms
  isms <- list()
    
  for (i in 1:length(ism.list)) {

    # gathers the current ism file name from the ism.list parameter in the for loop
    matrix.i.name <- ism.list[i]

    # reads in the current ism file
    matrix.i <- read.delim(paste(ism.path, matrix.i.name, sep=""), header = F, sep = " ", stringsAsFactors = F)

    # converts the ism file to a matrix
    matrix.i <- data.matrix(matrix.i[1:number.of.icons, ])
        
    # adds that ism matrix to the isms list created at the begining of the function
    isms[[i]] <- matrix.i
  }

  # returns a list of the isms
  return(isms)

}





################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################





################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################





# These "analysis functions" mostly take data returned from the "read functions" and run some sort of analysis with them.




# OsmGenerator: Adds all isms (created by ReadIsms) to one osm matrix.
# Parameters
# isms: list of matrices, a list of participants' isms (created by ReadIsms)
# icon.names: character vector, a list of the names of the icons (created by IconListGetter)
OsmGenerator <- function(isms, icon.names) {

  # creates an empty osm matrix
  osm <- matrix(0, nrow(isms[[1]]), ncol(isms[[1]]))
    
  # loops through all the isms in the isms parameter input
  for (ism in isms) {
    # adds each ism (in effect adding all the isms together)
    osm <- osm + ism
  }

  # defining the row and column names for the osm from the icon.list parameter
  dimnames(osm) <- list(icon.names, icon.names)

  # returns the osm matrix
  return(osm)

}





# MdsScaling: performs multi-dimensional scaling on an overall similarity matrix (created by OsmGenerator). 
# Parameters
# osm: matrix, the osm matrix created by OsmGenerator by adding all isms together
MdsScaling <- function(osm) {
	osm.dist <- dist(osm, method = "euclidean")
	mds <- cmdscale(osm.dist)
	return(mds)
	
}





# ClusterAnalysis: performs cluster analysis on a given overall similarity matrix and returns a list, where the 1st entry is a dendrogram, and the 2nd entry is a cophenectic matrix
# Parameters
# osm: matrix, the osm matrix created by OsmGenerator by adding all isms together
# number.of.participants: integer, the number of participants in the experiment (created by ParticipantCounter)
# cluster.method: string, optional parameter where the default is set to Ward's method (user has choice between options provided by the hclust function)
ClusterAnalysis  <- function(osm, number.of.participants, cluster.method="ward.D") {
	
	# Participants minus osm generates dissimilarity
	clust <- hclust(method = cluster.method, as.dist(number.of.participants - osm))
	
	# compute and save cophenectic matrices
	coph <- as.matrix(cophenetic(clust)) 
	
	# Export the dendrograms as a jpeg files
	dend.clust <- as.dendrogram(clust)

	cluster <- list()
	cluster[[1]] <- dend.clust
	cluster[[2]] <- coph

	return(cluster)

}





# ParticipantSimilarity: Participant similarity analysis
# Parameters
# isms: list of matrices, a list of participants' isms (created by ReadIsms)
# ism.list: character vector, list of all the isms you want included (if you want to include all of them, list.files(ism.path) will work)
ParticipantSimilarity <- function(isms, ism.list) {
	
	np <- length(isms)

	# Calculate participant similarity matrices of all pairs of partcipants based on the hamming distance of their ISMs (dm) and Jaccard index (dm_jaccard)
	dm <- matrix(0, ncol = np, nrow = np)
	dm.jac <- matrix(0, ncol = np, nrow = np)
	dm.rand <- matrix(0, ncol = np, nrow = np)
	for (i in 1:np) {
		for (j in 1:np) {
			dm[i, j] <- sum(abs(isms[[i]] - isms[[j]]))
			
			m11 <- sum(isms[[i]] * isms[[j]])
			m01 <- sum(abs(1-isms[[i]]) * isms[[j]])
			m10 <- sum(isms[[i]] * abs(1-isms[[j]]))
			m00 <- sum(abs(1-isms[[i]]) * abs(1-isms[[j]]))
			
			dm.jac[i, j] <- m11 / (m01+m10+m11) 
			
			dm.rand[i, j] <- (m11 + m00) / (m01+m10+m00+m11)
		}
	}
	
	# Extract the participant number of all participants and store them in a vector named names
	names <- c()
	for (i in 1:length(ism.list)) {
		name <- ism.list[i]
		names <- append(names, substr(name, 12, nchar(name) - 5))
	}
	
	# Assign participants numbers as the row&column names of the participant similarity matrix (dm)
	colnames(dm) <- names
	rownames(dm) <- names
	colnames(dm.jac) <- names
	rownames(dm.jac) <- names
	colnames(dm.rand) <- names
	rownames(dm.rand) <- names

	participant.similarity.output <- list()
	participant.similarity.output[[1]] <- dm
	participant.similarity.output[[2]] <- dm.jac
	participant.similarity.output[[3]] <- dm.rand

	return(participant.similarity.output)
	
}





################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################





################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################




# These "write functions" write newly created files to drive from analysis.



# Writes an osm to drive (usually, the osm is created by OsmGenerator)
# Parameters
# osm: matrix, osm generated by OsmGenerator
# scenario.name: string, name of the experiment
WriteOsm <- function(osm, scenario.name) {

	# create a folder to hold data outputs.
	data.path <- paste(getwd(), "DataOutputs/", sep = "/")
	dir.create(data.path, showWarnings = FALSE)

	# create klipart folder if not already created
	klipart.path <- paste(getwd(), "/", scenario.name, "-klipart/", sep = "")
	dir.create(klipart.path, showWarnings = FALSE)
	dir.create(paste(klipart.path, "matrices/", sep=""), showWarnings = FALSE)

	# create matrices folder if not already created
	# matrices.path <- paste(getwd(), "/matrices/", sep = "")
	# dir.create(matrices.path, showWarnings = FALSE)

  	# Export OSM
  	write.table(osm, file = paste(klipart.path, "matrices/", "total.mtrx", sep = ""), 
      	sep = " ", row.names = F,  col.names = F)
  
  	write.table(osm, file = paste(data.path, "osm.csv", sep = ""), 
      	sep = ",", row.names = T,  col.names = F)

}





# WriteOsmViz: generates a heatmap based on the OSM.
# No dendrograms are generated and the icons are in alphabetical order
# It is intended to be a raw heat map without dendrograms
# Parameters
# osm: matrix, osm generated by OsmGenerator
# number.of.participants: integer, the number of participants in the experiment (created by ParticipantCounter)
WriteOsmViz <- function(osm, number.of.participants) {

	# create a folder to hold viz outputs
	viz.path <- paste(getwd(), "VizOutputs/", sep = "/")
	dir.create(viz.path, showWarnings = FALSE)
	
	# Drawing the heatmap and export as a jpeg file
	jpeg(filename = paste(viz.path, "heat_map.jpeg", sep=""), width = 2000, height = 2000, units = "px",
			pointsize = 5, bg = "white", res = 700)
	heatmap.2(as.matrix(number.of.participants - osm), Rowv = F, Colv = "Rowv", dendrogram = "none", 
			margin = c(4, 4), cexRow = 0.35, cexCol = 0.35, revC = F, trace = "none", key = F)
	dev.off()

}





# WriteClusterHeatmap: generates a cluster heatmap based on the osm
# Parameters
# osm: matrix, the osm matrix created by OsmGenerator
# number.of.participants: integer, the number of participants in the experiment can be created by ParticipantCounter
# cluster.method: string, optional parameter where the default is set to Ward's method (user has choice between options provided by the hclust function)
WriteClusterHeatmap <- function(osm, number.of.participants, cluster.method="ward.D") {

	# create a folder to hold viz outputs
	viz.path <- paste(getwd(), "VizOutputs/", sep = "/")
	dir.create(viz.path, showWarnings = FALSE)
	
	# Generate the dendrogram using wards method
	cluster <- hclust(method = cluster.method, as.dist(number.of.participants - osm))
	dend <- as.dendrogram(cluster)
	
	# Drawing the cluster heatmap and export as a jpeg file
	jpeg(filename = paste(viz.path, "cluster_heatmap.jpeg", sep=""), width = 2000, height = 2000, units = "px",
			pointsize = 5, bg = "white", res = 700)
	heatmap.2(as.matrix(number.of.participants - osm), Rowv = dend, Colv = dend, 
			margin = c(4, 4), cexRow = 0.35, cexCol = 0.35, dendrogram = "both", 
			revC = T, trace = "none", key = T)
	dev.off()

}





# WriteClusterAnalysis: write the dendrogram and cophenectic matrix created from ClusterAnalysis
# Parameters
# cluster.output; list, first entry in the list is a dendrogram, the second entry is a cophenectic matrix (this list is created by ClusterAnalysis)
WriteClusterAnalysis  <- function(cluster.output) {

	# create a folder to hold viz outputs
	viz.path <- paste(getwd(), "VizOutputs/", sep = "/")
	dir.create(viz.path, showWarnings = FALSE)

	# create a folder to hold data outputs.
	data.path <- paste(getwd(), "DataOutputs/", sep = "/")
	dir.create(data.path, showWarnings = FALSE)

	jpeg(filename = paste(viz.path, "dendrogram.jpeg", sep=""),
			width = 2000, height=2000, units="px",
			pointsize=5, bg = "white", res = 600)
	par(cex=0.65)
	plot(cluster.output[[1]])
	dev.off()

	write.table(cluster.output[[2]], file = paste(data.path, "coph_matrix.mtrx", sep=""), sep = " ", row.names = F, col.names = F)

}





# WriteMdsScaling: write multi-dimensional scaling plot from MdsScaling output
# Parameters
# mds: "data type" is output from cmdscale function, output from MdsScaling function
WriteMdsScaling <- function(mds) {

	# create a folder to hold viz outputs
	viz.path <- paste(getwd(), "VizOutputs/", sep = "/")
	dir.create(viz.path, showWarnings = FALSE)

	col <- rainbow(50)
	jpeg(filename = paste(viz.path, "mds.jpeg", sep=""), width = 3, height =3, units = "in", pointsize = 5, bg = "white", res = 600)
	plot(min(mds[, 1], mds[, 2]):max(mds[, 1],mds[, 2]), min(mds[, 1], mds[, 2]):max(mds[, 1], mds[, 2]), type = "n", xlab = "", ylab = "", main = "Multidimensional Scaling")
	for(i in 1:nrow(mds)) {
		points(mds[i, 1], mds[i, 2], type = "p", cex = 1.5)
	}
	dev.off()
	
}





# WriteParticipantSimilarity: Write 3 participant similarity (hamming, jaccard, rand) files
# Parmaeters: 
# participant.similarity.output: list, a list containing participant similarity matrices output from ParticipantSimilarity
WriteParticipantSimilarity <- function(participant.similarity.output) {

	# create a folder to hold data outputs
	data.path <- paste(getwd(), "DataOutputs/", sep = "/")
	dir.create(data.path, showWarnings = FALSE)

	write.table(participant.similarity.output[[1]], file = paste(data.path, "participant_similarity_hamming.csv", sep = ""),
			row.names = T, col.names = T)
	
	write.table(participant.similarity.output[[2]], file = paste(data.path, "participant_similarity_jaccard.csv", sep = ""),
			row.names = T, col.names = T)
	
	write.table(participant.similarity.output[[3]], file = paste(data.path, "participant_similarity_rand.csv", sep = ""),
			row.names = T, col.names = T)

}





# Cluster validation
# Parameters
# ism.path: string, path to ism directory
# k: integer, number of clusters
# title: string, title of the experiment
# number.of.participants: integer, the number of participants in the experiment (created by ParticipantCounter)
# icon.names: character vector, a list of the names of the icons (created by IconListGetter)
ClusterValidation <- function(ism.path, k, title="", number.of.participants, icon.names) {

	# Checks if "/" exists after ism.path. If not, one is added
	if(substr(ism.path, nchar(ism.path), nchar(ism.path)) != "/") {
		ism.path <- paste(ism.path, "/", sep = "")
	}

	# create a folder to hold viz outputs
	viz.path <- paste(getwd(), "VizOutputs/", sep = "/")
	dir.create(viz.path, showWarnings = FALSE)
	
	ism <- list.files(ism.path)
	r <- sample(1:100, size=number.of.participants, replace=TRUE)
	ism.list <- data.frame(ism, r)
	ism.list <- ism.list[order(r), ]
	
	if(number.of.participants%%2 == 0) {
		split <- number.of.participants/2
	} else {
		split <- (number.of.participants-1)/2
	}
	
	# Split the participants
	group1 <- ism.list[1:split, 1]
	group2 <- ism.list[(split+1):number.of.participants, 1]
	
	# read in group1 matrix
	matrix1 <- read.delim(paste(ism.path, group1[1], sep=""), header=F, sep=" ", stringsAsFactors=F)
	osm1 <- data.matrix(matrix1)
	
	for (i in 2:length(group1)) {
		matrix.i <- read.delim(paste(ism.path, group1[i], sep=""), header=F, sep=" ", stringsAsFactors=F)
		matrix.i <- data.matrix(matrix.i)
		osm1 <- osm1 + matrix.i
	}
	
	# read in group2 matrix
	matrix2 <- read.delim(paste(ism.path, group2[1], sep=""), header=F, sep=" ", stringsAsFactors=F)
	osm2 <- data.matrix(matrix2)
	
	for (i in 2:length(group2)) {
		matrix.i <- read.delim(paste(ism.path, group2[i], sep=""), header=F, sep=" ", stringsAsFactors=F)
		matrix.i <- data.matrix(matrix.i)
		osm2 <- osm2 + matrix.i
	}
	
	d1 <- data.frame(icon.names, osm1)
	d1m <- as.matrix(d1[, -1])
	dimnames(d1m) <- list(d1[, 1], d1[, 1])
	
	d2 <- data.frame(icon.names, osm2)
	d2m <- as.matrix(d2[, -1])
	dimnames(d2m) <- list(d2[, 1], d2[, 1])
	
	ave1 <- hclust(method = "average", as.dist(number.of.participants-d1m))
	ave2 <- hclust(method = "average", as.dist(number.of.participants-d2m))
	
	comp1 <- hclust(method = "complete", as.dist(number.of.participants-d1m))
	comp2 <- hclust(method = "complete", as.dist(number.of.participants-d2m))
	
	ward1 <- hclust(method = "ward.D", as.dist(number.of.participants-d1m))
	ward2 <- hclust(method = "ward.D", as.dist(number.of.participants-d2m))
	
	# load code of A2R function
	source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
	
	# Define colors
	pre.colors <- rainbow(k)
	colors <- pre.colors[1:k]
	
	# colored dendrograms
	pdf(file= paste(viz.path, "cluster_validation.pdf", sep=""),onefile=T,width=12, height=4)
	A2Rplot(ave1, k = k, boxes = FALSE, col.up = "gray50", col.down = colors, main=paste(title, " Group 1 Average Linkage", sep=""))
	A2Rplot(ave2, k = k, boxes = FALSE, col.up = "gray50", col.down = colors, main=paste(title, " Group 2 Average Linkage", sep=""))
	
	A2Rplot(comp1, k = k, boxes = FALSE, col.up = "gray50", col.down = colors, main=paste(title, " Group 1 Complete Linkage", sep=""))
	A2Rplot(comp2, k = k, boxes = FALSE, col.up = "gray50", col.down = colors, main=paste(title, " Group 2 Complete Linkage", sep=""))
	
	A2Rplot(ward1, k = k, boxes = FALSE, col.up = "gray50", col.down = colors, main=paste(title, " Group 1 Ward's Method", sep=""))
	A2Rplot(ward2, k = k, boxes = FALSE, col.up = "gray50", col.down = colors, main=paste(title, " Group 2 Ward's Method", sep=""))
	
	dev.off()
}





# PrototypeFreq: visualize the frequency that each icon is being selected as group prototype
# Parameters
# zip.path: string, path to participants zip directory
# icon.names: character vector, a list of the names of the icons (created by IconListGetter)
PrototypeFreq <- function(zip.path, icon.names) {

	# Checks if "/" exists after zip.path. If not, one is added
	if(substr(zip.path, nchar(zip.path), nchar(zip.path)) != "/") {
		zip.path <- paste(zip.path, "/", sep = "")
	}

	# create a folder to hold data outputs
	data.path <- paste(getwd(), "DataOutputs/", sep = "/")
	dir.create(data.path, showWarnings = FALSE)
	
	# List all the zip files
	files <- list.files(zip.path)
	
	# Create a dataframe to store the prototype frequency
	freq <- data.frame(icon = icon.names, 
			icon_index = 0:(length(icon.names)-1), 
			count = rep(0, length(icon.names))
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
	write.table(freq, file = paste(data.path, "prototype.csv", sep=""), sep = ",", col.names = F, row.names = F)

	# Deletes all unzipped files as they are no longer needed
	folder.names <- substr(files, 1, nchar(files)-4)
  	for(folder in folder.names) {
    	unlink(paste(getwd(), "/", folder, sep = ""), recursive = TRUE)
  	}

  	return(freq)
  	
}





# WriteAssignment: generate the assignment.csv for KlipArt
# Parameters
# zip.path: string, path to participants zip directory
# scenario.name: string, name of the experiment
WriteAssignment <- function(zip.path, scenario.name) {

	# Checks if "/" exists after zip.path. If not, one is added
	if(substr(zip.path, nchar(zip.path), nchar(zip.path)) != "/") {
		zip.path <- paste(zip.path, "/", sep = "")
	}

	# Create an empty dataframe
	df <- data.frame()
	
	# List all zip files
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

	# create klipart folder if not already created
	klipart.path <- paste(getwd(), "/", scenario.name, "-klipart/", sep = "")
	dir.create(klipart.path, showWarnings = FALSE)

	# Export the assignment.csv
	write.table(df, file = paste(klipart.path, "assignment.csv", sep = ""),
			sep = ",", row.names = F,  col.names = F)

	# Deletes all unzipped files as they are no longer needed
	folder.names <- substr(files, 1, nchar(files)-4)
  	for(folder in folder.names) {
    	unlink(paste(getwd(), "/", folder, sep = ""), recursive = TRUE)
  	}

}





# WriteParticipantInfo: collect demographic info and basic experiment info (# of groups created and time spent in seconds)
# Parameters
# zip.path: string, path to participants zip directory
# scenario.name: string, name of the experiment
WriteParticipantInfo <- function(zip.path, scenario.name) {
	
	# Checks if "/" exists after zip.path. If not, one is added
	if(substr(zip.path, nchar(zip.path), nchar(zip.path)) != "/") {
		zip.path <- paste(zip.path, "/", sep = "")
	}

	# create a folder to hold data outputs
	data.path <- paste(getwd(), "DataOutputs/", sep = "/")
	dir.create(data.path, showWarnings = FALSE)

	# List all zip files
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
	write.table(demographic, file = paste(data.path, "participant.csv", sep = ""),
			sep = ",", row.names = F,  col.names = F)

	# create klipart folder if not already created
	klipart.path <- paste(getwd(), "/", scenario.name, "-klipart/", sep = "")
	dir.create(klipart.path, showWarnings = FALSE)
	
	# Export the participant.csv for KlipArt
	write.table(demographic, file = paste(klipart.path, "participant.csv", sep = ""), sep = ",", row.names = F,  col.names = F)

	# Deletes all unzipped files as they are no longer needed
	folder.names <- substr(files, 1, nchar(files)-4)
  	for(folder in folder.names) {
    	unlink(paste(getwd(), "/", folder, sep = ""), recursive = TRUE)
  	}

}




# WriteDescription: extract the linguistic labels (both long and short) from all participants and store in a single csv file
# Parameters
# zip.path: string, path to participants zip directory
# scenario.name: string, name of the experiment
WriteDescription <- function(zip.path, scenario.name) {

	# Checks if "/" exists after zip.path. If not, one is added
	if(substr(zip.path, nchar(zip.path), nchar(zip.path)) != "/") {
		zip.path <- paste(zip.path, "/", sep = "")
	}
	
	# create a folder to hold data outputs
	data.path <- paste(getwd(), "DataOutputs/", sep = "/")
	dir.create(data.path, showWarnings = FALSE)

	# List all the zip files
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
	write.table(description, file = paste(data.path, "description.csv", sep = ""), 
			sep = ",", row.names = F,  col.names = F)
	
	# create klipart folder if not already created
	klipart.path <- paste(getwd(), "/", scenario.name, "-klipart/", sep = "")
	dir.create(klipart.path, showWarnings = FALSE)

	# Export batch.csv for Klipart
	write.table(description, file=paste(klipart.path, "batch.csv", sep = ""), sep = ",", col.names=  F, row.names = F)

	# Deletes all unzipped files as they are no longer needed
	folder.names <- substr(files, 1, nchar(files)-4)
  	for(folder in folder.names) {
    	unlink(paste(getwd(), "/", folder, sep = ""), recursive = TRUE)
  	}

}





# WriteOverview: produces an overall report of the general information from the experiment 
# Parameters
# scenario.name: string, name of the experiment
# participant.info.path: string, full path to the participant.csv created by WriteParticipantInfo
# number.of.participants: integer, the number of participants in the experiment (created by ParticipantCounter)
WriteOverview <- function(scenario.name, participant.info.path, number.of.participants) {
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





# Visualize row values for the entire OSM
# MeanVarianceGraphics: visualize row values for the entire OSM
# Parameters
# osm: matrix, the osm matrix created by OsmGenerator
# icon.names: character vector, a list of the names of the icons (created by IconListGetter)
# icons.path: string, path to icons directory that holds the experiment icon files
MeanVarianceGraphics <- function(Osm, icon.names, icons.path) {

  # create a folder to hold viz outputs
	viz.path <- paste(getwd(), "VizOutputs/", sep = "/")
	dir.create(viz.path, showWarnings = FALSE)

  # create a folder to hold individual graphics inside viz outputs folder
  graphics.path <- paste(viz.path, "MeanVarianceGraphics/", sep = "")
  dir.create(graphics.path, showWarnings = FALSE)

  myOSM <- as.data.frame(Osm)

  ## Calculate the mean and variance for each row. Store in data frame.
  myOSM$Mean <- apply(myOSM[1:nrow(Osm)],1,mean,na.rm=TRUE) #mean for each row, add to new column "Mean"
  myOSM$Variance <- apply(myOSM[1:nrow(Osm)],1,var,na.rm=TRUE)

  # Icon names are in row 1. They are stored in myLabels
  myLabels <- as.vector(icon.names)
  myMean <- round(myOSM[,nrow(Osm)+1], 2)
  myVar <- round(myOSM[,nrow(Osm)+2], 2)

  # Visualize row data for each row
  for (i in 1:nrow(Osm)) {
    lab <- i
    png(file = paste(graphics.path, myLabels[lab], "_RF", ".png", sep=""),
        width = 300, height = 300, pointsize = 10)
    subT = paste(myMean[lab], myVar[lab], sep = '//')
    barplot(myOSM[,i], main = paste(myLabels[lab], subT, sep = ': '), cex.main = 1.5 )
    dev.off()

  }

  # Create bar plots for the lower quartile
  my25 <- quantile(myVar, .25)
  distinct25 <- c()
  png(file = paste(graphics.path, "groupFreq_25", ".png", sep=""), width = 1200, height = 1200, pointsize = 12)
  # Figures out the proper "par" parameters by calculating how many icons would be in a quartile, then taking the square root to determine how large the window needs to be 
  par(mfrow=c(ceiling((nrow(Osm)/4)^(1/2)),ceiling((nrow(Osm)/4)^(1/2))))
  for(i in 1:nrow(Osm)) {
    lab <- i
    if (myVar[lab] < my25) {
      distinct25 <- append(distinct25, myLabels[lab])
      subT = paste(myMean[lab], myVar[lab], sep = '//')
      barplot(myOSM[,i], main = paste(myLabels[lab], subT, sep = ': '), cex.main = 1.5 )
    }
  }
  dev.off()

  # Create bar plots for upper quartile
  # Set upper quartile
  my75 <- quantile(myVar, .75)
  distinct75 <- c()
  png(file = paste(graphics.path, "groupFreq_75", ".png", sep=""), width = 1200, height = 1200, pointsize = 12)
  # Figures out the proper "par" parameters by calculating how many icons would be in a quartile, then taking the square root to determine how large the window needs to be 
  par(mfrow=c(ceiling((nrow(Osm)/4)^(1/2)),ceiling((nrow(Osm)/4)^(1/2))))
  for(i in 1:nrow(Osm)) {
    lab <- i
    if (myVar[lab] > my75) {
      distinct75 <- append(distinct75, myLabels[lab])
      subT = paste(myMean[lab], myVar[lab], sep = '//')
      barplot(myOSM[,i], main = paste(myLabels[lab], subT, sep = ': '), cex.main = 1.5 )
    }
  }
  dev.off()


  # Find out what the extension is for the icons (i.e. ".png", ".jpeg", ".gif")
  icon.names <- list.files(icons.path)
  icon.extension <- strsplit(icon.names[1], "\\.")[[1]][2]
  icon.extension <- paste(".", icon.extension, sep="")

  # Make HMTL file with lower and upper quantile images
  #define output file
  output <- "LowerQuantiles25.html"
  HTMLoutput=file.path(graphics.path, output)
  for (i in distinct25) {
    HTMLInsertGraph(GraphFileName= paste(icons.path, i, icon.extension, sep = ""), file=HTMLoutput, Caption=i, Align="center", WidthHTML=200, HeightHTML=NULL)
  }

  output <- "UpperQuantiles75.html"
  HTMLoutput=file.path(graphics.path, output)
  for (i in distinct75) { 
    HTMLInsertGraph(GraphFileName= paste(icons.path, i, icon.extension, sep = ""), file=HTMLoutput, Caption=i, Align="center", WidthHTML=200, HeightHTML=NULL)
  }

}





################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################





## EXE
number.of.participants <- ParticipantCounter(zip.path)

number.of.icons <- IconCounter(zip.path)

icon.names <- IconNamesGetter(zip.path, scenario.name)

ExtractIsms(zip.path, scenario.name, number.of.icons)

isms <- ReadIsms(paste(getwd(), "/ism", sep=""), list.files(paste(getwd(), "/ism", sep="")), number.of.icons)

Osm <- OsmGenerator(isms, icon.names)

mds <- MdsScaling(Osm)

cluster.output <- ClusterAnalysis(Osm, number.of.participants)

participant.similarity.output <- ParticipantSimilarity(isms, list.files(paste(getwd(), "/ism", sep="")))

WriteOsm(Osm, scenario.name)

WriteOsmViz(Osm, number.of.participants)

WriteClusterHeatmap(Osm, number.of.participants)

WriteClusterAnalysis(cluster.output)

WriteMdsScaling(mds)

WriteParticipantSimilarity(participant.similarity.output)

ClusterValidation(paste(getwd(), "/ism/", sep=""), 3, "geo terms", number.of.participants, icon.names)

PrototypeFreq.output <- PrototypeFreq(zip.path, icon.names)

WriteAssignment(zip.path, scenario.name)

WriteParticipantInfo(zip.path, scenario.name)

WriteDescription(zip.path, scenario.name)

WriteOverview(scenario.name, paste(getwd(), "/DataOutputs", "/participant.csv", sep=""), number.of.participants)

MeanVarianceGraphics(Osm, icon.names, paste(getwd(), "/icons/", sep=""))

# OsmViz: generates a heatmap based on the OSM.
# No dendrograms are generated and the icons are in alphabetical order
# It is intended to be a raw heat map without dendrograms
OsmViz <- function(osm.path, number.of.participants) {
	
	# Read in the osm.csv file and format the row/column names
	d <- read.csv(osm.path, header = F)
	dm <- as.matrix(d[, -1])
	dimnames(dm) <- list(d[, 1], d[, 1])
	
	# Drawing the heatmap and export as a jpeg file
	jpeg(filename = "heat_map.jpeg", width = 2000, height = 2000, units = "px",
			pointsize = 5, bg = "white", res = 700)
	heatmap.2(as.matrix(number.of.participants - dm), Rowv = F, Colv = "Rowv", dendrogram = "none", 
			margin = c(4, 4), cexRow = 0.35, cexCol = 0.35, revC = F, trace = "none", key = F)
	dev.off()
}

OsmViz(paste(path, "osm.csv", sep=""), number.of.participants)





# ClusterHeatmap: generates a cluster heatmap based on the OSM
ClusterHeatmap <- function(osm.path, number.of.participants) {
	
	# Read in the osm.csv file and format the row/column names
	d <- read.csv(osm.path, header = F)
	dm <- as.matrix(d[, -1])
	dimnames(dm) <- list(d[, 1], d[, 1])
	
	# Generate the dendrogram using wards method
	cluster <- hclust(method = "ward", as.dist(number.of.participants - dm))
	dend <- as.dendrogram(cluster)
	
	# Drawing the cluster heatmap and export as a jpeg file
	jpeg(filename = "cluster_heatmap.jpeg", width = 2000, height = 2000, units = "px",
			pointsize = 5, bg = "white", res = 700)
	heatmap.2(as.matrix(number.of.participants - dm), Rowv = dend, Colv = dend, 
			margin = c(4, 4), cexRow = 0.35, cexCol = 0.35, dendrogram = "both", 
			revC = T, trace = "none", key = T)
	dev.off()
}

ClusterHeatmap(paste(path, "osm.csv", sep=""), number.of.participants)





# General cluster analysis
GeneralClusterAnalysis  <- function(osm.path, number.of.participants) {
	d <- read.csv(osm.path, header = F)
	dm <- as.matrix(d[, -1])
	dimnames(dm) <- list(d[, 1], d[, 1])
	# Old code: dm = as.matrix(d)
	# Jinlong: I'm pretty sure the code above won't work for this function
	
	# Participants minus osm generates dissimilarity
	ave <- hclust(method = "average", as.dist(number.of.participants - dm))
	comp <- hclust(method = "complete", as.dist(number.of.participants - dm))
	ward <- hclust(method = "ward", as.dist(number.of.participants - dm))
	
	# compute and save cophenectic matrices
	
	coph.ave <- as.matrix(cophenetic(ave)) 
	coph.comp <- as.matrix(cophenetic(comp)) 
	coph.ward <- as.matrix(cophenetic(ward)) 
	
	write.table(coph.ave, file = "coph_matrix_ave.mtrx", sep = " ", row.names = F, col.names = F)
	write.table(coph.comp, file = "coph_matrix_comp.mtrx", sep = " ", row.names = F, col.names = F)
	write.table(coph.ward, file = "coph_matrix_ward.mtrx", sep = " ", row.names = F, col.names = F)
	
	# Export the dendrograms as a jpeg files
	dend.ave <- as.dendrogram(ave)
	dend.comp <- as.dendrogram(comp)
	dend.ward <- as.dendrogram(ward)
	
	# png(filename = paste(path, "dendrogram_ave.png", sep =""),
	# 		width = 2000, height=2000, units="px",
	# 		pointsize=5, compression = "none", bg = "white", res = 600)
	jpeg(filename = "dendrogram_ave.jpeg",
			width = 2000, height=2000, units="px",
			pointsize=5, bg = "white", res = 600)
	plot(dend.ave)
	dev.off()
	
	# png(filename = paste(path, "dendrogram_comp.png", sep =""),
	# 		width = 2000, height=2000, units="px",
	# 		pointsize=5, compression = "none", bg = "white", res = 600)
	jpeg(filename = "dendrogram_comp.jpeg",
			width = 2000, height=2000, units="px",
			pointsize=5, bg = "white", res = 600)
	plot(dend.comp)
	dev.off()
	
	# png(filename = paste(path, "dendrogram_ward.png", sep =""),
	# 		width = 2000, height=2000, units="px",
	# 		pointsize=5, compression = "none", bg = "white", res = 600)
	jpeg(filename = "dendrogram_ward.jpeg",
			width = 2000, height=2000, units="px",
			pointsize=5, bg = "white", res = 600)
	plot(dend.ward)
	dev.off()
}

GeneralClusterAnalysis(paste(path, "osm.csv", sep=""), number.of.participants)





# Participant similarity analysis
ParticipantSimilarity <- function(ism.path) {

	# Checks if "/" exists after path. If not, one is added
	if(substr(ism.path, nchar(ism.path), nchar(ism.path)) != "/") {
		ism.path <- paste(ism.path, "/", sep = "")
	}

	# List all ISMs
	isms <- list.files(ism.path)
	all.isms <- list()
	
	np <- length(isms)
	
	# Read in all ISMs and store them in a list named all.isms
	for (i in 1:length(isms)) {
		aism <- read.delim(paste(ism.path, isms[i], sep = ""),
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
	
	write.table(dm, file = "participant_similarity_hamming.csv", sep = " ",
			row.names = T, col.names = T)
	
	write.table(dm.jac, file = "participant_similarity_jaccard.csv", sep = " ",
			row.names = T, col.names = T)
	
	write.table(dm.rand, file = "participant_similarity_rand.csv", sep = " ",
			row.names = T, col.names = T)
}

ParticipantSimilarity(paste(path, , sep=""))




# multi-dimensional scaling
MdsScaling <- function(osm.path) {
	d <-  read.csv(osm.path, header = F)
	dm <- as.matrix(d[, -1])
	dimnames(dm) <- list(d[, 1], d[, 1])
	dm.dist <- dist(dm, method = "euclidean")
	mds <- cmdscale(dm.dist)
	col <- rainbow(50)
	jpeg(filename = "mds.jpeg", width = 3, height =3, units = "in", pointsize = 5, bg = "white", res = 600)
	plot(min(mds[, 1], mds[, 2]):max(mds[, 1],mds[, 2]), min(mds[, 1], mds[, 2]):max(mds[, 1], mds[, 2]), type = "n", xlab = "", ylab = "", main = "Multidimensional Scaling")
	for(i in 1:nrow(mds)) {
		points(mds[i, 1], mds[i, 2], type = "p", cex = 1.5)
	}
	dev.off()
	return(mds)
}

MdsScaling(osm.path)




# Visualize the frequency that each icon is being selected as group prototype
PrototypeFreq <- function(path, icon.list) {

	# Checks if "/" exists after path. If not, one is added
	if(substr(path, nchar(path), nchar(path)) != "/") {
		path <- paste(path, "/", sep = "")
	}
	
	# Construct the path for the zip folder and list all the zip files
	zip.path <- paste(path, "zip/", sep = "")
	files <- list.files(zip.path)
	
	# Create a dataframe to store the prototype frequency
	freq <- data.frame(icon = icon.list, 
			icon_index = 0:(length(icon.list)-1), 
			count = rep(0, length(icon.list))
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
	write.table(freq, file = "prototype.csv", sep = ",", col.names = F, row.names = F)
	return(freq)
}

PrototypeFreq(path, icon.list)




# Cluster validation
ClusterValidation <- function(path, k, title="", number.of.participants) {
	
	ism <- list.files(paste(path, "ism/", sep=""))
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
	
	ave1 <- hclust(method = "average", as.dist(number.of.participants-d1m))
	ave2 <- hclust(method = "average", as.dist(number.of.participants-d2m))
	
	comp1 <- hclust(method = "complete", as.dist(number.of.participants-d1m))
	comp2 <- hclust(method = "complete", as.dist(number.of.participants-d2m))
	
	ward1 <- hclust(method = "ward", as.dist(number.of.participants-d1m))
	ward2 <- hclust(method = "ward", as.dist(number.of.participants-d2m))
	
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

ClusterValidation(path, 3, "geo terms", number.of.participants)



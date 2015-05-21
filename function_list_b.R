# # OsmViz: generates a heatmap based on the OSM.
# # No dendrograms are generated and the icons are in alphabetical order
# # It is intended to be a raw heat map without dendrograms
# OsmViz <- function(path, osm.path) {
	
# 	# Read in the osm.csv file and format the row/column names
# 	d <- read.csv(osm.path, header = F)
# 	dm <- as.matrix(d[, -1])
# 	dimnames(dm) <- list(d[, 1], d[, 1])
	
# 	# Drawing the heatmap and export as a jpeg file
# 	jpeg(filename = paste(path, "heat_map.jpeg", sep = ""),width = 2000, height = 2000, units = "px",
# 			pointsize = 5, bg = "white", res = 700)
# 	heatmap.2(as.matrix(ParticipantCounter(path) - dm), Rowv = F, Colv = "Rowv", dendrogram = "none", 
# 			margin = c(4, 4), cexRow = 0.35, cexCol = 0.35, revC = F, trace = "none", key = F)
# 	dev.off()
# }
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




# # ClusterHeatmap: generates a cluster heatmap based on the OSM
# ClusterHeatmap <- function(path, osm.path) {
	
# 	# Read in the osm.csv file and format the row/column names
# 	d <- read.csv(osm.path, header = F)
# 	dm <- as.matrix(d[, -1])
# 	dimnames(dm) <- list(d[, 1], d[, 1])
	
# 	# Generate the dendrogram using wards method
# 	cluster <- hclust(method = "ward", as.dist(ParticipantCounter(path) - dm))
# 	dend <- as.dendrogram(cluster)
	
# 	# Drawing the cluster heatmap and export as a jpeg file
# 	jpeg(filename = paste(path, "cluster_heatmap.jpeg", sep = ""), width = 2000, height = 2000, units = "px",
# 			pointsize = 5, bg = "white", res = 700)
# 	heatmap.2(as.matrix(ParticipantCounter(path) - dm), Rowv = dend, Colv = dend, 
# 			margin = c(4, 4), cexRow = 0.35, cexCol = 0.35, dendrogram = "both", 
# 			revC = T, trace = "none", key = T)
# 	dev.off()
# }
# ClusterHeatmap: generates a cluster heatmap based on the OSM
ClusterHeatmap <- function(osm.path, number.of.participants) {
	
	# Read in the osm.csv file and format the row/column names
	d <- read.csv(osm.path, header = F)
	dm <- as.matrix(d[, -1])
	dimnames(dm) <- list(d[, 1], d[, 1])
	
	# Generate the dendrogram using wards method
	cluster <- hclust(method = "ward", as.dist(ParticipantCounter(path) - dm))
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




# # General cluster analysis
# GeneralClusterAnalysis  <- function(path, osm.path) {
# 	d <- read.csv(osm.path, header = F)
# 	dm <- as.matrix(d[, -1])
# 	dimnames(dm) <- list(d[, 1], d[, 1])
# 	# Old code: dm = as.matrix(d)
# 	# Jinlong: I'm pretty sure the code above won't work for this function
	
# 	# Participants minus osm generates dissimilarity
# 	ave <- hclust(method = "average", as.dist(ParticipantCounter(path) - dm))
# 	comp <- hclust(method = "complete", as.dist(ParticipantCounter(path) - dm))
# 	ward <- hclust(method = "ward", as.dist(ParticipantCounter(path) - dm))
	
# 	# compute and save cophenectic matrices
	
# 	coph.ave <- as.matrix(cophenetic(ave)) 
# 	coph.comp <- as.matrix(cophenetic(comp)) 
# 	coph.ward <- as.matrix(cophenetic(ward)) 
	
# 	write.table(coph.ave, file = paste(path, "coph_matrix_ave.mtrx", sep = ""), sep = " ", row.names = F, col.names = F)
# 	write.table(coph.comp, file = paste(path, "coph_matrix_comp.mtrx", sep = ""), sep = " ", row.names = F, col.names = F)
# 	write.table(coph.ward, file = paste(path, "coph_matrix_ward.mtrx", sep = ""), sep = " ", row.names = F, col.names = F)
	
# 	# Export the dendrograms as a jpeg files
# 	dend.ave <- as.dendrogram(ave)
# 	dend.comp <- as.dendrogram(comp)
# 	dend.ward <- as.dendrogram(ward)
	
# 	# png(filename = paste(path, "dendrogram_ave.png", sep =""),
# 	# 		width = 2000, height=2000, units="px",
# 	# 		pointsize=5, compression = "none", bg = "white", res = 600)
# 	jpeg(filename = paste(path, "dendrogram_ave.jpeg", sep =""),
# 			width = 2000, height=2000, units="px",
# 			pointsize=5, bg = "white", res = 600)
# 	plot(dend.ave)
# 	dev.off()
	
# 	# png(filename = paste(path, "dendrogram_comp.png", sep =""),
# 	# 		width = 2000, height=2000, units="px",
# 	# 		pointsize=5, compression = "none", bg = "white", res = 600)
# 	jpeg(filename = paste(path, "dendrogram_comp.jpeg", sep =""),
# 			width = 2000, height=2000, units="px",
# 			pointsize=5, bg = "white", res = 600)
# 	plot(dend.comp)
# 	dev.off()
	
# 	# png(filename = paste(path, "dendrogram_ward.png", sep =""),
# 	# 		width = 2000, height=2000, units="px",
# 	# 		pointsize=5, compression = "none", bg = "white", res = 600)
# 	jpeg(filename = paste(path, "dendrogram_ward.jpeg", sep =""),
# 			width = 2000, height=2000, units="px",
# 			pointsize=5, bg = "white", res = 600)
# 	plot(dend.ward)
# 	dev.off()
# }
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

ParticipantSimilarity(ism.path)




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




# Overview
# set the scenario here and file name
OverviewGenerator <- function(scenario.name, participant_info.path, number.of.participants) {
	output <- paste(scenario.name, "_overview.pdf", sep = "")
	data <- read.csv(participant_info.path, header=F, stringsAsFactors = F)
	
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

OverviewGenerator(scenario.name, paste(path, "participant.csv", sep=""), number.of.participants)


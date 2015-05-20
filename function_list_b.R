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







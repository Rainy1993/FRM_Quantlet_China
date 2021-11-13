rm(list = ls(all = TRUE))

wdir = "/Users/annshchekina/Desktop/Kod/FRM_All"

plot_labels = c(20190603, 20200101, 20200601)
channel = "EM"
tau = 0.50
s = 63

#Plot parameter defined based on the outliers
if (channel == "Crypto") {
  lambda_cutoff = 0.1359
  M_macro = 5} else 
if (channel == "EM") {
  lambda_cutoff = 10
  M_macro = 12}

setwd(wdir)

if (tau == 0.05) input_path = paste0("Output/", channel) else 
  input_path = paste0("Output/", channel, "/Sensitivity/tau=", 100*tau, "/s=", s)
output_path = input_path 

#Check if package is installed, if not: install, either way: load 
if (!require(igraph)) install.packages("igraph"); library(igraph)
if (!require(qgraph)) install.packages("graph"); library(qgraph)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(gganimate)) install.packages("gganimate"); library(gganimate)
if (!require(av)) install.packages('av'); library(av)
if (!require(gifski)) install.packages("gifski"); library(gifski)
if (!require(strex)) install.packages("strex"); library(strex)
if (!require(magick)) install.packages("magick"); library(magick)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(matrixStats)) install.packages("matrixStats"); library(matrixStats)
if (!require(tools)) install.packages("tools"); library(tools)

#Set ggplot2 theme
theme_set(theme_classic())

#List of centrality types and their numbers
centralitylist = list("OutDegree" = 1, "InDegree" = 2, "Closeness" = 3, 
                      "Betweenness" = 4, "InInfluence" = 5, "OutInfluence" = 6)

#Read historical FRM index
FRM_index = read.csv(paste0(input_path, "/Lambda/FRM_", channel, "_index.csv"))


## Calculate centralities

#Create a list of files in the folder and extract dates from the names
file_list = list.files(path = paste0(input_path, "/Adj_Matrices"))
file_list = file_list[file_list!="Fixed"]
dates = as.character(str_first_number(file_list), format = "%Y%m%d")
dates = as.Date(dates, format = "%Y%m%d")
N = length(file_list)

#Create a list of all network graphs
allgraphs = lapply(1:N, function(i) {
  data = read.csv(paste0(input_path, "/Adj_Matrices/", file_list[i]), row.names = 1)
  M_stock = ncol(data)-M_macro
  adj_matrix = data.matrix(data[1:M_stock, 1:M_stock])
  q = qgraph(adj_matrix, layout = "circle", details = TRUE, 
                                   vsize = c(5,15), DoNotPlot = TRUE)
  return(q)
})

allcentralities = centrality(allgraphs)
eigencentrality = lapply(1:N, function(i) eigen_centrality(as.igraph(allgraphs[[i]]))$vector)

#Calculate averages
outdegree_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$OutDegree))
indegree_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$InDegree))
closeness_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$Closeness))
betweenness_avg = sapply(1:N, function(i) mean(allcentralities[[i]]$Betweenness))
eigenvector_avg = sapply(1:N, function(i) mean(eigencentrality[[i]]))

#Restructure list into individual node centralities



outliers = which(FRM_index$FRM > lambda_cutoff)


## Plot FRM index vs all centralities

cent_plot = function(cent_type, lambda) {
  cent_string = deparse(substitute(cent_type))
  png(paste0(output_path, "/Centrality/FRM_", cent_string,".png"), 
      width = 900, height = 600, bg = "transparent")
  par(mar = c(5, 4, 4, 4) + 0.3)
  plot(lambda[-outliers], type = "l", col = "blue", xlab = "", 
       ylab = "FRM index", xaxt = "n", lwd = 2)
  par(new = TRUE)
  plot(cent_type[-outliers], type = "l", col = "red", axes = FALSE, 
       xlab = "", ylab = "", xaxt = "n")
  axis(side = 4, at = pretty(range(cent_type[-outliers])))
  ll = which(FRM_index$date[-outliers] %in% plot_labels)
  axis(1, at = ll, labels = plot_labels)
  mtext(paste0(gsub("_.*", "", cent_string) %>% toTitleCase, " centrality"), 
        side = 4, line = 3)
  dev.off()
}

cent_plot(outdegree_avg, FRM_index$FRM)
cent_plot(indegree_avg, FRM_index$FRM)
cent_plot(betweenness_avg, FRM_index$FRM)
cent_plot(closeness_avg, FRM_index$FRM)
cent_plot(eigenvector_avg, FRM_index$FRM)




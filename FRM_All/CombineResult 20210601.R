rm(list = ls(all = TRUE))

libraries = c("xlsx","scales","ggplot2","dplyr","gcookbook")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#----------------------------------------START UPDATE----------------------------------------

wdir = "/Users/wkhaerdle/Documents/Project/FRM-master_Modify_WRT/FRM_Quantlet/FRM_All/"
                     
setwd(wdir)
output_path = "Output/Asia/Adj_Matrices_test"

# result1 = read.csv(file = paste0(output_path, "/MarcoScores.csv"), header = TRUE) 
# result1 = result1[result1$Date<20190717,]
# 
# result2 = read.csv(file = paste0(output_path, "/MarcoScores_2.csv"), header = TRUE) 
# result2 = result2[result2$Date>=20190717,]
# 
# result = rbind(result1, result2)
# write.xlsx(result, paste0(output_path, "/MarcoScores.xlsx"), sheetName="Sheet1",
           # col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)
Date = 20210601
tau = 0.25
Type = "Shapley"
name = paste0("MarcoScores_Quantile_",Type,"_") # "MarcoScores""MarcoScores_Quantile_shapley_"
result = read.csv(file = paste0(output_path, "/",name, tau,"_",Date,".csv"), header = TRUE) 
result$Date = as.Date(paste0(floor(result$Date / 10000),"-",
                      floor(result$Date / 100) -floor(result$Date / 10000)*100,"-",
                      result$Date - floor(result$Date / 100) * 100))

# plot each curve
Type = names(result)
Column = intersect(which(Type!="X"), which(Type!="Date"))
datebreaks <- seq(result$Date[1], result$Date[length(result$Date)],
                  by = "1 month")
     
for (iCol in Column){
  name_col = Type[iCol]
  exec <- parse(text = paste0('ggplot(result, aes(x = Date, y = ', name_col,')) +  geom_line()'))
  econ_plot <- eval(exec)
  
  png(paste0(output_path, "/quantile/",name,tau,"_", name_col, "_",Date,".png"), width = 900, height = 600, bg = "transparent")
  
  print(econ_plot +
    scale_x_date(breaks = datebreaks) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          axis.line = element_line(colour = "black")))
  dev.off()
}

# top 3 index
Top3_Result = result$Date
Temp_M = data.frame(matrix(0, length(Top3_Result), 6))
Top3_Result = cbind(Top3_Result, Temp_M)
names(Top3_Result) = c("Date", "Top1_name", "Top1_value","Top2_name", "Top2_value","Top3_name", "Top3_value")

for (iRow in 1:nrow(Top3_Result)){
   Temp_sort = order(result[iRow, Column], decreasing = TRUE)
   Temp_sort = Temp_sort + 2 
   Top3_Result[iRow, 2] = names(result)[Temp_sort[1]]
   Top3_Result[iRow, 3] = result[iRow, Temp_sort[1]]
   
   Top3_Result[iRow, 4] = names(result)[Temp_sort[2]]
   Top3_Result[iRow, 5] = result[iRow, Temp_sort[2]]
   
   Top3_Result[iRow, 6] = names(result)[Temp_sort[3]]
   Top3_Result[iRow, 7] = result[iRow, Temp_sort[3]]
}

png(paste0(output_path, "/quantile/Top3_",tau,"_",name,Date,".png"), width = 900, height = 600, bg = "transparent")

print(ggplot()+geom_line(data = Top3_Result,aes(x = Date,y = Top1_value),colour = "red",size=1)+
  geom_line(data = Top3_Result,aes(x = Date,y = Top2_value),colour = "blue",size=1)+
  geom_line(data = Top3_Result,aes(x = Date,y = Top3_value),colour = "black",size=1)+
  xlab("Date")+ylab("Scores")+
  scale_x_date(breaks = datebreaks) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.line = element_line(colour = "black"),legend.position="none")
  )
  
  
dev.off()

write.xlsx(Top3_Result, paste0(output_path, "/quantile/",name,tau,"_Top3_",Date,".xlsx"), sheetName="Sheet1",
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)


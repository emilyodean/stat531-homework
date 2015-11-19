getMonitor = function(id = 1:332, view_plot=FALSE) {
    completedata = data.frame(monitor_number = numeric(0), complete_cases = numeric(0), correlation = numeric(0), intercept=numeric(0),coefficient=numeric(0), stringsAsFactors=FALSE, check.names=FALSE)
    
    for(i in id)
    {
        if (i < 10) {id = paste(c("00",i),collapse="") }
        else if (i >= 10 & i <= 99) { id = paste(c("0",i),collapse="") }
        else {id = i}
        
        data = read.csv(file=paste(c(id, ".csv"), collapse=""), head=TRUE)      
        clean_data = subset(data, !is.na(data[,1]) & !is.na(data[,2]) & !is.na(data[,3]) & !is.na(data[,4]))
        num_rows = nrow(clean_data)
        
        if(num_rows > 2)
        {
            correlation = cor(clean_data$nitrate, clean_data$sulfate)
            m1 = lm(clean_data$nitrate~clean_data$sulfate)
            coefficients = coef(m1)
            
            if(view_plot == TRUE)
            {
                equation = paste(c("y= ",coefficients(m1)[2], "x + ", coefficients(m1)[1]), collapse="")
                plot(clean_data$sulfate, clean_data$nitrate, xlab="Sulfate", ylab="Nitrate", main=id)
                text(x=15, y=1.5, labels=equation)
                abline(reg=m1)
            }
            
            #completedata = rbind(completedata, data.frame(id, num_rows, correlation, coefficients))
            completedata[nrow(completedata) + 1,] = c(id, num_rows, correlation, coefficients)
        }else
        {
            #completedata = rbind(completedata, data.frame(id, NA, NA, NA, NA))
            completedata[nrow(completedata) + 1,] = c(id, NA, NA, NA, NA)
        }
    }  
    
    completedata
}



setwd("/Users/emilyodean/CodeProjects")
pdf(file="Rexercise_all.pdf")
setwd("/Users/emilyodean/CodeProjects/specdata")
complete_data = getMonitor(1:10)
dev.off()


#12 stuff
hist(complete_data$complete_cases, na.rm=T, main="Distribution of sample sizes")
hist(as.integer(complete_data$correlation), na.rm=T, main="Distribution of correlation")
hist(as.integer(complete_data$complete_cases), na.rm=T, main="Distribution of sample sizes")
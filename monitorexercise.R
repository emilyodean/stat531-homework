getMonitor = function(fileName, plot = FALSE)
{
    data = read.csv(file=fileName, header=T, as.is=T)
    data_clean = na.omit(data)
    
    if(nrow(data_clean) > 1)
    {
        plot(nitrate~sulfate, data=data_clean, main=paste("Monitor", substr(fileName, 1, 3)))
        abline(reg=model, col=2)
        output=c(nrow(data_clean), cor(data_clean$sulfate,data_clean$nitrate), model$coef)
    } else {
        output=c(NA,NA,NA,NA)
    }
    return(output)
}

setwd("/Users/emilyodean/CodeProjects")
pdf(file="Rexercise_all.pdf")
setwd("/Users/emilyodean/CodeProjects/specdata")
files = dir()

monitorInfo = data.frame(Name = vector(mode="character", length=0),
                         N = vector(mode="numeric", length=0),
                         Corr = vector(mode="numeric", length=0),
                         B0 = vector(mode="numeric", length=0), 
                         B1 = vector(mode="numeric", length=0),
                         stringsAsFactors=FALSE)

counter = 1
for(monitorI in files)
{
    infoTemp = getMonitor(monitorI, TRUE)
    nameTemp = substr(monitorI, 1, 3)
    
    monitorInfoTemp = data.frame(Name = nameTemp,
                                 N = infoTemp[1]
                                 Corr = infoTemp[2]
                                 B0 = infoTemp[3]
                                 B1 = infoTemp[4]
                                 stringsAsFactors=FALSE)
    
    monitorInfo = rbind(monitorInfo, monitorInfoTemp)
    
    counter = counter+1
}

setwd("/Users/emilyodean/CodeProjects")
dev.off()

rownames(monitorInfo) = NULL


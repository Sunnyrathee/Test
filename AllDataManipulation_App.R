#setwd("/Users/brentducote/Dropbox/Strategic Insights/R Directory/Copy Analysis")
AD <- read.csv("/Users/brentducote/Dropbox/Strategic Insights/R Directory/Copy Analysis/AllData_Worksheet.csv", header=TRUE)

#variables 0s included (for rates)
imps <- AD$impressions
linkImps <- AD$impressions [AD$link == "1"]
RTs <- AD$retweets
replies <- AD$replies
favs <- AD$favorites
clicks <- AD$url.clicks
engmnt <- clicks + favs + replies + RTs
overallER <- sum(engmnt)/sum(imps)


# User defined function to generate graph
withVsWithout <- function (dataCol) {  
    

  withX= (RTs[dataCol == 1] + replies[dataCol == 1] + favs[dataCol == 1] + clicks[dataCol == 1]) / imps[dataCol == 1]
  withX[imps[dataCol==1]==0] <- NA
  withX <- na.omit(withX)
  
  withoutX= (RTs[dataCol == 0] + replies[dataCol == 0] + favs[dataCol == 0] + clicks[dataCol == 0]) / imps[dataCol == 0]
  withoutX[imps[dataCol==0]==0] <- NA
  withoutX <- na.omit(withoutX)
  
  WithXGlobal <<- withX
  WithoutXGlobal <<- withoutX
  
  meanWithX = (sum(RTs[dataCol == "1"], na.rm = TRUE) + sum(replies[dataCol == "1"], na.rm = TRUE) + sum(favs[dataCol == "1"], na.rm = TRUE) + sum(clicks[dataCol == "1"], na.rm = TRUE)) / sum(imps[dataCol == "1"], na.rm = TRUE)
  withXsd = sd(withX)
  
  meanWithoutX = (sum(RTs[dataCol == "0"], na.rm = TRUE) + sum(replies[dataCol == "0"], na.rm = TRUE) + sum(favs[dataCol == "0"], na.rm = TRUE) + sum(clicks[dataCol == "0"], na.rm = TRUE)) / sum(imps[dataCol == "0"], na.rm = TRUE)
  withoutXsd = sd(withoutX)
  
  zero1<<- length(WithXGlobal[WithXGlobal==0])/length(WithXGlobal)
  zero2<<- length(WithoutXGlobal[WithoutXGlobal==0])/length(WithoutXGlobal)
  
  Xpd = abs(meanWithX-meanWithoutX)/((meanWithX+meanWithoutX)/2)
  
  outputTable <- matrix(c(meanWithX,withXsd,zero1,meanWithoutX,withoutXsd,zero2,Xpd,"",""),ncol=3,byrow=TRUE)
  colnames(outputTable) <- c("Engagement Rate", "Std. Dev", "% of Zeroes")
  rownames(outputTable) <- c("With", "Without","Percent Difference")
  
  return(outputTable)
}

# Inputs
outputTable <- withVsWithout(AD$link)
BM = 0.0084

##### PDF/CDF CREATION ######

# input: arrayOfValues = matrix of blah blah
# output: p
calculatePDF <- function(arrayOfValues) {
  interval = .0001
  list = seq(0,.3,interval)
  
  PDFmatrix = matrix(ncol=2,nrow=length(list)-1)
  PDFmatrix[,1] = list[1:length(list)-1]
  PDFmatrix[,2] = 0
  for (index in 2:length(list)) {
    preValue = list[index-1]
    currentValue = list[index]
    
    #PDF
    PDF =  length(arrayOfValues[arrayOfValues > preValue & arrayOfValues <= currentValue])/length(arrayOfValues[arrayOfValues>0])
    PDFmatrix[index-1,2] = PDF
    
  }
  return(PDFmatrix)
}

convertPDFtoCDF <- function(pdfMatrix) {
  cdfMatrix = matrix(ncol = 2, nrow = length(pdfMatrix[,1]))
  cdfMatrix[,1] = pdfMatrix[,1]
  summation = 0
  for (index in 1:length(pdfMatrix[,1])) {
    summation = summation + pdfMatrix[index,2]
    cdfMatrix[index,2] = summation
  }
  return(cdfMatrix)
} 

pdfMatrixERWith = calculatePDF(WithXGlobal)
cdfMatrixERWith = convertPDFtoCDF(pdfMatrixERWith)
pdfMatrixERWithout = calculatePDF(WithoutXGlobal)
cdfMatrixERWithout = convertPDFtoCDF(pdfMatrixERWithout)

cdfMatrixERWith1 = floor(cdfMatrixERWith*10000)/10000
cdfMatrixERWithout1 = floor(cdfMatrixERWithout*10000)/10000

cdfERBMWith = cdfMatrixERWith1[,2][cdfMatrixERWith1[,1] == BM]
cdfERBMWithout = cdfMatrixERWithout1[,2][cdfMatrixERWithout1[,1] == BM]
prob1With = (1- cdfERBMWith)
prob1Without = (1-cdfERBMWithout)
prob2With = prob1With*(1-as.numeric(outputTable[1,3]))
prob2Without = prob1Without*(1-as.numeric(outputTable[2,3]))
probPD = (prob2With-prob2Without)/((prob2With+prob2Without)/2)
CDFatBM <- matrix(c(cdfERBMWith, cdfERBMWithout,"",prob1With,prob1Without,"",prob2With,prob2Without,probPD), ncol=3, nrow=3,byrow=FALSE)
colnames(CDFatBM) <- c("CDF at Benchmark", "Probability >= to BM", "Probability >= BM *with zeroes")

#####

OverallTable <- cbind(outputTable,CDFatBM)
View(OverallTable)
#View(tableDump)



#setwd("/Users/brentducote/Dropbox/Strategic Insights/R Directory/Copy Analysis")
print("sourcing alldatamanipulation...")

AD <- read.csv("/Users/brentducote/Dropbox/Strategic Insights/R Directory/Copy Analysis/AllData_Worksheet.csv", header=TRUE)
clients <- as.matrix(unique(AD$client))
verticals <- as.matrix(unique(AD$vertical))

#variables 0s included (for rates)
imps <- AD$impressions
linkImps <- AD$impressions [AD$link == "1"]
RTs <- AD$retweets
replies <- AD$replies
favs <- AD$favorites
clicks <- AD$url.clicks
engmnt <- clicks + favs + replies + RTs
overallER <- sum(engmnt, na.rm = TRUE)/sum(imps, na.rm = TRUE)
overallCTR <- sum(clicks, na.rm = TRUE)/sum(linkImps, na.rm = TRUE)


if(exists("client", envir = .GlobalEnv)) {
  client <- get("client", envir = .GlobalEnv)
} else {
  client <- ""
}

if(exists("benchmark", envir = .GlobalEnv)) {
  benchmark <- get("benchmark", envir = .GlobalEnv)
} else {
  benchmark <- ""
}

if(exists("goal", envir = .GlobalEnv)) {
  goal <- get("goal", envir = .GlobalEnv)
} else {
  goal <- ""
}


AD <- AD[AD$client == client,]


clients <- as.matrix(unique(AD$client))
verticals <- as.matrix(unique(AD$vertical))


####  FROM APP

# goal = "Engagement Rate"
# client= "Leviton"
# benchmark = .0084


TEST_START_INDEX = 12
TEST_END_INDEX = 26
tests = colnames(AD[TEST_START_INDEX:TEST_END_INDEX])

outputMatrix = matrix(ncol= 4, nrow=(length(tests)))
colnames(outputMatrix) = c("Test Name", "With", "Without", "% Diff")
outputMatrix[,1] = tests


############################# ER test 
if (exists("goal") && goal == "Engagement Rate"){
  cat("ER TRUE");


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
rowIndex = 1

for (loopIndex in seq(from = TEST_START_INDEX, length.out = length(tests), by = 1)) {     
  outputTable <- withVsWithout(AD[,loopIndex])

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
  
  cdfERBMWith = cdfMatrixERWith1[,2][cdfMatrixERWith1[,1] == abs(benchmark)]
  cdfERBMWithout = cdfMatrixERWithout1[,2][cdfMatrixERWithout1[,1] == abs(benchmark)]
  prob1With = (1- cdfERBMWith)
  prob1Without = (1-cdfERBMWithout)
  prob2With = prob1With*(1-as.numeric(outputTable[1,3]))
  prob2Without = prob1Without*(1-as.numeric(outputTable[2,3]))
  probPD = (prob2With-prob2Without)/((prob2With+prob2Without)/2)
  
  prob2With1 = round(prob2With*100,2)
  prob2Without1 = round(prob2Without*100,2)
  probPD1 = round(probPD*100,2)
  
  outputMatrix[rowIndex,2] = prob2With1
  outputMatrix[rowIndex,3] = prob2Without1
  outputMatrix[rowIndex,4] = probPD1
  rowIndex = rowIndex + 1
  
#   CDFatBMapp <- CDFatBM[,3]
#   CDFatBMapp <- cbind(c("With","Without","% diff"),c("","",""), CDFatBMapp)
#   YN <- ifelse(CDFatBMapp[1,3]>CDFatBMapp[2,3],"Y","N")
#   NY <- ifelse(CDFatBMapp[2,3]>CDFatBMapp[1,3],"Y","N")
#   CDFatBMapp[1,2] <- YN
#   CDFatBMapp[2,2] <- NY
#   CDFatBMapp[3,2] <- "---"
#   colnames(CDFatBMapp) <- c("TEST NAME","Y/N", "CDF (%)")
#   
  
 # colnames(CDFatBM) <- c("CDF at Benchmark", "Probability >= to BM", "Probability >= BM *with zeroes")
    
}
}

############################# CTR test
if (exists("goal") && goal == "CTR"){
  
  cat("CTR TRUE");
  
  withVsWithoutCTR <- function (dataCol) {  
    withX= clicks[dataCol == 1]/ linkImps[dataCol == 1]
    withX[linkImps[dataCol==1]==0] <- NA
    withX <- na.omit(withX)
    
    withoutX= clicks[dataCol == 0] / linkImps[dataCol == 0]
    withoutX[linkImps[dataCol==0]==0] <- NA
    withoutX <- na.omit(withoutX)
    
    WithXGlobal <<- withX
    WithoutXGlobal <<- withoutX
    
    meanWithX = sum(clicks[dataCol == "1"], na.rm = TRUE) / sum(na.omit(linkImps[dataCol == "1"], na.rm = TRUE))
    withXsd = sd(withX)
    
    meanWithoutX = sum(clicks[dataCol == "0"], na.rm = TRUE) / sum(na.omit(linkImps[dataCol == "0"], na.rm = TRUE))
    withoutXsd = sd(withoutX)
    
    zero1<<- length(WithXGlobal[WithXGlobal==0])/length(WithXGlobal)
    zero2<<- length(WithoutXGlobal[WithoutXGlobal==0])/length(WithoutXGlobal)
    
    Xpd = abs(meanWithX-meanWithoutX)/((meanWithX+meanWithoutX)/2)
    
    outputTable <- matrix(c(meanWithX,withXsd,zero1,meanWithoutX,withoutXsd,zero2,Xpd,"",""),ncol=3,byrow=TRUE)
    colnames(outputTable) <- c("CTR", "Std. Dev", "% of Zeroes")
    rownames(outputTable) <- c("With", "Without","Percent Difference")
    
    return(outputTable)
  }
 
  rowIndex = 1
  for (loopIndex in seq(from = TEST_START_INDEX, length.out = length(tests), by = 1)) {     
  outputTable <- withVsWithoutCTR(AD[,loopIndex])  


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

cdfERBMWith = cdfMatrixERWith1[,2][cdfMatrixERWith1[,1] == abs(benchmark)]
cdfERBMWithout = cdfMatrixERWithout1[,2][cdfMatrixERWithout1[,1] == abs(benchmark)]
prob1With = (1- cdfERBMWith)
prob1Without = (1-cdfERBMWithout)
prob2With = prob1With*(1-as.numeric(outputTable[1,3]))
prob2Without = prob1Without*(1-as.numeric(outputTable[2,3]))
probPD = (prob2With-prob2Without)/((prob2With+prob2Without)/2)

prob2With1 = round(prob2With*100,2)
prob2Without1 = round(prob2Without*100,2)
probPD1 = round(probPD*100,2)

outputMatrix[rowIndex,2] = prob2With1
outputMatrix[rowIndex,3] = prob2Without1
outputMatrix[rowIndex,4] = probPD1
rowIndex = rowIndex + 1

#   CDFatBMapp <- CDFatBM[,3]
#   CDFatBMapp <- cbind(c("With","Without","% diff"),c("","",""), CDFatBMapp)
#   YN <- ifelse(CDFatBMapp[1,3]>CDFatBMapp[2,3],"Y","N")
#   NY <- ifelse(CDFatBMapp[2,3]>CDFatBMapp[1,3],"Y","N")
#   CDFatBMapp[1,2] <- YN
#   CDFatBMapp[2,2] <- NY
#   CDFatBMapp[3,2] <- "---"
#   colnames(CDFatBMapp) <- c("TEST NAME","Y/N", "CDF (%)")
#   

# colnames(CDFatBM) <- c("CDF at Benchmark", "Probability >= to BM", "Probability >= BM *with zeroes")

}
}


b2cCTR = ".0048"
b2cER = .0084
b2bCTR = .0041
b2bER = .0083
mediaCTR = .0048
mediaER = .0073
sportsCTR = .0040
sportsER = .0072


refMatrix = matrix(c(b2cCTR,b2cER,b2bCTR,b2bER,mediaCTR,mediaER,sportsCTR,sportsER), ncol= 2, nrow=4)
colnames(refMatrix) = c("CTR", "Engagement Rate")
rownames(refMatrix) = c("B2B", "B2C", "Media", "Sports")



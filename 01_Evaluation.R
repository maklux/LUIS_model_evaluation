##########################################################
### CUSTOM PARAMETERS
##########################################################

# Set Working Directory
setwd('....')

# New result data OR processed data
newData = TRUE
oldDataName = 'filename.csv'

# Experiment (model version & dataset ID)
exp <- 't0_' # experiment id
ds <- 'b5_' # dataset id
ver <- 'v1_' # model version

# Source folders
d_dir <- '02Data/'
r_dir <- '03Results/'
rp_dir <- '03Results/Processed/'

##########################################################
### LOAD NEW RESULTS
##########################################################

# Load Raw Result File
res <- read.csv2(paste0(r_dir, 'ResultsRaw.csv'), header= FALSE, stringsAsFactors = FALSE)
colnames(res) <- c('actual','utterance' ,'predicted','confidence','result_ignore')

# Remove incorrect evaluation
res$result_ignore <- NULL
res$confidence <- as.numeric(res$confidence)
res <- res[!is.na(res$confidence),]
res$result <-(res$actual == res$predicted)

# Performance
perf <- length(res$result[res$result == TRUE ]) / length(res$result); 

# Store Result File to Result Directory with Timestamp
timestamp <- Sys.time()
fn <- paste0(rp_dir,exp,ver,ds,format(timestamp, "%d%m%y%H%M"),'_', round(perf,3),'_results.csv') 
if (newData) {
  write.csv2(res, fn)
}

##########################################################
### EVALUATE RESULTS
##########################################################

# Read previous results, ** IF NEEDED **
fnn <- paste0(rp_dir, oldDataName)
if (newData == FALSE) {
  res <- read.csv2(fnn, stringsAsFactors = FALSE)
}

# Evaluate performance
table(res$result)
perf <- length(res$result[res$result == TRUE ]) / length(res$result); perf

# Extract low confidence
res_lc <- res[res$confidence < 0.6,]
table(res_lc$result)

# Extract false positives
res_f <- res[res$result == FALSE,]
mean(res$confidence)
mean(res_f$confidence)

# Calculate performance per category (precision)
res_a_freq <- data.frame(table(res$actual))
res_fa_freq <- data.frame(table(res_f$actual))
res_fa_perc <- merge.data.frame(res_fa_freq,res_a_freq, by = "Var1", all.x = TRUE)
res_fa_perc$percentage <- res_fa_perc$Freq.x /res_fa_perc$Freq.y 

##########################################################
### EVALUATE MAIN CATEGORY RESULTS
##########################################################

## LEVEL 1
res$lvl1_act <- substring(res$actual,0,3)
res$lvl1_pred <- substring(res$predicted,0,3)
res$lvl1_res <- (res$lvl1_act == res$lvl1_pred)
table(res$lvl1_res)
lvl1_perf <- length(res$lvl1_res[res$lvl1_res == TRUE ]) / length(res$lvl1_res); lvl1_perf
table(res$lvl1_act,res$lvl1_pred)

## LEVEL 2
res$lvl2_act <-  substring(res$actual,0,5)
res$lvl2_pred <- substring(res$predicted,0,5)
res$lvl2_res <- (res$lvl2_act == res$lvl2_pred)
table(res$lvl2_res)
lvl2_perf <- length(res$lvl2_res[res$lvl2_res == TRUE ]) / length(res$lvl2_res); lvl2_perf
table(res$lvl2_act,res$lvl2_pred)

##########################################################
### STORE CORE PERFORMANCE METRICS
##########################################################
library(MLmetrics)

fn <- paste0(r_dir, 'performanceSummary.csv') 
perf_overall <- read.csv(fn, stringsAsFactors = FALSE)

## Calculate accuracy
# Level 3
a3 <- Accuracy(res$actual,res$predicted)
# Level 2
a2 <- Accuracy(res$lvl2_act,res$lvl2_pred)
# Level 1
a1 <- Accuracy(res$lvl1_act,res$lvl1_pred)

## Calculate precision
# Level 3
p3 <- Precision(res$actual,res$predicted)
# Level 2
p2 <- Precision(res$lvl2_act,res$lvl2_pred)
# Level 1
p1 <- Precision(res$lvl1_act,res$lvl1_pred)

## Calculate recall
# Level 3
r3 <- Recall(res$actual,res$predicted)
# Level 2
r2 <- Recall(res$lvl2_act,res$lvl2_pred)
# Level 1
r1 <- Recall(res$lvl1_act,res$lvl1_pred)

## Calculate F-score
# Level 3
f3 <- F1_Score(res$actual,res$predicted)
# Level 2
f2 <- F1_Score(res$lvl2_act,res$lvl2_pred)
# Level 1
f1 <- F1_Score(res$lvl1_act,res$lvl1_pred)

## Category Accuracy
f_tel3 <- F1_Score(res$actual[res$lvl1_act == '1_1'],res$predicted[res$lvl1_act == '1_1'])
f_int3 <- F1_Score(res$actual[res$lvl1_act == '1_2'],res$predicted[res$lvl1_act == '1_2'])
f_ent3 <- F1_Score(res$actual[res$lvl1_act == '1_5'],res$predicted[res$lvl1_act == '1_5'])

# Count of missplaced categories
totalCat <- nrow(res_a_freq)
errorCat <- nrow(res_fa_freq)

# Combine results, for export
perf_new <- data.frame(model = exp, dataset = ds, version = ver, timestamp, a1,p1,r1,f1,a2,p2,r2,f2,accuracy = a3,precision = p3,recall = r3,fscore = f3, FScore_Tel = f_tel3, FScore_Ent = f_ent3, FScore_Int = f_int3, errorCat, totalCat)

# Print results
perf_new

# Append and write results to CSV
perf_overall <- rbind(perf_overall, perf_new)
write.csv(perf_overall, fn, row.names = FALSE)




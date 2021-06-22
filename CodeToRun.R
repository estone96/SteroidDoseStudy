require('data.table')
require('doSNOW')
require('ROCR')
require('dplyr')
require("remotes")
require("DatabaseConnector")

# remotes::install_github("OHDSI/DatabaseConnector")

oracleTempSchema = NULL

cdmDatabaseSchema <- "" ## server_database.server_scheme 

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "", #"postgresql"
                                                                server = "",
                                                                user = "",
                                                                password = "",
                                                                port = NULL)
# Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "C:/JDBC") 
# downloadJdbcDrivers("postgresql")

connection <- DatabaseConnector::connect(connectionDetails)

df = read.csv('autoimmune.csv')
df_drugs = c(read.csv('dexamethasone_systemic_drug.csv', encoding = 'UTF-8')$Id,read.csv('prednisolone_systemic_drug.csv',encoding = 'UTF-8')$Id)
df_type = c(read.csv('steroidstudy - osteonecrosisDTdrug.csv', encoding = 'UTF-8')$Id, read.csv('steroidstudy - boneFracture.csv',encoding = 'UTF-8')$Id, read.csv('steroidstudy - osteoporosis.csv',encoding = 'UTF-8')$Id)

sql <- "SELECT *
FROM @cdm_database_schema.drug_exposure
where person_id IN ( SELECT person_id
FROM @cdm_database_schema.condition_occurrence
where condition_concept_id IN (@autoimmune_list))
AND drug_concept_id IN (@drug_list)"
sql <- SqlRender::render(sql, 
                         cdm_database_schema = cdmDatabaseSchema,
                         autoimmune_list = df$Id,
                         drug_list = df_drugs)
sql <- SqlRender::translate(sql,
                            targetDialect = connection@dbms,
                            oracleTempSchema = oracleTempSchema)
res <- DatabaseConnector::querySql(connection = connection,
                                   sql = sql)


sql <- "SELECT PERSON_ID, YEAR_OF_BIRTH FROM @cdm_database_schema.person WHERE person_id IN (
SELECT person_id
FROM @cdm_database_schema.drug_exposure
where person_id IN ( SELECT person_id
FROM @cdm_database_schema.condition_occurrence
where condition_concept_id IN (@autoimmune_list))
AND drug_concept_id IN (@drug_list))"
sql <- SqlRender::render(sql, 
                         cdm_database_schema = cdmDatabaseSchema,
                         autoimmune_list = df$Id,
                         drug_list = df_drugs)
sql <- SqlRender::translate(sql,
                            targetDialect = connection@dbms,
                            oracleTempSchema = oracleTempSchema)
res_person <- DatabaseConnector::querySql(connection = connection,
                                          sql = sql)

target_person = unique(res_person$PERSON_ID)
target_person_id = rep(NA,length(target_person))

iterations = length(target_person)

numCores <- parallel::detectCores() - 1
myCluster <- makeCluster(numCores)
registerDoSNOW(myCluster)

pb = txtProgressBar(max = iterations, style = 3)
progress = function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

result_person <- foreach(i = 1:iterations, .combine = c, 
                  .options.snow = opts) %dopar% {
                    calc_age = function(i){
                      res_temp = res[res$PERSON_ID == target_person[i],]
                      res_temp = res_temp[order(res_temp$DRUG_EXPOSURE_START_DATE),]
                      res_temp = substr(as.character(res_temp$DRUG_EXPOSURE_START_DATE[1]),1,4)
                      age_target_person = as.numeric(res_temp) - res_person[res_person$PERSON_ID == target_person[i],'YEAR_OF_BIRTH']
                      age_target_person = age_target_person+1
                      if (age_target_person <= 64 & age_target_person >= 25){
                        return(target_person[i])
                      }
                    }
                    calc_age(i)
                  }


close(pb)
stopCluster(myCluster)


###Res2
drugsDf <- as.data.frame(df_drugs)
colnames(drugsDf) <- c("steroid_concept_id")

DatabaseConnector::insertTable(connection = connection,
                               tableName = "df_drugs",
                               data = drugsDf,
                               dropTableIfExists = TRUE,
                               createTable = TRUE,
                               tempTable = TRUE,
                               progressBar = TRUE,
                               useMppBulkLoad = FALSE)

sql = "SELECT condition_concept_id, person_id, condition_start_date
FROM @cdm_database_schema.condition_occurrence
WHERE person_id IN (
SELECT person_id
FROM @cdm_database_schema.drug_exposure
WHERE person_id IN ( SELECT person_id
FROM @cdm_database_schema.condition_occurrence
where condition_concept_id IN (@autoimmune_list))
AND drug_concept_id IN (SELECT steroid_concept_id FROM #@drug_table))
AND condition_concept_id IN (@type_list)"

sql <- SqlRender::render(sql, 
                         cdm_database_schema = cdmDatabaseSchema,
                         autoimmune_list = df$Id,
                         drug_table = "df_drugs",
                         type_list = df_type)
sql <- SqlRender::translate(sql,
                            targetDialect = connection@dbms,
                            oracleTempSchema = oracleTempSchema)
res2 <- DatabaseConnector::querySql(connection = connection,
                                   sql = sql)

# res2 = sqlQuery(dbconnection, gsub("\\n\\s+", " ", query))
#fwrite(res2,'bak2.csv')

sql = "SELECT *
FROM @cdm_database_schema.drug_strength
WHERE drug_concept_id IN (SELECT steroid_concept_id FROM #@drug_table) AND ingredient_concept_id IN (1518254, 1550557)
"
sql <- SqlRender::render(sql, 
                         cdm_database_schema = cdmDatabaseSchema,
                         drug_table = "df_drugs")
sql <- SqlRender::translate(sql,
                            targetDialect = connection@dbms,
                            oracleTempSchema = oracleTempSchema)
res3 <- DatabaseConnector::querySql(connection = connection,
                                    sql = sql)
#fwrite(res3,'bak3.csv')
# odbcClose(dbconnection)
DatabaseConnector::disconnect(connection)

data_input = res[res$PERSON_ID %in% result_person, ]
data_failed = res2[res2$PERSON_ID %in% result_person, ]
data_translation = res3

colnames(data_input) <- tolower(colnames(data_input))
colnames(data_failed) <- tolower(colnames(data_failed))
colnames(data_translation) <- tolower(colnames(data_translation))

data_translation$drug_value = coalesce(data_translation$amount_value, data_translation$numerator_value)

data_translation = data_translation[,c('drug_concept_id','ingredient_concept_id','drug_value')]

data_translation[data_translation$ingredient_concept_id == 1518254,'drug_value'] = as.numeric(data_translation[data_translation$ingredient_concept_id == 1518254,'drug_value']) * 6.25


data_translated = data_translation

data_input = data_input[,c("person_id","drug_concept_id", "drug_exposure_start_date", "drug_exposure_end_date" ,"drug_type_concept_id", "quantity", "days_supply" )]
data_input$drug_exposure_start_date = as.Date(data_input$drug_exposure_start_date)
data_input$drug_exposure_end_date = as.Date(data_input$drug_exposure_end_date)
data_input = merge(data_input, data_translated, by = 'drug_concept_id')
data_input$drug_value = data_input$quantity * as.numeric(data_input$drug_value)

data_failed$condition_start_date = as.Date(data_failed$condition_start_date)

data_boneFracture = fread('steroidstudy - boneFracture.csv')
data_osteonecrosis = fread('steroidstudy - osteonecrosisDTdrug.csv')
data_osteoporosis = fread('steroidstudy - osteoporosis.csv')

data_output = list()

patient_unique = unique(data_input$person_id)
iterations = length(patient_unique)

numCores <- parallel::detectCores() - 1
myCluster <- makeCluster(numCores)
registerDoSNOW(myCluster)

pb = txtProgressBar(max = iterations, style = 3)
progress = function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

result <- foreach(i = 1:iterations, .combine = rbind, 
.options.snow = opts) %dopar% {
library(data.table)
do_steroid = function(i){
data_output = list()
j = 1
temp = data_input[data_input$person_id == patient_unique[i],]
temp = temp[order(temp$drug_exposure_start_date),]
temp = temp[order(temp$drug_exposure_end_date),]

index = temp$drug_exposure_start_date[2:nrow(temp)] - temp$drug_exposure_end_date[1:nrow(temp)-1]

index = which(index > 90)

if (is.na(which(temp$drug_exposure_start_date[2:nrow(temp)] - temp$drug_exposure_end_date[1:nrow(temp)-1] > 90)[1])){
temp2 = temp
temp2_failed = data_failed[data_failed$person_id == temp2$person_id[1] & data_failed$condition_start_date >= min(temp2$drug_exposure_start_date)+90 & data_failed$condition_start_date <= max(temp2$drug_exposure_start_date)+90,]
temp2 = data.frame(temp2$person_id[1], min(temp2$drug_exposure_start_date), max(temp2$drug_exposure_end_date), sum(temp2$drug_value),sum(temp2$days_supply),max(temp2$drug_value)>=250,0,0,0)
colnames(temp2) = c('person_id','start_date','end_date','drug_sum','duration_sum','is_pulse','bonefracture','osteonecrosis','osteoporosis')
if (nrow(temp2_failed) >= 1){
if (temp2_failed$condition_concept_id %in% data_boneFracture$Id){
temp2$bonefracture = 1
}
if (temp2_failed$condition_concept_id %in% data_osteonecrosis$Id){
temp2$osteonecrosis = 1
}
if (temp2_failed$condition_concept_id %in% data_osteoporosis$Id){
temp2$osteoporosis = 1
}
}
data_output[[j]] = temp2
j = j+1
} else {
start_value = 1
index = c(index, nrow(temp))
for (items in 1:length(index)){
temp2 = temp[start_value:index[items],]
temp2_failed = data_failed[data_failed$person_id == temp2$person_id[1] & data_failed$condition_start_date >= min(temp2$drug_exposure_start_date)+90 & data_failed$condition_start_date <= max(temp2$drug_exposure_start_date)+90,]
temp2 = data.frame(temp2$person_id[1], min(temp2$drug_exposure_start_date), max(temp2$drug_exposure_end_date), sum(temp2$drug_value),sum(temp2$days_supply),max(temp2$drug_value) >= 250,0,0,0)
colnames(temp2) = c('person_id','start_date','end_date','drug_sum','duration_sum','is_pulse','bonefracture','osteonecrosis','osteoporosis')
if (nrow(temp2_failed) >= 1){
if (sum(temp2_failed$condition_concept_id %in% data_boneFracture$Id) >= 1){
temp2$bonefracture = 1
}
if (sum(temp2_failed$condition_concept_id %in% data_osteonecrosis$Id) >= 1){
temp2$osteonecrosis = 1
}
if (sum(temp2_failed$condition_concept_id %in% data_osteoporosis$Id) >= 1){
temp2$osteoporosis = 1
}
}
data_output[[j]] = temp2
start_value = 1 + index[items]
j = j+1
}
}
output = rbindlist(data_output)
return(output)
}

do_steroid(i)
}

close(pb)
stopCluster(myCluster)
#fwrite(result,'bak4.csv')

result$start_date = as.Date(result$start_date)
result$end_date = as.Date(result$end_date)
result$day_average = result$drug_sum / result$duration_sum
result$drug_sum_original = result$drug_sum
result$drug_sum = log(result$drug_sum)
result$duration_sum_original = result$duration_sum
result$duration_sum = log(result$duration_sum_original)

steroid_analysis = function(target, min_dose = 0, end_dose = Inf, pulse = F, output_name = 'output'){
output = c(NA, NA, NA)
output_p = c(NA, NA, NA)
output_n = c(NA, NA, NA)
output_all = c(NA, NA, NA)

if (pulse == F){
target = target[target$day_average > min_dose & target$day_average <= end_dose,]
} else {
target = target[target$is_pulse == T,]
}

jpeg(paste(output_name,'.jpg',sep=''),width = 1280, height = 720)
par(mfrow = c(3,5))
output_n[1] = sum(target$bonefracture)
if (output_n[1] != 0){
hist(unlist(target[target$bonefracture == 1,"drug_sum_original"]),main = 'Bonefracture', xlab = 'Prednisone (mg)')
hist(unlist(target[target$bonefracture == 1,"drug_sum"]),main = 'Bonefracture (log)', xlab = 'Prednisone log(mg)')
target$bonefracture = factor(target$bonefracture)

model = glm(bonefracture ~ drug_sum, family = binomial(link = 'logit'), data = target)
#summary(model)
#exp(model$coefficients)
p <- predict(model, newdata=target, type="response")
pr <- prediction(p, target$bonefracture)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main = "ROC Curve")

predictions = prediction(predict(model, newdata=target, type="response"), target$bonefracture)
plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values),
type="l", lwd=1, ylab="Sensitivity", xlab="Cutoff")
par(new=TRUE)
plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values),
type="l", lwd=1, col='red', ylab="", xlab="", main = "Cutoff Analysis")
#axis(4, at=seq(0,1,0.2),labels=z)
mtext("Specificity",side=4, padj=-2, col='red')

sens = cbind(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values))
spec = cbind(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values))

morp = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]

#1/(1+exp(-(model$coefficients[2]*300+model$coefficients[1])))
p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
out = (log(p/(1-p)) - coef(model)[1])/coef(model)[2]

X1_range = seq(from=min(target$drug_sum), to=max(target$drug_sum)*1.05, by=0.1)
logits = coef(model)[1]+coef(model)[2]*X1_range
probs = exp(logits)/(1 + exp(logits))
plot(exp(X1_range), probs, type = 'l',ylim = c(0,1), main = 'Probablity Plot', xlab = 'Prednisone (mg)', ylab = 'Probablity')
abline(h = p, col = 'red', lty = 'dashed')

output_p[1] = p
output[1] = exp(out)
output_all[1] = nrow(target)
}

output_n[2] = sum(target$osteonecrosis)
if (output_n[2] != 0){
hist(unlist(target[target$osteonecrosis == 1,"drug_sum_original"]),main = 'Osteonecrosis', xlab = 'Prednisone (mg)')
hist(unlist(target[target$osteonecrosis == 1,"drug_sum"]),main = 'Osteonecrosis (log)', xlab = 'Prednisone log(mg)')
target$osteonecrosis = factor(target$osteonecrosis)

model = glm(osteonecrosis ~ drug_sum, family = binomial(link = 'logit'), data = target)
#summary(model)
#exp(model$coefficients)
p <- predict(model, newdata=target, type="response")
pr <- prediction(p, target$osteonecrosis)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main = "ROC Curve")

predictions = prediction(predict(model, newdata=target, type="response"), target$osteonecrosis)
plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values),
type="l", lwd=2, ylab="Sensitivity", xlab="Cutoff")
par(new=TRUE)
plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values),
type="l", lwd=2, col='red', ylab="", xlab="", main = 'Cutoff Analysis')
#axis(4, at=seq(0,1,0.2),labels=z)
mtext("Specificity",side=4, padj=-2, col='red')

sens = cbind(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values))
spec = cbind(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values))

p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]

#1/(1+exp(-(model$coefficients[2]*300+model$coefficients[1])))

out = (log(p/(1-p)) - coef(model)[1])/coef(model)[2]

X1_range = seq(from=min(target$drug_sum), to=max(target$drug_sum)*1.05, by=0.1)
logits = coef(model)[1]+coef(model)[2]*X1_range
probs = exp(logits)/(1 + exp(logits))
plot(exp(X1_range), probs, type = 'l',ylim = c(0,1), main = 'Probablity Plot', xlab = 'Prednisone (mg)', ylab = 'Probablity')
abline(h = p, col = 'red', lty = 'dashed')

output_p[2] = p
output[2] = exp(out)
output_all[2] = nrow(target)
}

output_n[3] = sum(target$osteoporosis)
if (output_n[3] != 0){
hist(unlist(target[target$osteoporosis == 1,"drug_sum_original"]),main = 'Osteoporosis', xlab = 'Prednisone (mg)')
hist(unlist(target[target$osteoporosis == 1,"drug_sum"]),main = 'Osteoporosis (log)', xlab = 'Prednisone log(mg)')
target$osteoporosis = factor(target$osteoporosis)


model = glm(osteoporosis ~ drug_sum, family = binomial(link = 'logit'), data = target)
#summary(model)
#exp(model$coefficients)
p <- predict(model, newdata=target, type="response")
pr <- prediction(p, target$osteoporosis)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main = "ROC Curve")

predictions = prediction(predict(model, newdata=target, type="response"), target$osteoporosis)
plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values),
type="l", lwd=2, ylab="Sensitivity", xlab="Cutoff")
par(new=TRUE)
plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values),
type="l", lwd=2, col='red', ylab="", xlab="",main='Cutoff Analysis')
##axis(4, at=seq(0,1,0.2),labels=z)
mtext("Specificity",side=4, padj=-2, col='red')

sens = cbind(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values))
spec = cbind(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values))

p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]

#1/(1+exp(-(model$coefficients[2]*300+model$coefficients[1])))

out = (log(p/(1-p)) - coef(model)[1])/coef(model)[2]

X1_range = seq(from=min(target$drug_sum), to=max(target$drug_sum)*1.05, by=0.1)
logits = coef(model)[1]+coef(model)[2]*X1_range
probs = exp(logits)/(1 + exp(logits))
plot(exp(X1_range), probs, type = 'l',ylim = c(0,1), main = 'Probablity Plot', xlab = 'Prednisone (mg)', ylab = 'Probablity')
abline(h = p, col = 'red', lty = 'dashed')

output_p[3] = p
output[3] = exp(out)
output_all[3] = nrow(target)
}

output_return = rbind(output_p, output, output_n, output_all)
colnames(output_return) = c('Bonefracture','Osteonecrosis','Osteoporosis')
rownames(output_return) = c('Cutoff Probablity','Cutoff Value','Number of Items','Total Items')
dev.off()
par(mfrow = c(1,1))
write.csv(output_return, paste(output_name,'.csv',sep=""))
return(output_return)
}



steroid_analysis(result, output_name = 'output_all')
steroid_analysis(result, min_dose = 0, end_dose = 7.5, output_name = 'output_low_dose')
steroid_analysis(result, min_dose = 7.5, end_dose = 30, output_name = 'output_medium_dose')
steroid_analysis(result, min_dose = 30, end_dose = 100, output_name = 'output_high_dose')
steroid_analysis(result, min_dose = 100, end_dose = Inf, output_name = 'output_very_high_dose')
steroid_analysis(result, pulse = T, output_name = 'output_pulse')

                         
steroid_analysis_duration = function(target, min_dose = 0, end_dose = Inf, pulse = F, output_name = 'output'){
  output = c(NA, NA, NA)
  output_p = c(NA, NA, NA)
  output_n = c(NA, NA, NA)
  output_all = c(NA, NA, NA)
  
  if (pulse == F){
    target = target[target$day_average > min_dose & target$day_average <= end_dose,]
  } else {
    target = target[target$is_pulse == T,]
  }
  
  jpeg(paste(output_name,'.jpg',sep=''),width = 1280, height = 720)
  par(mfrow = c(3,5))
  output_n[1] = sum(target$bonefracture)
  if (output_n[1] != 0){
    hist(unlist(target[target$bonefracture == 1,"duration_sum_original"]),main = 'Bonefracture', xlab = 'Prednisone (mg)')
    hist(unlist(target[target$bonefracture == 1,"duration_sum"]),main = 'Bonefracture (log)', xlab = 'Prednisone log(mg)')
    target$bonefracture = factor(target$bonefracture)
    
    model = glm(bonefracture ~ duration_sum, family = binomial(link = 'logit'), data = target)
    #summary(model)
    #exp(model$coefficients)
    p <- predict(model, newdata=target, type="response")
    pr <- prediction(p, target$bonefracture)
    prf <- performance(pr, measure = "tpr", x.measure = "fpr")
    plot(prf, main = "ROC Curve")
    
    predictions = prediction(predict(model, newdata=target, type="response"), target$bonefracture)
    plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values),
         type="l", lwd=1, ylab="Sensitivity", xlab="Cutoff")
    par(new=TRUE)
    plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values),
         type="l", lwd=1, col='red', ylab="", xlab="", main = "Cutoff Analysis")
    #axis(4, at=seq(0,1,0.2),labels=z)
    mtext("Specificity",side=4, padj=-2, col='red')
    
    sens = cbind(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values))
    spec = cbind(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values))
    
    morp = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
    
    #1/(1+exp(-(model$coefficients[2]*300+model$coefficients[1])))
    p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
    out = (log(p/(1-p)) - coef(model)[1])/coef(model)[2]
    
    X1_range = seq(from=min(target$duration_sum), to=max(target$duration_sum)*1.05, by=0.1)
    logits = coef(model)[1]+coef(model)[2]*X1_range
    probs = exp(logits)/(1 + exp(logits))
    plot(exp(X1_range), probs, type = 'l',ylim = c(0,1), main = 'Probablity Plot', xlab = 'Prednisone (mg)', ylab = 'Probablity')
    abline(h = p, col = 'red', lty = 'dashed')
    
    output_p[1] = p
    output[1] = exp(out)
    output_all[1] = nrow(target)
  }
  
  output_n[2] = sum(target$osteonecrosis)
  if (output_n[2] != 0){
    hist(unlist(target[target$osteonecrosis == 1,"duration_sum_original"]),main = 'Osteonecrosis', xlab = 'Prednisone (mg)')
    hist(unlist(target[target$osteonecrosis == 1,"duration_sum"]),main = 'Osteonecrosis (log)', xlab = 'Prednisone log(mg)')
    target$osteonecrosis = factor(target$osteonecrosis)
    
    model = glm(osteonecrosis ~ duration_sum, family = binomial(link = 'logit'), data = target)
    #summary(model)
    #exp(model$coefficients)
    p <- predict(model, newdata=target, type="response")
    pr <- prediction(p, target$osteonecrosis)
    prf <- performance(pr, measure = "tpr", x.measure = "fpr")
    plot(prf, main = "ROC Curve")
    
    predictions = prediction(predict(model, newdata=target, type="response"), target$osteonecrosis)
    plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values),
         type="l", lwd=2, ylab="Sensitivity", xlab="Cutoff")
    par(new=TRUE)
    plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values),
         type="l", lwd=2, col='red', ylab="", xlab="", main = 'Cutoff Analysis')
    #axis(4, at=seq(0,1,0.2),labels=z)
    mtext("Specificity",side=4, padj=-2, col='red')
    
    sens = cbind(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values))
    spec = cbind(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values))
    
    p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
    
    #1/(1+exp(-(model$coefficients[2]*300+model$coefficients[1])))
    
    out = (log(p/(1-p)) - coef(model)[1])/coef(model)[2]
    
    X1_range = seq(from=min(target$duration_sum), to=max(target$duration_sum)*1.05, by=0.1)
    logits = coef(model)[1]+coef(model)[2]*X1_range
    probs = exp(logits)/(1 + exp(logits))
    plot(exp(X1_range), probs, type = 'l',ylim = c(0,1), main = 'Probablity Plot', xlab = 'Prednisone (mg)', ylab = 'Probablity')
    abline(h = p, col = 'red', lty = 'dashed')
    
    output_p[2] = p
    output[2] = exp(out)
    output_all[2] = nrow(target)
  }
  
  output_n[3] = sum(target$osteoporosis)
  if (output_n[3] != 0){
    hist(unlist(target[target$osteoporosis == 1,"duration_sum_original"]),main = 'Osteoporosis', xlab = 'Prednisone (mg)')
    hist(unlist(target[target$osteoporosis == 1,"duration_sum"]),main = 'Osteoporosis (log)', xlab = 'Prednisone log(mg)')
    target$osteoporosis = factor(target$osteoporosis)
    
    
    model = glm(osteoporosis ~ duration_sum, family = binomial(link = 'logit'), data = target)
    #summary(model)
    #exp(model$coefficients)
    p <- predict(model, newdata=target, type="response")
    pr <- prediction(p, target$osteoporosis)
    prf <- performance(pr, measure = "tpr", x.measure = "fpr")
    plot(prf, main = "ROC Curve")
    
    predictions = prediction(predict(model, newdata=target, type="response"), target$osteoporosis)
    plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values),
         type="l", lwd=2, ylab="Sensitivity", xlab="Cutoff")
    par(new=TRUE)
    plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values),
         type="l", lwd=2, col='red', ylab="", xlab="",main='Cutoff Analysis')
    ##axis(4, at=seq(0,1,0.2),labels=z)
    mtext("Specificity",side=4, padj=-2, col='red')
    
    sens = cbind(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values))
    spec = cbind(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values))
    
    p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
    
    #1/(1+exp(-(model$coefficients[2]*300+model$coefficients[1])))
    
    out = (log(p/(1-p)) - coef(model)[1])/coef(model)[2]
    
    X1_range = seq(from=min(target$duration_sum), to=max(target$duration_sum)*1.05, by=0.1)
    logits = coef(model)[1]+coef(model)[2]*X1_range
    probs = exp(logits)/(1 + exp(logits))
    plot(exp(X1_range), probs, type = 'l',ylim = c(0,1), main = 'Probablity Plot', xlab = 'Prednisone (mg)', ylab = 'Probablity')
    abline(h = p, col = 'red', lty = 'dashed')
    
    output_p[3] = p
    output[3] = exp(out)
    output_all[3] = nrow(target)
  }
  
  output_return = rbind(output_p, output, output_n, output_all)
  colnames(output_return) = c('Bonefracture','Osteonecrosis','Osteoporosis')
  rownames(output_return) = c('Cutoff Probablity','Cutoff Value','Number of Items','Total Items')
  dev.off()
  par(mfrow = c(1,1))
  write.csv(output_return, paste(output_name,'_duration.csv',sep=""))
  return(output_return)
}



steroid_analysis_duration(result, output_name = 'output_all')
steroid_analysis_duration(result, min_dose = 0, end_dose = 7.5, output_name = 'output_low_dose')
steroid_analysis_duration(result, min_dose = 7.5, end_dose = 30, output_name = 'output_medium_dose')
steroid_analysis_duration(result, min_dose = 30, end_dose = 100, output_name = 'output_high_dose')
steroid_analysis_duration(result, min_dose = 100, end_dose = Inf, output_name = 'output_very_high_dose')
steroid_analysis_duration(result, pulse = T, output_name = 'output_pulse')

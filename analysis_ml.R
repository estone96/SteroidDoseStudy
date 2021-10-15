require('data.table')
require('doSNOW')
require('ROCR')
require('dplyr')
require("remotes")
require("DatabaseConnector")
require('CohortMethod')
require('survey')
require('caret')
require('WeightSVM')
require('glmnet')
# remotes::install_github("OHDSI/DatabaseConnector")


analysis_ml = function(connection,cdmDatabaseSchema, oracleTempSchema,resultsDatabaseSchema){

sql <- "select * from @vocabulary_database_schema.concept
where concept_id in (
select distinct(drug_concept_id) from @vocabulary_database_schema.drug_exposure 
where drug_concept_id in (
SELECT c.concept_id FROM (select distinct I.concept_id FROM
( 
  select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (1550557, 1506270, 1551099, 40054909, 19016867, 1518254, 920458)
UNION  select c.concept_id
  from @vocabulary_database_schema.CONCEPT c
  join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
  and ca.ancestor_concept_id in (1550557, 1506270, 1551099, 40054909, 19016867, 1518254, 920458)
  and c.invalid_reason is null
) I
) C
)
and route_concept_id in (4171047,4132161,4302612,4240824)
)"
sql <- SqlRender::render(sql, 
                         vocabulary_database_schema = cdmDatabaseSchema)
sql <- SqlRender::translate(sql,
                            targetDialect = connection@dbms,
                            oracleTempSchema = oracleTempSchema)
df_drugs <- DatabaseConnector::querySql(connection = connection,
                                        sql = sql)

df_drugs = df_drugs$CONCEPT_ID


sql <- "SELECT 2 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
  select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (4344166,437082,80182,255891,80800,40319772,80809,4117686,254443,134442,4137275)
UNION  select c.concept_id
  from @vocabulary_database_schema.CONCEPT c
  join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
  and ca.ancestor_concept_id in (4344166,437082,80182,255891,80800,40319772,80809,4117686,254443,134442,4137275)
  and c.invalid_reason is null
) I
) C"
sql <- SqlRender::render(sql, 
                         vocabulary_database_schema = cdmDatabaseSchema)
sql <- SqlRender::translate(sql,
                            targetDialect = connection@dbms,
                            oracleTempSchema = oracleTempSchema)
df_autoimmune <- DatabaseConnector::querySql(connection = connection,
                                             sql = sql)

df_autoimmune = df_autoimmune$CONCEPT_ID


sql <- "SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
  select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (4344387,433856,4033089,4067765,81390,4069306,80502,45772069,45772718,45772713,4010333,4067768,4002133,4003482,4015350,45763653,442560,4278672,4129394,4300192,4053828,73571)
UNION  select c.concept_id
  from @vocabulary_database_schema.CONCEPT c
  join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
  and ca.ancestor_concept_id in (4344387,433856,4033089,4067765,81390,4069306,80502,45772069,45772718,45772713,4010333,4067768,4002133,4003482,4015350,45763653,442560,4278672,4129394,4300192,4053828,73571)
  and c.invalid_reason is null
) I
) C"
sql <- SqlRender::render(sql, 
                         vocabulary_database_schema = cdmDatabaseSchema)
sql <- SqlRender::translate(sql,
                            targetDialect = connection@dbms,
                            oracleTempSchema = oracleTempSchema)
df_mskAE <- DatabaseConnector::querySql(connection = connection,
                                        sql = sql)

df_mskAE = df_mskAE$CONCEPT_ID

sql <- "SELECT 1 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
  select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (4015350,45763653,442560,4278672,4129394,4300192,4053828,73571)
UNION  select c.concept_id
  from @vocabulary_database_schema.CONCEPT c
  join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
  and ca.ancestor_concept_id in (4015350,45763653,442560,4278672,4129394,4300192,4053828,73571)
  and c.invalid_reason is null
) I
) C"
sql <- SqlRender::render(sql, 
                         vocabulary_database_schema = cdmDatabaseSchema)
sql <- SqlRender::translate(sql,
                            targetDialect = connection@dbms,
                            oracleTempSchema = oracleTempSchema)
df_pathologicfrac <- DatabaseConnector::querySql(connection = connection,
                                                 sql = sql)

df_pathologicfrac = df_pathologicfrac$CONCEPT_ID

sql <- "SELECT 1 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
  select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (4344387)
UNION  select c.concept_id
  from @vocabulary_database_schema.CONCEPT c
  join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
  and ca.ancestor_concept_id in (4344387)
  and c.invalid_reason is null
) I
LEFT JOIN
(
  select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (433856)
UNION  select c.concept_id
  from @vocabulary_database_schema.CONCEPT c
  join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
  and ca.ancestor_concept_id in (433856)
  and c.invalid_reason is null

) E ON I.concept_id = E.concept_id
WHERE E.concept_id is null
) C"
  sql <- SqlRender::render(sql, 
                           vocabulary_database_schema = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql,
                              targetDialect = connection@dbms,
                              oracleTempSchema = oracleTempSchema)
  df_necrosis <- DatabaseConnector::querySql(connection = connection,
                                             sql = sql)
  
  df_necrosis = df_necrosis$CONCEPT_ID
  
  sql <- "SELECT 1 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
  select concept_id from @vocabulary_database_schema.CONCEPT where concept_id in (4033089,4067765,81390,4069306,80502,45772069,45772718,45772713,4010333,4067768,4002133,4003482)
UNION  select c.concept_id
  from @vocabulary_database_schema.CONCEPT c
  join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
  and ca.ancestor_concept_id in (4033089,4067765,81390,4069306,80502,45772069,45772718,45772713,4010333,4067768,4002133,4003482)
  and c.invalid_reason is null

) I
) C"
  
  sql <- SqlRender::render(sql, 
                           vocabulary_database_schema = cdmDatabaseSchema)
  sql <- SqlRender::translate(sql,
                              targetDialect = connection@dbms,
                              oracleTempSchema = oracleTempSchema)
  df_porosis <- DatabaseConnector::querySql(connection = connection,
                                            sql = sql)
  
  df_porosis = df_porosis$CONCEPT_ID
  
  df_type = c(df_mskAE, df_pathologicfrac, df_necrosis, df_porosis)
  
  sql <- "SELECT *
FROM @cdm_database_schema.drug_exposure
where person_id IN ( SELECT person_id
FROM @cdm_database_schema.condition_occurrence
where condition_concept_id IN (@autoimmune_list))
AND drug_concept_id IN (@drug_list) AND route_concept_id in (4171047,4132161,4302612,4240824)"
  sql <- SqlRender::render(sql, 
                           cdm_database_schema = cdmDatabaseSchema,
                           autoimmune_list = df_autoimmune,
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
AND drug_concept_id IN (@drug_list) AND route_concept_id in (4171047,4132161,4302612,4240824))"
  sql <- SqlRender::render(sql, 
                           cdm_database_schema = cdmDatabaseSchema,
                           autoimmune_list = df_autoimmune,
                           drug_list = df_drugs)
  sql <- SqlRender::translate(sql,
                              targetDialect = connection@dbms,
                              oracleTempSchema = oracleTempSchema)
  res_person <- DatabaseConnector::querySql(connection = connection,
                                            sql = sql)
  
  target_person = unique(res_person$PERSON_ID)
  target_person_id = rep(NA,length(target_person))
  
  iterations = length(target_person)
  
  numCores <- parallel::detectCores() - 4
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
                               age_target_person = age_target_person
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
AND drug_concept_id IN (SELECT steroid_concept_id FROM #@drug_table) 
AND route_concept_id in (4171047,4132161,4302612,4240824))
AND condition_concept_id IN (@type_list)"
  
  sql <- SqlRender::render(sql, 
                           cdm_database_schema = cdmDatabaseSchema,
                           autoimmune_list = df_autoimmune,
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
WHERE drug_concept_id IN (SELECT steroid_concept_id FROM #@drug_table)
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
  
  data_input = res[res$PERSON_ID %in% result_person, ]
  data_failed = res2[res2$PERSON_ID %in% result_person, ]
  data_translation = res3
  
  colnames(data_input) <- tolower(colnames(data_input))
  colnames(data_failed) <- tolower(colnames(data_failed))
  colnames(data_translation) <- tolower(colnames(data_translation))
  
  data_translation$drug_value = coalesce(data_translation$amount_value, data_translation$numerator_value)
  
  data_translation = data_translation[,c('drug_concept_id','ingredient_concept_id','drug_value')]
  
  data_translation = data_translation[data_translation$ingredient_concept_id %in% c(1550557, 1506270, 1551099, 40054909, 19016867, 1518254, 920458),]
  data_translation[data_translation$ingredient_concept_id == 1518254,'drug_value'] = as.numeric(data_translation[data_translation$ingredient_concept_id == 1518254,'drug_value']) * 6.25
  data_translation[data_translation$ingredient_concept_id == 40054909,'drug_value'] = as.numeric(data_translation[data_translation$ingredient_concept_id == 40054909,'drug_value']) * 0.75
  data_translation[data_translation$ingredient_concept_id == 19016867,'drug_value'] = as.numeric(data_translation[data_translation$ingredient_concept_id == 19016867,'drug_value']) * 0.75
  data_translation[data_translation$ingredient_concept_id == 920458,'drug_value'] = as.numeric(data_translation[data_translation$ingredient_concept_id == 920458,'drug_value']) * 6.25
  
  
  data_translated = na.omit(data_translation)
  
  data_input = data_input[,c("person_id","drug_concept_id", "drug_exposure_start_date", "drug_exposure_end_date" ,"drug_type_concept_id", "quantity", "days_supply" )]
  data_input$drug_exposure_start_date = as.Date(data_input$drug_exposure_start_date)
  data_input$drug_exposure_end_date = as.Date(data_input$drug_exposure_end_date)
  data_input = merge(data_input, data_translated, by = 'drug_concept_id')
  data_input$drug_value = data_input$quantity * as.numeric(data_input$drug_value)
  
  data_failed$condition_start_date = as.Date(data_failed$condition_start_date)
  
  data_mskAE = data.frame(id = df_mskAE)
  data_pathologicfrac = data.frame(id = df_pathologicfrac)
  data_osteonecrosis = data.frame(id = df_necrosis)
  data_osteoporosis = data.frame(id = df_porosis)
  
  data_output = list()
  
  patient_unique = unique(data_input$person_id)
  iterations = length(patient_unique)
  
  numCores <- parallel::detectCores() - 4
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
                          temp2 = data.frame(temp2$person_id[1], min(temp2$drug_exposure_start_date), max(temp2$drug_exposure_end_date), sum(temp2$drug_value),sum(temp2$days_supply),max(temp2$drug_value)>=250,0,0,0,0)
                          colnames(temp2) = c('person_id','start_date','end_date','drug_sum','duration_sum','is_pulse','mskAE','osteonecrosis','osteoporosis','pathologicfrac')
                          if (nrow(temp2_failed) >= 1){
                            if (temp2_failed$condition_concept_id %in% data_mskAE$id){
                              temp2$mskAE = 1
                            }
                            if (temp2_failed$condition_concept_id %in% data_osteonecrosis$id){
                              temp2$osteonecrosis = 1
                            }
                            if (temp2_failed$condition_concept_id %in% data_osteoporosis$id){
                              temp2$osteoporosis = 1
                            }
                            if (temp2_failed$condition_concept_id %in% data_pathologicfrac$id){
                              temp2$pathologicfrac = 1
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
                            temp2 = data.frame(temp2$person_id[1], min(temp2$drug_exposure_start_date), max(temp2$drug_exposure_end_date), sum(temp2$drug_value),sum(temp2$days_supply),max(temp2$drug_value) >= 250,0,0,0,0)
                            colnames(temp2) = c('person_id','start_date','end_date','drug_sum','duration_sum','is_pulse','mskAE','osteonecrosis','osteoporosis','pathologicfrac')
                            if (nrow(temp2_failed) >= 1){
                              if (sum(temp2_failed$condition_concept_id %in% data_mskAE$id) >= 1){
                                temp2$mskAE = 1
                              }
                              if (sum(temp2_failed$condition_concept_id %in% data_osteonecrosis$id) >= 1){
                                temp2$osteonecrosis = 1
                              }
                              if (sum(temp2_failed$condition_concept_id %in% data_osteoporosis$id) >= 1){
                                temp2$osteoporosis = 1
                              }
                              if (sum(temp2_failed$condition_concept_id %in% data_pathologicfrac$id) >= 1){
                                temp2$pathologicfrac = 1
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
  
  result1 = result[apply(data.frame(result$mskAE, result$osteonecrosis, result$pathologicfrac),1,sum) > 0,]
  result2 = result[apply(data.frame(result$mskAE, result$osteonecrosis, result$pathologicfrac),1,sum) == 0,]
  
  sample1 = sample(nrow(result1),round(nrow(result1))*0.7,replace = F)
  sample2 = sample(nrow(result2),round(nrow(result2))*0.7,replace = F)
  
  result = rbind(result1[sample1,], result2[sample2,])
  result_test = rbind(result1[-sample1,], result2[-sample2,])
  
  steroid_analysis = function(target,target_test, min_dose = 0, end_dose = Inf, pulse = F, output_name = 'output'){
    output = c(NA, NA, NA, NA)
    output_p = c(NA, NA, NA, NA)
    output_n = c(NA, NA, NA, NA)
    output_all = c(NA, NA, NA, NA)
    output_auc = c(NA, NA, NA, NA)
    test_output = list()
    
    if (pulse == F){
      target = target[target$day_average > min_dose & target$day_average <= end_dose,]
    } else {
      target = target[target$is_pulse == T,]
    }
    
    jpeg(paste(output_name,'.jpg',sep=''),width = 1880, height = 1000)
    par(mfrow = c(4,5))
    output_n[1] = sum(target$mskAE)
    if (output_n[1] != 0){
      hist(unlist(target[target$mskAE == 1,"drug_sum_original"]),main = 'mskAE', xlab = 'Prednisone (mg)')
      hist(unlist(target[target$mskAE == 1,"drug_sum"]),main = 'mskAE (log)', xlab = 'Prednisone log(mg)')
      target$mskAE = factor(target$mskAE)
      
      model = glm(mskAE ~ drug_sum, family = binomial(link = 'logit'), data = target)
      #summary(model)
      
      #exp(model$coefficients)
      p <- predict(model, newdata=target, type="response")
      pr <- prediction(p, target$mskAE)
      output_auc[1] = performance(pr, "auc")@y.values[[1]][1]
      prf <- performance(pr, measure = "tpr", x.measure = "fpr")
      plot(prf, main = "ROC Curve")
      
      predictions = prediction(predict(model, newdata=target, type="response"), target$mskAE)
      plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values),
           type="l", lwd=1, ylab="Sensitivity", xlab="Cutoff")
      par(new=TRUE)
      plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values),
           type="l", lwd=1, col='red', ylab="", xlab="", main = "Cutoff Analysis")
      #axis(4, at=seq(0,1,0.2),labels=z)
      mtext("Specificity",side=4, padj=-2, col='red')
      
      ppv_npv <- performance(pr, measure = "ppv", x.measure = "npv")
      ppv_npv = data.frame(ppv_npv@alpha.values, ppv_npv@y.values, ppv_npv@x.values)
      colnames(ppv_npv) = c('cutoff','ppv','npv')
      
      spec_fr = performance(predictions, "spec")
      spec_fr = data.frame(spec_fr@x.values, spec_fr@y.values)
      colnames(spec_fr) = c('cutoff','specificity')
      
      write.csv(merge(spec_fr, ppv_npv, by = 'cutoff'), paste(output_name,'_cutoff_mskAE_ml_ml.csv',sep=""))
      
      sens = cbind(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values))
      spec = cbind(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values))
      
      morp = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
      
      #1/(1+exp(-(model$coefficients[2]*300+model$coefficients[1])))
      p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
      out = (log(p/(1-p)) - coef(model)[1])/coef(model)[2]
      
      enu_out = list()
      enu = seq(0.1,0.9,by = 0.1)
      for (enu_iter in 1:length(enu)){
        cf = confusionMatrix(as.factor(ifelse(predict(model, newdata = target, type = 'response') > enu[enu_iter], 1,0)),target$mskAE)
        tmp2 = t(data.frame(c(enu[enu_iter],cf[[4]])))
        colnames(tmp2)[1] = 'Cutoff'
        enu_out[[enu_iter]] = as.data.frame(tmp2)
      }
      write.csv(rbindlist(enu_out), paste(output_name,'_cutoff_diagnosis_mskAE_ml.csv',sep=""))
      
      cf_test = confusionMatrix(as.factor(ifelse(predict(model, newdata = target_test, type = 'response') > p, 1,0)),as.factor(target_test$mskAE))
      test_output[[1]] = t(data.frame(c('mskAE',cf_test[[4]])))
      
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
      output_auc[2] = performance(pr, "auc")@y.values[[1]][1]
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
      ppv_npv <- performance(pr, measure = "ppv", x.measure = "npv")
      ppv_npv = data.frame(ppv_npv@alpha.values, ppv_npv@y.values, ppv_npv@x.values)
      colnames(ppv_npv) = c('cutoff','ppv','npv')
      
      spec_fr = performance(predictions, "spec")
      spec_fr = data.frame(spec_fr@x.values, spec_fr@y.values)
      colnames(spec_fr) = c('cutoff','specificity')
      plot(x = spec_fr$cutoff, y = spec_fr$specificity, type = 'l', col = 2)
      lines(x = ppv_npv$cutoff, y = ppv_npv$ppv, col = 3)
      lines(x = ppv_npv$cutoff, y = ppv_npv$npv, col = 4)
      write.csv(merge(spec_fr, ppv_npv, by = 'cutoff'), paste(output_name,'_cutoff_osteonecrosis_ml.csv',sep=""))
      p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
      
      cf_test = confusionMatrix(as.factor(ifelse(predict(model, newdata = target_test, type = 'response') > p, 1,0)),as.factor(target_test$osteonecrosis))
      test_output[[2]] = t(data.frame(c('osteonecrosis',cf_test[[4]])))
      
      enu_out = list()
      enu = seq(0.1,0.9,by = 0.1)
      for (enu_iter in 1:length(enu)){
        cf = confusionMatrix(as.factor(ifelse(predict(model, newdata = target, type = 'response') > enu[enu_iter], 1,0)),target$osteonecrosis)
        tmp2 = t(data.frame(c(enu[enu_iter],cf[[4]])))
        colnames(tmp2)[1] = 'Cutoff'
        enu_out[[enu_iter]] = as.data.frame(tmp2)
      }
      write.csv(rbindlist(enu_out), paste(output_name,'_cutoff_diagnosis_osteonecrosis_ml.csv',sep=""))
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
      hist(unlist(target[target$osteoporosis == 1,"drug_sum_original"]),main = 'osteoporosis', xlab = 'Prednisone (mg)')
      hist(unlist(target[target$osteoporosis == 1,"drug_sum"]),main = 'pathologicfrac (log)', xlab = 'Prednisone log(mg)')
      target$osteoporosis = factor(target$osteoporosis)
      
      
      model = glm(osteoporosis ~ drug_sum, family = binomial(link = 'logit'), data = target)
      #summary(model)
      #exp(model$coefficients)
      
      p <- predict(model, newdata=target, type="response")
      pr <- prediction(p, target$osteoporosis)
      output_auc[3] = performance(pr, "auc")@y.values[[1]][1]
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
      ppv_npv <- performance(pr, measure = "ppv", x.measure = "npv")
      ppv_npv = data.frame(ppv_npv@alpha.values, ppv_npv@y.values, ppv_npv@x.values)
      colnames(ppv_npv) = c('cutoff','ppv','npv')
      
      spec_fr = performance(predictions, "spec")
      spec_fr = data.frame(spec_fr@x.values, spec_fr@y.values)
      colnames(spec_fr) = c('cutoff','specificity')
      
      write.csv(merge(spec_fr, ppv_npv, by = 'cutoff'), paste(output_name,'_cutoff_osteoporosis_ml.csv',sep=""))
      p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
      
      cf_test = confusionMatrix(as.factor(ifelse(predict(model, newdata = target_test, type = 'response') > p, 1,0)),as.factor(target_test$osteoporosis))
      test_output[[3]] = t(data.frame(c('osteoporosis',cf_test[[4]])))
      
      
      enu_out = list()
      enu = seq(0.1,0.9,by = 0.1)
      for (enu_iter in 1:length(enu)){
        cf = confusionMatrix(as.factor(ifelse(predict(model, newdata = target, type = 'response') > enu[enu_iter], 1,0)),target$osteoporosis)
        tmp2 = t(data.frame(c(enu[enu_iter],cf[[4]])))
        colnames(tmp2)[1] = 'Cutoff'
        enu_out[[enu_iter]] = as.data.frame(tmp2)
      }
      write.csv(rbindlist(enu_out), paste(output_name,'_cutoff_diagnosis_osteoporosis_ml.csv',sep=""))
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
    
    
    output_n[4] = sum(target$pathologicfrac)
    if (output_n[4] != 0){
      hist(unlist(target[target$pathologicfrac == 1,"drug_sum_original"]),main = 'pathologicfrac', xlab = 'Prednisone (mg)')
      hist(unlist(target[target$pathologicfrac == 1,"drug_sum"]),main = 'pathologicfrac (log)', xlab = 'Prednisone log(mg)')
      target$pathologicfrac = factor(target$pathologicfrac)
      
      
      model = glm(pathologicfrac ~ drug_sum, family = binomial(link = 'logit'), data = target)
      #summary(model)
      #exp(model$coefficients)
      
      p <- predict(model, newdata=target, type="response")
      pr <- prediction(p, target$pathologicfrac)
      output_auc[4] = performance(pr, "auc")@y.values[[1]][1]
      prf <- performance(pr, measure = "tpr", x.measure = "fpr")
      plot(prf, main = "ROC Curve")
      
      predictions = prediction(predict(model, newdata=target, type="response"), target$pathologicfrac)
      plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values),
           type="l", lwd=2, ylab="Sensitivity", xlab="Cutoff")
      par(new=TRUE)
      plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values),
           type="l", lwd=2, col='red', ylab="", xlab="",main='Cutoff Analysis')
      ##axis(4, at=seq(0,1,0.2),labels=z)
      mtext("Specificity",side=4, padj=-2, col='red')
      
      sens = cbind(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values))
      spec = cbind(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values))
      ppv_npv <- performance(pr, measure = "ppv", x.measure = "npv")
      ppv_npv = data.frame(ppv_npv@alpha.values, ppv_npv@y.values, ppv_npv@x.values)
      colnames(ppv_npv) = c('cutoff','ppv','npv')
      
      spec_fr = performance(predictions, "spec")
      spec_fr = data.frame(spec_fr@x.values, spec_fr@y.values)
      colnames(spec_fr) = c('cutoff','specificity')
      
      write.csv(merge(spec_fr, ppv_npv, by = 'cutoff'), paste(output_name,'_cutoff_pathologicfrac_ml.csv',sep=""))
      p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
      
      cf_test = confusionMatrix(as.factor(ifelse(predict(model, newdata = target_test, type = 'response') > p, 1,0)),as.factor(target_test$pathologicfrac))
      test_output[[4]] = t(data.frame(c('pathologicfrac',cf_test[[4]])))
      
      
      enu_out = list()
      enu = seq(0.1,0.9,by = 0.1)
      for (enu_iter in 1:length(enu)){
        cf = confusionMatrix(as.factor(ifelse(predict(model, newdata = target, type = 'response') > enu[enu_iter], 1,0)),target$pathologicfrac)
        tmp2 = t(data.frame(c(enu[enu_iter],cf[[4]])))
        colnames(tmp2)[1] = 'Cutoff'
        enu_out[[enu_iter]] = as.data.frame(tmp2)
      }
      write.csv(rbindlist(enu_out), paste(output_name,'_cutoff_diagnosis_pathologicfrac_ml.csv',sep=""))
      #1/(1+exp(-(model$coefficients[2]*300+model$coefficients[1])))
      
      out = (log(p/(1-p)) - coef(model)[1])/coef(model)[2]
      
      X1_range = seq(from=min(target$drug_sum), to=max(target$drug_sum)*1.05, by=0.1)
      logits = coef(model)[1]+coef(model)[2]*X1_range
      probs = exp(logits)/(1 + exp(logits))
      plot(exp(X1_range), probs, type = 'l',ylim = c(0,1), main = 'Probablity Plot', xlab = 'Prednisone (mg)', ylab = 'Probablity')
      abline(h = p, col = 'red', lty = 'dashed')
      
      output_p[4] = p
      output[4] = exp(out)
      output_all[4] = nrow(target)
    }
    
    output_return = rbind(output_auc, output_p, output, output_n, output_all)
    colnames(output_return) = c('mskAE','osteonecrosis','osteoporosis','pathologicfrac')
    rownames(output_return) = c('AUC','Cutoff Probablity','Cutoff Value','Number of Items','Total Items')
    dev.off()
    par(mfrow = c(1,1))
    for(iii in 1:4){
      test_output[[iii]] = as.data.frame(test_output[[iii]])
    }
    test_output = rbindlist(test_output)
    write.csv(test_output, paste(output_name,'_ml_test.csv',sep=""))
    write.csv(output_return, paste(output_name,'_ml.csv',sep=""))
    return(output_return)
  }
  
  
  steroid_analysis(result, result_test, output_name = 'output_all')
  # steroid_analysis(result, min_dose = 0, end_dose = 7.5, output_name = 'output_low_dose')
  # steroid_analysis(result, min_dose = 7.5, end_dose = 30, output_name = 'output_medium_dose')
  # steroid_analysis(result, min_dose = 30, end_dose = 100, output_name = 'output_high_dose')
  # steroid_analysis(result, min_dose = 100, end_dose = Inf, output_name = 'output_very_high_dose')
  # steroid_analysis(result, pulse = T, output_name = 'output_pulse')
  # 
  # steroid_analysis(result[result$drug_sum_original > 2000,], output_name = 'output_sub_2000')
  # steroid_analysis(result[result$drug_sum_original <= 2000,], output_name = 'output_over_2000')
  
  
  
  # weighting and svm
  
  result$person_row_id = 1:nrow(result)
  
  create_cohort = function(result_binded, target, comparator){
    result_temp = list()
    if (target == 'ALL' & comparator == 'ALL'){
      result_temp[[1]] = result_binded
      result_temp[[1]]$cohort_definition_id = 1
      result_temp[[2]] = result_binded 
      result_temp[[2]]$cohort_definition_id = 2
      result_temp[[3]] = result_binded[result_binded$pathologicfrac == 1|result_binded$mskAE  == 1|result_binded$osteonecrosis  == 1|result_binded$osteoporosis == 1,]
      result_temp[[3]]$cohort_definition_id = 3
      result_temp = rbindlist(result_temp)
    } else {
      result_temp[[1]] = result_binded[result_binded$drug_exposure_type == target,] 
      result_temp[[1]]$cohort_definition_id = 1
      result_temp[[2]] = result_binded[result_binded$drug_exposure_type == comparator,] 
      result_temp[[2]]$cohort_definition_id = 2
      result_temp[[3]] = result_binded[result_binded$pathologicfrac == 1|result_binded$mskAE  == 1|result_binded$osteonecrosis  == 1|result_binded$osteoporosis == 1,]
      result_temp[[3]]$cohort_definition_id = 3
      result_temp = rbindlist(result_temp)
    }
    colnames(result_temp)[c(1,2,3)] = c('subject_id','cohort_start_date','cohort_end_date')
    result_temp = result_temp[,c('cohort_definition_id','cohort_start_date','cohort_end_date','subject_id','person_row_id')]
    
    return(result_temp)
  }
  
  
  DatabaseConnector::insertTable(connection = connection,
                                 tableName = "vcdm_results_hyumc.steroiddosestudy",
                                 data = create_cohort(result, 'ALL','ALL'),
                                 dropTableIfExists = TRUE,
                                 tempTable = FALSE,
                                 createTable = TRUE,
                                 progressBar = TRUE,
                                 useMppBulkLoad = FALSE)
  
  covariateSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = c(drugsDf$steroid_concept_id, df_type),
                                                      addDescendantsToExclude = TRUE)
  
  covariateData <- getDbCovariateData(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      cohortDatabaseSchema = resultsDatabaseSchema,
                                      cohortTable = "steroiddosestudy",
                                      cohortId = 1,
                                      rowIdField = "person_row_id",
                                      covariateSettings = covariateSettings)
  
  temp = as.data.frame(covariateData$covariates)
  temp_col = as.data.frame(covariateData$covariateRef)
  temp = merge(temp, temp_col[,c(1,2)], by = 'covariateId')
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  
  result_temp = dcast(temp,rowId ~ covariateName,value.var = 'covariateValue')
  sparsity = apply(result_temp, 2, sum)
  quantile(sparsity)
  result_temp = select(result_temp, names(sparsity[sparsity > 2]))
  
  result_temp = merge(result_temp, result[,c('drug_sum','person_row_id')], by.x = 'rowId', by.y = 'person_row_id')
  
  outcome = 'drug_sum'
  y_train = result_temp[[outcome]]
  f = paste(outcome, "~.-1", sep = "")
  
  
  library(glmnet)
  result_temp2 = select(result_temp,-c('rowId'))
  colnames(result_temp2)[1:(ncol(result_temp2)-1)] = paste('V',c(1:(ncol(result_temp2)-1)),sep='')
  x_train = sparse.model.matrix(as.formula(f),data = result_temp2)
  
  fit = cv.glmnet(x = x_train, y = y_train)
  
  fitted.fit = predict(fit, s=fit$lambda.min, newx = x_train)  
  gps_fit = dnorm(y_train, mean = fitted.fit, sd = sd(y_train))
  ps = dnorm(y_train, mean = mean(y_train), sd = sd(y_train))/gps_fit
  
  
  result = result[order(result$person_row_id, decreasing = F),]
  ps = ps[order(result_temp$rowId, decreasing = F)]
  
  
  
  y_value = 'mskAE'
  
  library(kernlab)
  
  numCores <- parallel::detectCores() - 2
  myCluster <- makeCluster(numCores)
  registerDoSNOW(myCluster)
  
  
  
  
  
  
  
  #install.packages('WeightSVM')
  library(WeightSVM)
  y = as.factor(result[[y_value]])
  x = result$drug_sum
  result_oc = result[result[[y_value]] ==0,]
  ps_oc = ps[result[[y_value]] ==0]
  x_oc = result_oc$drug_sum
  
  # pb = txtProgressBar(max = 100, style = 3)
  # progress = function(n) setTxtProgressBar(pb, n)
  # opts <- list(progress = progress)
  # 
  # nu_param = seq(0.0001, 0.9999, length.out = 10)
  # gamma_param = 2^seq(-10,0, length.out = 10)
  # param_grid_oc = as.data.frame(expand.grid(nu_param, gamma_param))
  # 
  # c_param = 2^seq(-10,10, length.out = 10)
  # gamma_param = 2^seq(-10,0, length.out = 10)
  # param_grid = as.data.frame(expand.grid(c_param, gamma_param))
  # 
  # 
  # 
  # result_oc <- foreach(i = 1:nrow(param_grid_oc), .combine = rbind,
  #                      .options.snow = opts) %dopar% {
  #                        
  #                        nu_param = seq(0.0001, 0.9999, length.out = 10)
  #                        gamma_param = 2^seq(-10,0, length.out = 10)
  #                        param_grid_oc = as.data.frame(expand.grid(nu_param, gamma_param))
  #                        
  #                        get_value = function(value1, value2){
  #                          library(kernlab)
  #                          library(ROCR)
  #                          model = ksvm(x_oc, type = 'one-svc',  scaled = F, nu=value1,kpar = list(sigma = value2), prob.model = TRUE)
  #                          platt_one_svc = function(model){
  #                            library(caret)
  #                            res = predict(model, x, type = 'response')
  #                            res = ifelse(as.numeric(res) == 1, 0, 1)
  #                            
  #                            train_control <- trainControl(method = "cv", number = 10)
  #                            model2 <- train(y ~ res,
  #                                            data = data.frame(y,res),
  #                                            trControl = train_control,
  #                                            method = "glm",
  #                                            family=binomial())
  #                            
  #                            return(predict(model2, res, type = 'prob')[,2])
  #                          }
  #                          p1 = platt_one_svc(model)
  #                          pr = prediction(p1, y)
  #                          out = performance(pr, 'auc')@y.values[[1]]
  #                          
  #                          return(c(value1, value2, out))
  #                        }
  #                        
  #                        get_value(param_grid_oc[i,1],param_grid_oc[i,2])
  #                      }
  # 
  # 
  # 
  # result_svm <- foreach(i = 1:nrow(param_grid), .combine = rbind,
  #                       .options.snow = opts) %dopar% {
  #                         c_param = 2^seq(-10,10, length.out = 10)
  #                         gamma_param = 2^seq(-10,0, length.out = 10)
  #                         param_grid = as.data.frame(expand.grid(c_param, gamma_param))
  #                         
  #                         get_value = function(value1, value2){
  #                           library(kernlab)
  #                           library(ROCR)
  #                           model = ksvm(x,y, type = 'C-svc', scaled = F, C=value1,kpar = list(sigma = value2),  prob.model = TRUE)
  #                           p1 = predict(model, x, type = 'probabilities')[,2]
  #                           pr = prediction(p1, y)
  #                           prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
  #                           out = performance(pr, 'auc')@y.values[[1]]
  #                           return(c(value1, value2, out))
  #                         }
  #                         
  #                         get_value(param_grid[i,1],param_grid[i,2])
  #                       }
  # 
  # 
  # result_oc_ps <- foreach(i = 1:nrow(param_grid_oc), .combine = rbind,
  #                         .options.snow = opts) %dopar% {
  #                           nu_param = seq(0.0001, 0.9999, length.out = 10)
  #                           gamma_param = 2^seq(-10,0, length.out = 10)
  #                           param_grid_oc = as.data.frame(expand.grid(nu_param, gamma_param))
  #                           
  #                           get_value = function(value1, value2){
  #                             library(WeightSVM)
  #                             library(ROCR)
  #                             obj = wsvm(x = x_oc, weight = ps_oc, type = 'one-classification', nu = value1, gamma = value2, scale = FALSE)
  #                             platt_one_svc = function(model){
  #                               res = predict(model, x, type = 'response')
  #                               res = ifelse(as.numeric(res) == 1, 0, 1)
  #                               m1 = glm(y~res, family = binomial(link = 'logit'))
  #                               return(m1$fitted.values)
  #                             }
  #                             p1 = platt_one_svc(obj)
  #                             pr = prediction(p1, y)
  #                             out = performance(pr, 'auc')@y.values[[1]]
  #                             return(c(value1, value2, out))
  #                           }
  #                           
  #                           get_value(param_grid_oc[i,1],param_grid_oc[i,2])
  #                         }
  # 
  # result_svm_ps <- foreach(i = 1:nrow(param_grid), .combine = rbind,
  #                          .options.snow = opts) %dopar% {
  #                            c_param = 2^seq(-10,10, length.out = 10)
  #                            gamma_param = 2^seq(-10,0, length.out = 10)
  #                            param_grid = as.data.frame(expand.grid(c_param, gamma_param))
  #                            
  #                            get_value = function(value1, value2){
  #                              library(WeightSVM)
  #                              library(caret)
  #                              obj = wsvm(x = x, y = y, weight = ps, type = 'C-classification', cost = value1, gamma = value2, scale = FALSE, probability = TRUE)
  #                              p1 = predict(obj, x, probability = TRUE)
  #                              p1 = as.data.frame(attr(p1,"probabilities"))[,2]
  #                              pr = prediction(p1, y)
  #                              prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
  #                              out = performance(pr, 'auc')@y.values[[1]]
  #                              return(c(value1, value2, out))
  #                            }
  #                            
  #                            get_value(param_grid[i,1],param_grid[i,2])
  #                          }
  # 
  # 
  # close(pb)
  # stopCluster(myCluster)
  # 
  # results_ = list()
  # results_[[1]] = result_oc[order(result_oc[,3], decreasing = T),]
  # results_[[2]] = result_svm[order(result_svm[,3], decreasing = T),]
  # 
  # results_[[3]] = result_oc_ps[order(result_oc_ps[,3], decreasing = T),]
  # results_[[4]] = result_svm_ps[order(result_svm_ps[,3], decreasing = T),]
  # 
  # results_rbf = results_
  
  numCores <- parallel::detectCores() - 2
  myCluster <- makeCluster(numCores)
  registerDoSNOW(myCluster)
  
  
  pb = txtProgressBar(max = 100, style = 3)
  progress = function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  nu_param = seq(0.0001, 0.9999, length.out = 100)
  c_param = 2^seq(-10,10, length.out = 100)
  
  
  result_oc <- foreach(i = 1:100, .combine = rbind,
                       .options.snow = opts,
                       .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                         
                         nu_param = seq(0.0001, 0.9999, length.out = 100)
                         
                         get_value = function(value1){
                           library(kernlab)
                           library(ROCR)
                           model = ksvm(x_oc, type = 'one-svc',  scaled = F, nu=value1, prob.model = TRUE, kernel = 'vanilladot')
                           platt_one_svc = function(model){
                             library(caret)
                             res = predict(model, x, type = 'response')
                             res = ifelse(as.numeric(res) == 1, 0, 1)
                             
                             train_control <- trainControl(method = "cv", number = 10)
                             model2 <- train(y ~ res,
                                             data = data.frame(y,res),
                                             trControl = train_control,
                                             method = "glm",
                                             family=binomial())
                             
                             return(predict(model2, res, type = 'prob')[,"1"])
                           }
                           p1 = platt_one_svc(model)
                           pr = prediction(p1, y)
                           out = performance(pr, 'auc')@y.values[[1]]
                           return(c(value1, out))
                         }
                         
                         get_value(nu_param[i])
                       }
  
  
  
  result_svm <- foreach(i = 1:100, .combine = rbind,
                        .options.snow = opts,
                        .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                          c_param = 2^seq(-10,10, length.out = 100)
                          
                          get_value = function(value1){
                            library(kernlab)
                            library(ROCR)
                            model = ksvm(x,y, type = 'C-svc', scaled = F, C=value1,kernel = 'vanilladot',  prob.model = TRUE)
                            p1 = predict(model, x, type = 'probabilities')[,"1"]
                            pr = prediction(p1, y)
                            prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
                            out = performance(pr, 'auc')@y.values[[1]]
                            return(c(value1, out))
                          }
                          
                          get_value(c_param[i])
                        }
  
  
  result_oc_ps <- foreach(i = 1:100, .combine = rbind,
                          .options.snow = opts,
                          .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                            nu_param = seq(0.0001, 0.9999, length.out = 100)
                            
                            get_value = function(value1){
                              library(WeightSVM)
                              library(ROCR)
                              obj = wsvm(x = x_oc, weight = ps_oc, type = 'one-classification', nu = value1, scale = FALSE, kernel = 'linear')
                              platt_one_svc = function(model){
                                library(caret)
                                res = predict(model, x, type = 'response')
                                res = ifelse(as.numeric(res) == 1, 0, 1)
                                
                                train_control <- trainControl(method = "cv", number = 10)
                                model2 <- train(y ~ res,
                                                data = data.frame(y,res),
                                                trControl = train_control,
                                                method = "glm",
                                                family=binomial())
                                
                                return(predict(model2, res, type = 'prob')[,"1"])
                              }
                              p1 = platt_one_svc(obj)
                              pr = prediction(p1, y)
                              out = performance(pr, 'auc')@y.values[[1]]
                              return(c(value1, out))
                            }
                            
                            get_value(nu_param[i])
                          }
  
  result_svm_ps <- foreach(i = 1:100, .combine = rbind,
                           .options.snow = opts,
                           .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                             c_param = 2^seq(-10,10, length.out = 100)
                             
                             get_value = function(value1){
                               library(WeightSVM)
                               library(caret)
                               obj = wsvm(x = x, y = y, weight = ps, type = 'C-classification', cost = value1, kernel = 'linear', scale = FALSE, probability = TRUE)
                               p1 = predict(obj, x, probability = TRUE)
                               p1 = as.data.frame(attr(p1,"probabilities"))[,"1"]
                               pr = prediction(p1, y)
                               prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
                               out = performance(pr, 'auc')@y.values[[1]]
                               return(c(value1, out))
                             }
                             
                             get_value(c_param[i])
                           }
  
  
  close(pb)
  stopCluster(myCluster)
  
  results_ = list()
  results_[[1]] = result_oc[order(result_oc[,2], decreasing = T),]
  results_[[2]] = result_svm[order(result_svm[,2], decreasing = T),]
  
  results_[[3]] = result_oc_ps[order(result_oc_ps[,2], decreasing = T),]
  results_[[4]] = result_svm_ps[order(result_svm_ps[,2], decreasing = T),]
  
  results_svm = results_
  
  mskae_result_svm = results_svm
  # mskae_result_svm_rbf = results_rbf
  
  
  
  y_value = 'osteoporosis'
  
  library(kernlab)
  
  numCores <- parallel::detectCores() - 2
  myCluster <- makeCluster(numCores)
  registerDoSNOW(myCluster)
  
  
  
  
  
  
  
  #install.packages('WeightSVM')
  library(WeightSVM)
  y = as.factor(result[[y_value]])
  x = result$drug_sum
  result_oc = result[result[[y_value]] ==0,]
  ps_oc = ps[result[[y_value]] ==0]
  x_oc = result_oc$drug_sum
  
  # pb = txtProgressBar(max = 100, style = 3)
  # progress = function(n) setTxtProgressBar(pb, n)
  # opts <- list(progress = progress)
  # 
  # nu_param = seq(0.0001, 0.9999, length.out = 10)
  # gamma_param = 2^seq(-10,0, length.out = 10)
  # param_grid_oc = as.data.frame(expand.grid(nu_param, gamma_param))
  # 
  # c_param = 2^seq(-10,10, length.out = 10)
  # gamma_param = 2^seq(-10,0, length.out = 10)
  # param_grid = as.data.frame(expand.grid(c_param, gamma_param))
  # 
  # 
  # 
  # result_oc <- foreach(i = 1:nrow(param_grid_oc), .combine = rbind,
  #                      .options.snow = opts) %dopar% {
  #                        
  #                        nu_param = seq(0.0001, 0.9999, length.out = 10)
  #                        gamma_param = 2^seq(-10,0, length.out = 10)
  #                        param_grid_oc = as.data.frame(expand.grid(nu_param, gamma_param))
  #                        
  #                        get_value = function(value1, value2){
  #                          library(kernlab)
  #                          library(ROCR)
  #                          model = ksvm(x_oc, type = 'one-svc',  scaled = F, nu=value1,kpar = list(sigma = value2), prob.model = TRUE)
  #                          platt_one_svc = function(model){
  #                            library(caret)
  #                            res = predict(model, x, type = 'response')
  #                            res = ifelse(as.numeric(res) == 1, 0, 1)
  #                            
  #                            train_control <- trainControl(method = "cv", number = 10)
  #                            model2 <- train(y ~ res,
  #                                            data = data.frame(y,res),
  #                                            trControl = train_control,
  #                                            method = "glm",
  #                                            family=binomial())
  #                            
  #                            return(predict(model2, res, type = 'prob')[,2])
  #                          }
  #                          p1 = platt_one_svc(model)
  #                          pr = prediction(p1, y)
  #                          out = performance(pr, 'auc')@y.values[[1]]
  #                          return(c(value1, value2, out))
  #                        }
  #                        
  #                        get_value(param_grid_oc[i,1],param_grid_oc[i,2])
  #                      }
  # 
  # 
  # 
  # result_svm <- foreach(i = 1:nrow(param_grid), .combine = rbind,
  #                       .options.snow = opts) %dopar% {
  #                         c_param = 2^seq(-10,10, length.out = 10)
  #                         gamma_param = 2^seq(-10,0, length.out = 10)
  #                         param_grid = as.data.frame(expand.grid(c_param, gamma_param))
  #                         
  #                         get_value = function(value1, value2){
  #                           library(kernlab)
  #                           library(ROCR)
  #                           model = ksvm(x,y, type = 'C-svc', scaled = F, C=value1,kpar = list(sigma = value2),  prob.model = TRUE)
  #                           p1 = predict(model, x, type = 'probabilities')[,2]
  #                           pr = prediction(p1, y)
  #                           prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
  #                           out = performance(pr, 'auc')@y.values[[1]]
  #                           return(c(value1, value2, out))
  #                         }
  #                         
  #                         get_value(param_grid[i,1],param_grid[i,2])
  #                       }
  # 
  # 
  # result_oc_ps <- foreach(i = 1:nrow(param_grid_oc), .combine = rbind,
  #                         .options.snow = opts) %dopar% {
  #                           nu_param = seq(0.0001, 0.9999, length.out = 10)
  #                           gamma_param = 2^seq(-10,0, length.out = 10)
  #                           param_grid_oc = as.data.frame(expand.grid(nu_param, gamma_param))
  #                           
  #                           get_value = function(value1, value2){
  #                             library(WeightSVM)
  #                             library(ROCR)
  #                             obj = wsvm(x = x_oc, weight = ps_oc, type = 'one-classification', nu = value1, gamma = value2, scale = FALSE)
  #                             platt_one_svc = function(model){
  #                               library(caret)
  #                               res = predict(model, x, type = 'response')
  #                               res = ifelse(as.numeric(res) == 1, 0, 1)
  #                               
  #                               train_control <- trainControl(method = "cv", number = 10)
  #                               model2 <- train(y ~ res,
  #                                               data = data.frame(y,res),
  #                                               trControl = train_control,
  #                                               method = "glm",
  #                                               family=binomial())
  #                               
  #                               return(predict(model2, res, type = 'prob')[,2])
  #                             }
  #                             p1 = platt_one_svc(obj)
  #                             pr = prediction(p1, y)
  #                             out = performance(pr, 'auc')@y.values[[1]]
  #                             return(c(value1, value2, out))
  #                           }
  #                           
  #                           get_value(param_grid_oc[i,1],param_grid_oc[i,2])
  #                         }
  # 
  # result_svm_ps <- foreach(i = 1:nrow(param_grid), .combine = rbind,
  #                          .options.snow = opts) %dopar% {
  #                            c_param = 2^seq(-10,10, length.out = 10)
  #                            gamma_param = 2^seq(-10,0, length.out = 10)
  #                            param_grid = as.data.frame(expand.grid(c_param, gamma_param))
  #                            
  #                            get_value = function(value1, value2){
  #                              library(WeightSVM)
  #                              library(caret)
  #                              obj = wsvm(x = x, y = y, weight = ps, type = 'C-classification', cost = value1, gamma = value2, scale = FALSE, probability = TRUE)
  #                              p1 = predict(obj, x, probability = TRUE)
  #                              p1 = as.data.frame(attr(p1,"probabilities"))[,2]
  #                              pr = prediction(p1, y)
  #                              prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
  #                              out = performance(pr, 'auc')@y.values[[1]]
  #                              return(c(value1, value2, out))
  #                            }
  #                            
  #                            get_value(param_grid[i,1],param_grid[i,2])
  #                          }
  # 
  # 
  # close(pb)
  # stopCluster(myCluster)
  # 
  # results_ = list()
  # results_[[1]] = result_oc[order(result_oc[,3], decreasing = T),]
  # results_[[2]] = result_svm[order(result_svm[,3], decreasing = T),]
  # 
  # results_[[3]] = result_oc_ps[order(result_oc_ps[,3], decreasing = T),]
  # results_[[4]] = result_svm_ps[order(result_svm_ps[,3], decreasing = T),]
  # 
  # results_rbf = results_
  
  numCores <- parallel::detectCores() - 2
  myCluster <- makeCluster(numCores)
  registerDoSNOW(myCluster)
  
  
  pb = txtProgressBar(max = 100, style = 3)
  progress = function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  nu_param = seq(0.0001, 0.9999, length.out = 100)
  c_param = 2^seq(-10,10, length.out = 100)
  
  
  result_oc <- foreach(i = 1:100, .combine = rbind,
                       .options.snow = opts,
                       .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                         
                         nu_param = seq(0.0001, 0.9999, length.out = 100)
                         
                         get_value = function(value1){
                           library(kernlab)
                           library(ROCR)
                           model = ksvm(x_oc, type = 'one-svc',  scaled = F, nu=value1, prob.model = TRUE, kernel = 'vanilladot')
                           platt_one_svc = function(model){
                             library(caret)
                             res = predict(model, x, type = 'response')
                             res = ifelse(as.numeric(res) == 1, 0, 1)
                             
                             train_control <- trainControl(method = "cv", number = 10)
                             model2 <- train(y ~ res,
                                             data = data.frame(y,res),
                                             trControl = train_control,
                                             method = "glm",
                                             family=binomial())
                             
                             return(predict(model2, res, type = 'prob')[,"1"])
                           }
                           p1 = platt_one_svc(model)
                           pr = prediction(p1, y)
                           out = performance(pr, 'auc')@y.values[[1]]
                           return(c(value1, out))
                         }
                         
                         get_value(nu_param[i])
                       }
  
  
  
  result_svm <- foreach(i = 1:100, .combine = rbind,
                        .options.snow = opts,
                        .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                          c_param = 2^seq(-10,10, length.out = 100)
                          
                          get_value = function(value1){
                            library(kernlab)
                            library(ROCR)
                            model = ksvm(x,y, type = 'C-svc', scaled = F, C=value1,kernel = 'vanilladot',  prob.model = TRUE)
                            p1 = predict(model, x, type = 'probabilities')[,"1"]
                            pr = prediction(p1, y)
                            prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
                            out = performance(pr, 'auc')@y.values[[1]]
                            return(c(value1, out))
                          }
                          
                          get_value(c_param[i])
                        }
  
  
  result_oc_ps <- foreach(i = 1:100, .combine = rbind,
                          .options.snow = opts,
                          .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                            nu_param = seq(0.0001, 0.9999, length.out = 100)
                            
                            get_value = function(value1){
                              library(WeightSVM)
                              library(ROCR)
                              obj = wsvm(x = x_oc, weight = ps_oc, type = 'one-classification', nu = value1, scale = FALSE, kernel = 'linear')
                              platt_one_svc = function(model){
                                library(caret)
                                res = predict(model, x, type = 'response')
                                res = ifelse(as.numeric(res) == 1, 0, 1)
                                
                                train_control <- trainControl(method = "cv", number = 10)
                                model2 <- train(y ~ res,
                                                data = data.frame(y,res),
                                                trControl = train_control,
                                                method = "glm",
                                                family=binomial())
                                
                                return(predict(model2, res, type = 'prob')[,"1"])
                              }
                              p1 = platt_one_svc(obj)
                              pr = prediction(p1, y)
                              out = performance(pr, 'auc')@y.values[[1]]
                              return(c(value1, out))
                            }
                            
                            get_value(nu_param[i])
                          }
  
  result_svm_ps <- foreach(i = 1:100, .combine = rbind,
                           .options.snow = opts,
                           .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                             c_param = 2^seq(-10,10, length.out = 100)
                             
                             get_value = function(value1){
                               library(WeightSVM)
                               library(caret)
                               obj = wsvm(x = x, y = y, weight = ps, type = 'C-classification', cost = value1, kernel = 'linear', scale = FALSE, probability = TRUE)
                               p1 = predict(obj, x, probability = TRUE)
                               p1 = as.data.frame(attr(p1,"probabilities"))[,"1"]
                               pr = prediction(p1, y)
                               prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
                               out = performance(pr, 'auc')@y.values[[1]]
                               return(c(value1, out))
                             }
                             
                             get_value(c_param[i])
                           }
  
  
  close(pb)
  stopCluster(myCluster)
  
  results_ = list()
  results_[[1]] = result_oc[order(result_oc[,2], decreasing = T),]
  results_[[2]] = result_svm[order(result_svm[,2], decreasing = T),]
  
  results_[[3]] = result_oc_ps[order(result_oc_ps[,2], decreasing = T),]
  results_[[4]] = result_svm_ps[order(result_svm_ps[,2], decreasing = T),]
  
  results_svm = results_
  
  porosis_result_svm = results_svm
  # porosis_result_svm_rbf = results_rbf
  
  
  
  
  
  
  y_value = 'osteonecrosis'
  
  
  # numCores <- parallel::detectCores() - 4
  # myCluster <- makeCluster(numCores)
  # registerDoSNOW(myCluster)
  
  
  y = as.factor(result[[y_value]])
  x = result$drug_sum
  result_oc = result[result[[y_value]] ==0,]
  ps_oc = ps[result[[y_value]] ==0]
  x_oc = result_oc$drug_sum
  # 
  # pb = txtProgressBar(max = 100, style = 3)
  # progress = function(n) setTxtProgressBar(pb, n)
  # opts <- list(progress = progress)
  # 
  # nu_param = seq(0.0001, 0.9999, length.out = 10)
  # gamma_param = 2^seq(-10,0, length.out = 10)
  # param_grid_oc = as.data.frame(expand.grid(nu_param, gamma_param))
  # 
  # c_param = 2^seq(-10,10, length.out = 10)
  # gamma_param = 2^seq(-10,0, length.out = 10)
  # param_grid = as.data.frame(expand.grid(c_param, gamma_param))
  # 
  # 
  # 
  # result_oc <- foreach(i = 1:nrow(param_grid_oc), .combine = rbind,
  #                      .options.snow = opts) %dopar% {
  #                        
  #                        nu_param = seq(0.0001, 0.9999, length.out = 10)
  #                        gamma_param = 2^seq(-10,0, length.out = 10)
  #                        param_grid_oc = as.data.frame(expand.grid(nu_param, gamma_param))
  #                        
  #                        get_value = function(value1, value2){
  #                          library(kernlab)
  #                          library(ROCR)
  #                          model = ksvm(x_oc, type = 'one-svc',  scaled = F, nu=value1,kpar = list(sigma = value2), prob.model = TRUE)
  #                          platt_one_svc = function(model){
  #                            library(caret)
  #                            res = predict(model, x, type = 'response')
  #                            res = ifelse(as.numeric(res) == 1, 0, 1)
  #                            
  #                            train_control <- trainControl(method = "cv", number = 10)
  #                            model2 <- train(y ~ res,
  #                                            data = data.frame(y,res),
  #                                            trControl = train_control,
  #                                            method = "glm",
  #                                            family=binomial())
  #                            
  #                            return(predict(model2, res, type = 'prob')[,2])
  #                          }
  #                          p1 = platt_one_svc(model)
  #                          pr = prediction(p1, y)
  #                          out = performance(pr, 'auc')@y.values[[1]]
  #                          return(c(value1, value2, out))
  #                        }
  #                        
  #                        get_value(param_grid_oc[i,1],param_grid_oc[i,2])
  #                      }
  # 
  # 
  # 
  # result_svm <- foreach(i = 1:nrow(param_grid), .combine = rbind,
  #                       .options.snow = opts) %dopar% {
  #                         c_param = 2^seq(-10,10, length.out = 10)
  #                         gamma_param = 2^seq(-10,0, length.out = 10)
  #                         param_grid = as.data.frame(expand.grid(c_param, gamma_param))
  #                         
  #                         get_value = function(value1, value2){
  #                           library(kernlab)
  #                           library(ROCR)
  #                           model = ksvm(x,y, type = 'C-svc', scaled = F, C=value1,kpar = list(sigma = value2),  prob.model = TRUE)
  #                           p1 = predict(model, x, type = 'probabilities')[,2]
  #                           pr = prediction(p1, y)
  #                           prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
  #                           out = performance(pr, 'auc')@y.values[[1]]
  #                           return(c(value1, value2, out))
  #                         }
  #                         
  #                         get_value(param_grid[i,1],param_grid[i,2])
  #                       }
  # 
  # 
  # result_oc_ps <- foreach(i = 1:nrow(param_grid_oc), .combine = rbind,
  #                         .options.snow = opts) %dopar% {
  #                           nu_param = seq(0.0001, 0.9999, length.out = 10)
  #                           gamma_param = 2^seq(-10,0, length.out = 10)
  #                           param_grid_oc = as.data.frame(expand.grid(nu_param, gamma_param))
  #                           
  #                           get_value = function(value1, value2){
  #                             library(WeightSVM)
  #                             library(ROCR)
  #                             obj = wsvm(x = x_oc, weight = ps_oc, type = 'one-classification', nu = value1, gamma = value2, scale = FALSE)
  #                             platt_one_svc = function(model){
  #                               library(caret)
  #                               res = predict(model, x, type = 'response')
  #                               res = ifelse(as.numeric(res) == 1, 0, 1)
  #                               
  #                               train_control <- trainControl(method = "cv", number = 10)
  #                               model2 <- train(y ~ res,
  #                                               data = data.frame(y,res),
  #                                               trControl = train_control,
  #                                               method = "glm",
  #                                               family=binomial())
  #                               
  #                               return(predict(model2, res, type = 'prob')[,2])
  #                             }
  #                             p1 = platt_one_svc(obj)
  #                             pr = prediction(p1, y)
  #                             out = performance(pr, 'auc')@y.values[[1]]
  #                             return(c(value1, value2, out))
  #                           }
  #                           
  #                           get_value(param_grid_oc[i,1],param_grid_oc[i,2])
  #                         }
  # 
  # result_svm_ps <- foreach(i = 1:nrow(param_grid), .combine = rbind,
  #                          .options.snow = opts) %dopar% {
  #                            c_param = 2^seq(-10,10, length.out = 10)
  #                            gamma_param = 2^seq(-10,0, length.out = 10)
  #                            param_grid = as.data.frame(expand.grid(c_param, gamma_param))
  #                            
  #                            get_value = function(value1, value2){
  #                              library(WeightSVM)
  #                              library(caret)
  #                              obj = wsvm(x = x, y = y, weight = ps, type = 'C-classification', cost = value1, gamma = value2, scale = FALSE, probability = TRUE)
  #                              p1 = predict(obj, x, probability = TRUE)
  #                              p1 = as.data.frame(attr(p1,"probabilities"))[,2]
  #                              pr = prediction(p1, y)
  #                              prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
  #                              out = performance(pr, 'auc')@y.values[[1]]
  #                              return(c(value1, value2, out))
  #                            }
  #                            
  #                            get_value(param_grid[i,1],param_grid[i,2])
  #                          }
  # 
  # 
  # close(pb)
  # stopCluster(myCluster)
  # 
  # results_ = list()
  # results_[[1]] = result_oc[order(result_oc[,3], decreasing = T),]
  # results_[[2]] = result_svm[order(result_svm[,3], decreasing = T),]
  # 
  # results_[[3]] = result_oc_ps[order(result_oc_ps[,3], decreasing = T),]
  # results_[[4]] = result_svm_ps[order(result_svm_ps[,3], decreasing = T),]
  # 
  # results_rbf = results_
  
  numCores <- parallel::detectCores() - 2
  myCluster <- makeCluster(numCores)
  registerDoSNOW(myCluster)
  
  
  pb = txtProgressBar(max = 100, style = 3)
  progress = function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  nu_param = seq(0.0001, 0.9999, length.out = 100)
  c_param = 2^seq(-10,10, length.out = 100)
  
  
  result_oc <- foreach(i = 1:100, .combine = rbind,
                       .options.snow = opts,
                       .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                         
                         nu_param = seq(0.0001, 0.9999, length.out = 100)
                         
                         get_value = function(value1){
                           library(kernlab)
                           library(ROCR)
                           model = ksvm(x_oc, type = 'one-svc',  scaled = F, nu=value1, prob.model = TRUE, kernel = 'vanilladot')
                           platt_one_svc = function(model){
                             library(caret)
                             res = predict(model, x, type = 'response')
                             res = ifelse(as.numeric(res) == 1, 0, 1)
                             
                             train_control <- trainControl(method = "cv", number = 10)
                             model2 <- train(y ~ res,
                                             data = data.frame(y,res),
                                             trControl = train_control,
                                             method = "glm",
                                             family=binomial())
                             
                             return(predict(model2, res, type = 'prob')[,"1"])
                           }
                           p1 = platt_one_svc(model)
                           pr = prediction(p1, y)
                           out = performance(pr, 'auc')@y.values[[1]]
                           return(c(value1, out))
                         }
                         
                         get_value(nu_param[i])
                       }
  
  
  
  result_svm <- foreach(i = 1:100, .combine = rbind,
                        .options.snow = opts,
                        .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                          c_param = 2^seq(-10,10, length.out = 100)
                          
                          get_value = function(value1){
                            library(kernlab)
                            library(ROCR)
                            model = ksvm(x,y, type = 'C-svc', scaled = F, C=value1,kernel = 'vanilladot',  prob.model = TRUE)
                            p1 = predict(model, x, type = 'probabilities')[,"1"]
                            pr = prediction(p1, y)
                            prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
                            out = performance(pr, 'auc')@y.values[[1]]
                            return(c(value1, out))
                          }
                          
                          get_value(c_param[i])
                        }
  
  
  result_oc_ps <- foreach(i = 1:100, .combine = rbind,
                          .options.snow = opts,
                          .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                            nu_param = seq(0.0001, 0.9999, length.out = 100)
                            
                            get_value = function(value1){
                              library(WeightSVM)
                              library(ROCR)
                              obj = wsvm(x = x_oc, weight = ps_oc, type = 'one-classification', nu = value1, scale = FALSE, kernel = 'linear')
                              platt_one_svc = function(model){
                                library(caret)
                                res = predict(model, x, type = 'response')
                                res = ifelse(as.numeric(res) == 1, 0, 1)
                                
                                train_control <- trainControl(method = "cv", number = 10)
                                model2 <- train(y ~ res,
                                                data = data.frame(y,res),
                                                trControl = train_control,
                                                method = "glm",
                                                family=binomial())
                                
                                return(predict(model2, res, type = 'prob')[,"1"])
                              }
                              p1 = platt_one_svc(obj)
                              pr = prediction(p1, y)
                              out = performance(pr, 'auc')@y.values[[1]]
                              return(c(value1, out))
                            }
                            
                            get_value(nu_param[i])
                          }
  
  result_svm_ps <- foreach(i = 1:100, .combine = rbind,
                           .options.snow = opts,
                           .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                             c_param = 2^seq(-10,10, length.out = 100)
                             
                             get_value = function(value1){
                               library(WeightSVM)
                               library(caret)
                               obj = wsvm(x = x, y = y, weight = ps, type = 'C-classification', cost = value1, kernel = 'linear', scale = FALSE, probability = TRUE)
                               p1 = predict(obj, x, probability = TRUE)
                               p1 = as.data.frame(attr(p1,"probabilities"))[,"1"]
                               pr = prediction(p1, y)
                               prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
                               out = performance(pr, 'auc')@y.values[[1]]
                               return(c(value1, out))
                             }
                             
                             get_value(c_param[i])
                           }
  
  
  close(pb)
  stopCluster(myCluster)
  
  results_ = list()
  results_[[1]] = result_oc[order(result_oc[,2], decreasing = T),]
  results_[[2]] = result_svm[order(result_svm[,2], decreasing = T),]
  
  results_[[3]] = result_oc_ps[order(result_oc_ps[,2], decreasing = T),]
  results_[[4]] = result_svm_ps[order(result_svm_ps[,2], decreasing = T),]
  
  results_svm = results_
  
  necrosis_result_svm = results_svm
  # necrosis_result_svm_rbf = results_rbf
  
  
  y_value = 'pathologicfrac'
  
  # 
  # numCores <- parallel::detectCores() - 4
  # myCluster <- makeCluster(numCores)
  # registerDoSNOW(myCluster)
  
  
  y = as.factor(result[[y_value]])
  x = result$drug_sum
  result_oc = result[result[[y_value]] ==0,]
  ps_oc = ps[result[[y_value]] ==0]
  x_oc = result_oc$drug_sum
  # 
  # pb = txtProgressBar(max = 100, style = 3)
  # progress = function(n) setTxtProgressBar(pb, n)
  # opts <- list(progress = progress)
  # 
  # nu_param = seq(0.0001, 0.9999, length.out = 10)
  # gamma_param = 2^seq(-10,0, length.out = 10)
  # param_grid_oc = as.data.frame(expand.grid(nu_param, gamma_param))
  # 
  # c_param = 2^seq(-10,10, length.out = 10)
  # gamma_param = 2^seq(-10,0, length.out = 10)
  # param_grid = as.data.frame(expand.grid(c_param, gamma_param))
  # 
  # 
  # 
  # result_oc <- foreach(i = 1:nrow(param_grid_oc), .combine = rbind,
  #                      .options.snow = opts) %dopar% {
  #                        
  #                        nu_param = seq(0.0001, 0.9999, length.out = 10)
  #                        gamma_param = 2^seq(-10,0, length.out = 10)
  #                        param_grid_oc = as.data.frame(expand.grid(nu_param, gamma_param))
  #                        
  #                        get_value = function(value1, value2){
  #                          library(kernlab)
  #                          library(ROCR)
  #                          model = ksvm(x_oc, type = 'one-svc',  scaled = F, nu=value1,kpar = list(sigma = value2), prob.model = TRUE)
  #                          platt_one_svc = function(model){
  #                            library(caret)
  #                            res = predict(model, x, type = 'response')
  #                            res = ifelse(as.numeric(res) == 1, 0, 1)
  #                            
  #                            train_control <- trainControl(method = "cv", number = 10)
  #                            model2 <- train(y ~ res,
  #                                            data = data.frame(y,res),
  #                                            trControl = train_control,
  #                                            method = "glm",
  #                                            family=binomial())
  #                            
  #                            return(predict(model2, res, type = 'prob')[,2])
  #                          }
  #                          p1 = platt_one_svc(model)
  #                          pr = prediction(p1, y)
  #                          out = performance(pr, 'auc')@y.values[[1]]
  #                          return(c(value1, value2, out))
  #                        }
  #                        
  #                        get_value(param_grid_oc[i,1],param_grid_oc[i,2])
  #                      }
  # 
  # 
  # 
  # result_svm <- foreach(i = 1:nrow(param_grid), .combine = rbind,
  #                       .options.snow = opts) %dopar% {
  #                         c_param = 2^seq(-10,10, length.out = 10)
  #                         gamma_param = 2^seq(-10,0, length.out = 10)
  #                         param_grid = as.data.frame(expand.grid(c_param, gamma_param))
  #                         
  #                         get_value = function(value1, value2){
  #                           library(kernlab)
  #                           library(ROCR)
  #                           model = ksvm(x,y, type = 'C-svc', scaled = F, C=value1,kpar = list(sigma = value2),  prob.model = TRUE)
  #                           p1 = predict(model, x, type = 'probabilities')[,2]
  #                           pr = prediction(p1, y)
  #                           prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
  #                           out = performance(pr, 'auc')@y.values[[1]]
  #                           return(c(value1, value2, out))
  #                         }
  #                         
  #                         get_value(param_grid[i,1],param_grid[i,2])
  #                       }
  # 
  # 
  # result_oc_ps <- foreach(i = 1:nrow(param_grid_oc), .combine = rbind,
  #                         .options.snow = opts) %dopar% {
  #                           nu_param = seq(0.0001, 0.9999, length.out = 10)
  #                           gamma_param = 2^seq(-10,0, length.out = 10)
  #                           param_grid_oc = as.data.frame(expand.grid(nu_param, gamma_param))
  #                           
  #                           get_value = function(value1, value2){
  #                             library(WeightSVM)
  #                             library(ROCR)
  #                             obj = wsvm(x = x_oc, weight = ps_oc, type = 'one-classification', nu = value1, gamma = value2, scale = FALSE)
  #                             platt_one_svc = function(model){
  #                               library(caret)
  #                               res = predict(model, x, type = 'response')
  #                               res = ifelse(as.numeric(res) == 1, 0, 1)
  #                               
  #                               train_control <- trainControl(method = "cv", number = 10)
  #                               model2 <- train(y ~ res,
  #                                               data = data.frame(y,res),
  #                                               trControl = train_control,
  #                                               method = "glm",
  #                                               family=binomial())
  #                               
  #                               return(predict(model2, res, type = 'prob')[,2])
  #                             }
  #                             p1 = platt_one_svc(obj)
  #                             pr = prediction(p1, y)
  #                             out = performance(pr, 'auc')@y.values[[1]]
  #                             return(c(value1, value2, out))
  #                           }
  #                           
  #                           get_value(param_grid_oc[i,1],param_grid_oc[i,2])
  #                         }
  # 
  # result_svm_ps <- foreach(i = 1:nrow(param_grid), .combine = rbind,
  #                          .options.snow = opts) %dopar% {
  #                            c_param = 2^seq(-10,10, length.out = 10)
  #                            gamma_param = 2^seq(-10,0, length.out = 10)
  #                            param_grid = as.data.frame(expand.grid(c_param, gamma_param))
  #                            
  #                            get_value = function(value1, value2){
  #                              library(WeightSVM)
  #                              library(caret)
  #                              obj = wsvm(x = x, y = y, weight = ps, type = 'C-classification', cost = value1, gamma = value2, scale = FALSE, probability = TRUE)
  #                              p1 = predict(obj, x, probability = TRUE)
  #                              p1 = as.data.frame(attr(p1,"probabilities"))[,2]
  #                              pr = prediction(p1, y)
  #                              prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
  #                              out = performance(pr, 'auc')@y.values[[1]]
  #                              return(c(value1, value2, out))
  #                            }
  #                            
  #                            get_value(param_grid[i,1],param_grid[i,2])
  #                          }
  # 
  # 
  # close(pb)
  # stopCluster(myCluster)
  # 
  # results_ = list()
  # results_[[1]] = result_oc[order(result_oc[,3], decreasing = T),]
  # results_[[2]] = result_svm[order(result_svm[,3], decreasing = T),]
  # 
  # results_[[3]] = result_oc_ps[order(result_oc_ps[,3], decreasing = T),]
  # results_[[4]] = result_svm_ps[order(result_svm_ps[,3], decreasing = T),]
  # 
  # results_rbf = results_
  
  numCores <- parallel::detectCores() - 2
  myCluster <- makeCluster(numCores)
  registerDoSNOW(myCluster)
  
  
  pb = txtProgressBar(max = 100, style = 3)
  progress = function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  nu_param = seq(0.0001, 0.9999, length.out = 100)
  c_param = 2^seq(-10,10, length.out = 100)
  
  
  result_oc <- foreach(i = 1:100, .combine = rbind,
                       .options.snow = opts,
                       .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                         
                         nu_param = seq(0.0001, 0.9999, length.out = 100)
                         
                         get_value = function(value1){
                           library(kernlab)
                           library(ROCR)
                           model = ksvm(x_oc, type = 'one-svc',  scaled = F, nu=value1, prob.model = TRUE, kernel = 'vanilladot')
                           platt_one_svc = function(model){
                             library(caret)
                             res = predict(model, x, type = 'response')
                             res = ifelse(as.numeric(res) == 1, 0, 1)
                             
                             train_control <- trainControl(method = "cv", number = 10)
                             model2 <- train(y ~ res,
                                             data = data.frame(y,res),
                                             trControl = train_control,
                                             method = "glm",
                                             family=binomial())
                             
                             return(predict(model2, res, type = 'prob')[,"1"])
                           }
                           p1 = platt_one_svc(model)
                           pr = prediction(p1, y)
                           out = performance(pr, 'auc')@y.values[[1]]
                           return(c(value1, out))
                         }
                         
                         get_value(nu_param[i])
                       }
  
  
  
  result_svm <- foreach(i = 1:100, .combine = rbind,
                        .options.snow = opts,
                        .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                          c_param = 2^seq(-10,10, length.out = 100)
                          
                          get_value = function(value1){
                            library(kernlab)
                            library(ROCR)
                            model = ksvm(x,y, type = 'C-svc', scaled = F, C=value1,kernel = 'vanilladot',  prob.model = TRUE)
                            p1 = predict(model, x, type = 'probabilities')[,"1"]
                            pr = prediction(p1, y)
                            prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
                            out = performance(pr, 'auc')@y.values[[1]]
                            return(c(value1, out))
                          }
                          
                          get_value(c_param[i])
                        }
  
  
  result_oc_ps <- foreach(i = 1:100, .combine = rbind,
                          .options.snow = opts,
                          .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                            nu_param = seq(0.0001, 0.9999, length.out = 100)
                            
                            get_value = function(value1){
                              library(WeightSVM)
                              library(ROCR)
                              obj = wsvm(x = x_oc, weight = ps_oc, type = 'one-classification', nu = value1, scale = FALSE, kernel = 'linear')
                              platt_one_svc = function(model){
                                library(caret)
                                res = predict(model, x, type = 'response')
                                res = ifelse(as.numeric(res) == 1, 0, 1)
                                
                                train_control <- trainControl(method = "cv", number = 10)
                                model2 <- train(y ~ res,
                                                data = data.frame(y,res),
                                                trControl = train_control,
                                                method = "glm",
                                                family=binomial())
                                
                                return(predict(model2, res, type = 'prob')[,"1"])
                              }
                              p1 = platt_one_svc(obj)
                              pr = prediction(p1, y)
                              out = performance(pr, 'auc')@y.values[[1]]
                              return(c(value1, out))
                            }
                            
                            get_value(nu_param[i])
                          }
  
  result_svm_ps <- foreach(i = 1:100, .combine = rbind,
                           .options.snow = opts,
                           .export = c("x","y","x_oc","ps","ps_oc")) %dopar% {
                             c_param = 2^seq(-10,10, length.out = 100)
                             
                             get_value = function(value1){
                               library(WeightSVM)
                               library(caret)
                               obj = wsvm(x = x, y = y, weight = ps, type = 'C-classification', cost = value1, kernel = 'linear', scale = FALSE, probability = TRUE)
                               p1 = predict(obj, x, probability = TRUE)
                               p1 = as.data.frame(attr(p1,"probabilities"))[,"1"]
                               pr = prediction(p1, y)
                               prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
                               out = performance(pr, 'auc')@y.values[[1]]
                               return(c(value1, out))
                             }
                             
                             get_value(c_param[i])
                           }
  
  
  close(pb)
  stopCluster(myCluster)
  
  results_ = list()
  results_[[1]] = result_oc[order(result_oc[,2], decreasing = T),]
  results_[[2]] = result_svm[order(result_svm[,2], decreasing = T),]
  
  results_[[3]] = result_oc_ps[order(result_oc_ps[,2], decreasing = T),]
  results_[[4]] = result_svm_ps[order(result_svm_ps[,2], decreasing = T),]
  
  results_svm = results_
  
  
  frac_result_svm = results_svm
  # frac_result_svm_rbf = results_rbf
  
  
  unregister_dopar <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
  }
  
  unregister_dopar()
  
  #vanilla svm
  
  svm_output = function(result_svm, y_output){
    
    x_test = result_test$drug_sum
    y_test = result_test[[y_output]]
    test_output = list()
    
    optimal = result_svm[[2]][1,1]
    y = as.factor(result[[y_output]])
    auc1 = result_svm[[1]][1,2]
    model = ksvm(x,y, type = 'C-svc', scaled = F, C=optimal, kernel = 'vanilladot',  prob.model = TRUE)
    p1 = predict(model, x, type = 'probabilities')[,"1"]
    pr = prediction(p1, y)
    prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
    
    ppv_npv <- performance(pr, measure = "ppv", x.measure = "npv")
    ppv_npv = data.frame(ppv_npv@alpha.values, ppv_npv@y.values, ppv_npv@x.values)
    colnames(ppv_npv) = c('cutoff','ppv','npv')
    
    spec_fr = performance(pr, "spec")
    spec_fr = data.frame(spec_fr@x.values, spec_fr@y.values)
    colnames(spec_fr) = c('cutoff','specificity')
    
    write.csv(merge(spec_fr, ppv_npv, by = 'cutoff'),paste(y_output, '_result_svm_ml.csv'))
    png(filename = paste(y_output, '_result_svm.png',sep=''), width = 800,height = 1600)
    par(mfrow = c(4,2))
    plot(prf, main = 'SVM')
    youden = performance(pr, 'sens')@y.values[[1]] + performance(pr, 'spec')@y.values[[1]] - 1
    plot(x = prf@alpha.values[[1]], y = youden, type = 'l', main = 'Youden Index', xlab = 'Cutoff')
    cutoff1 = prf@alpha.values[[1]][youden == max(youden)]
    value1 = max(exp(x[p1 == cutoff1]))
    
    enu_out = list()
    enu = seq(0.1,0.9,by = 0.1)
    for (enu_iter in 1:length(enu)){
      cf = confusionMatrix(as.factor(ifelse(p1 > enu[enu_iter], 1,0)),y)
      tmp2 = t(data.frame(c(enu[enu_iter],cf[[4]])))
      colnames(tmp2)[1] = 'Cutoff'
      enu_out[[enu_iter]] = as.data.frame(tmp2)
    }
    cf_test = confusionMatrix(as.factor(ifelse(predict(model, x_test, type = 'probabilities')[,2] > cutoff1, 1,0)),as.factor(y_test))
    test_output[[1]] = t(data.frame(c('svm',cf_test[[4]])))
    
    write.csv(rbindlist(enu_out), paste(y_output,'_cutoff_diagnosis_svm_ml.csv',sep=""))
    
    
    optimal = result_svm[[4]][1,1]
    auc2 = result_svm[[4]][1,2]
    obj = wsvm(x = x, y = y, weight = ps, type = 'C-classification', cost = optimal, scale = FALSE, probability = TRUE, kernel = 'linear')
    p1 = predict(obj, x, probability = TRUE)
    p1 = as.data.frame(attr(p1,"probabilities"))[,"1"]
    pr = prediction(p1, y)
    prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
    
    ppv_npv <- performance(pr, measure = "ppv", x.measure = "npv")
    ppv_npv = data.frame(ppv_npv@alpha.values, ppv_npv@y.values, ppv_npv@x.values)
    colnames(ppv_npv) = c('cutoff','ppv','npv')
    
    spec_fr = performance(pr, "spec")
    spec_fr = data.frame(spec_fr@x.values, spec_fr@y.values)
    colnames(spec_fr) = c('cutoff','specificity')
    
    write.csv(merge(spec_fr, ppv_npv, by = 'cutoff'),paste(y_output, '_result_svm_ps_ml.csv'))
    
    plot(prf, main = 'WSVM')
    youden = performance(pr, 'sens')@y.values[[1]] + performance(pr, 'spec')@y.values[[1]] - 1
    plot(x = prf@alpha.values[[1]], y = youden, type = 'l')
    cutoff2 = prf@alpha.values[[1]][youden == max(youden)]
    value2 = max(exp(x[p1 == cutoff2]))
    enu_out = list()
    enu = seq(0.1,0.9,by = 0.1)
    for (enu_iter in 1:length(enu)){
      cf = confusionMatrix(as.factor(ifelse(p1 > enu[enu_iter], 1,0)),y)
      tmp2 = t(data.frame(c(enu[enu_iter],cf[[4]])))
      colnames(tmp2)[1] = 'Cutoff'
      enu_out[[enu_iter]] = as.data.frame(tmp2)
    }
    
    p2 = predict(obj, x_test, probability = TRUE)
    p2 = as.data.frame(attr(p2,"probabilities"))[,2]
    cf_test = confusionMatrix(as.factor(ifelse(p2 > cutoff2, 1,0)),as.factor(y_test))
    test_output[[2]] = t(data.frame(c('wsvm',cf_test[[4]])))
    
    write.csv(rbindlist(enu_out), paste(y_output,'_cutoff_diagnosis_svm_ps_ml.csv',sep=""))
    
    #one-class svm
    
    optimal = result_svm[[1]][1,1]
    auc3 = result_svm[[1]][1,2]
    y = as.factor(result[[y_output]])
    result_oc = result[result[[y_output]] ==0,]
    ps_oc = ps[result[[y_output]] ==0]
    x_oc = result_oc$drug_sum
    model = ksvm(x_oc, type = 'one-svc', scaled = F, C=optimal, kernel = 'vanilladot',  prob.model = TRUE)
    platt_one_svc = function(model){
      library(caret)
      res = predict(model, x, type = 'response')
      res = ifelse(as.numeric(res) == 1, 0, 1)
      
      train_control <- trainControl(method = "cv", number = 10)
      model2 <- train(y ~ res,
                      data = data.frame(y,res),
                      trControl = train_control,
                      method = "glm",
                      family=binomial())
      
      return(predict(model2, res, type = 'prob')[,"1"])
    }
    p1 = platt_one_svc(model)
    pr = prediction(p1, y)
    prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
    
    ppv_npv <- performance(pr, measure = "ppv", x.measure = "npv")
    ppv_npv = data.frame(ppv_npv@alpha.values, ppv_npv@y.values, ppv_npv@x.values)
    colnames(ppv_npv) = c('cutoff','ppv','npv')
    
    spec_fr = performance(pr, "spec")
    spec_fr = data.frame(spec_fr@x.values, spec_fr@y.values)
    colnames(spec_fr) = c('cutoff','specificity')
    
    write.csv(merge(spec_fr, ppv_npv, by = 'cutoff'),paste(y_output, '_result_ocsvm_ml.csv'))
    
    plot(prf, main = 'OSSVM')
    youden = performance(pr, 'sens')@y.values[[1]] + performance(pr, 'spec')@y.values[[1]] - 1
    plot(x = prf@alpha.values[[1]], y = youden, type = 'l', main = 'Youden Index', xlab = 'Cutoff')
    cutoff3 = prf@alpha.values[[1]][youden == max(youden)]
    value3 = max(exp(x[p1 == cutoff3]))
    enu_out = list()
    enu = seq(0.1,0.9,by = 0.1)
    for (enu_iter in 1:length(enu)){
      cf = confusionMatrix(as.factor(ifelse(p1 > enu[enu_iter], 1,0)),y)
      tmp2 = t(data.frame(c(enu[enu_iter],cf[[4]])))
      colnames(tmp2)[1] = 'Cutoff'
      enu_out[[enu_iter]] = as.data.frame(tmp2)
    }
    write.csv(rbindlist(enu_out), paste(y_output,'_cutoff_diagnosis_ocsvm_ml.csv',sep=""))
    
    platt_one_svc_test = function(model){
      library(caret)
      res = predict(model, x, type = 'response')
      res = ifelse(as.numeric(res) == 1, 0, 1)
      
      train_control <- trainControl(method = "cv", number = 10)
      model2 <- train(y ~ res,
                      data = data.frame(y,res),
                      trControl = train_control,
                      method = "glm",
                      family=binomial())
      res2 = predict(model, x_test, type = 'response')
      res2 = ifelse(as.numeric(res2) == 1, 0, 1)
      return(predict(model2, newdata = data.frame(res = res2), type = 'prob')[,"1"])
    }
    p2 = platt_one_svc_test(model)
    cf_test = confusionMatrix(as.factor(ifelse(p2 > cutoff3, 1,0)),as.factor(y_test))
    test_output[[3]] = t(data.frame(c('ocsvm',cf_test[[4]])))
    
    
    optimal = result_svm[[3]][1,1]
    auc4 = result_svm[[3]][1,2]
    obj = wsvm(x = x_oc, weight = ps_oc, type = 'one-classification', nu = optimal, scale = FALSE, kernel = 'linear')
    platt_one_svc = function(model){
      res = predict(model, x, type = 'response')
      res = ifelse(as.numeric(res) == 1, 0, 1)
      
      train_control <- trainControl(method = "cv", number = 10)
      model2 <- train(y ~ res,
                      data = data.frame(y,res),
                      trControl = train_control,
                      method = "glm",
                      family=binomial())
      
      return(predict(model2, res, type = 'prob')[,"1"])
    }
    p1 = platt_one_svc(obj)
    pr = prediction(p1, y)
    prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
    
    ppv_npv <- performance(pr, measure = "ppv", x.measure = "npv")
    ppv_npv = data.frame(ppv_npv@alpha.values, ppv_npv@y.values, ppv_npv@x.values)
    colnames(ppv_npv) = c('cutoff','ppv','npv')
    
    spec_fr = performance(pr, "spec")
    spec_fr = data.frame(spec_fr@x.values, spec_fr@y.values)
    colnames(spec_fr) = c('cutoff','specificity')
    
    write.csv(merge(spec_fr, ppv_npv, by = 'cutoff'),paste(y_output, '_result_ocsvm_ps_ml.csv'))
    
    plot(prf, main = 'OC-WSVM')
    youden = performance(pr, 'sens')@y.values[[1]] + performance(pr, 'spec')@y.values[[1]] - 1
    plot(x = prf@alpha.values[[1]], y = youden, type = 'l', main = 'Youden Index', xlab = 'Cutoff')
    cutoff4 = prf@alpha.values[[1]][youden == max(youden)]
    value4 = max(exp(x[p1 == cutoff4]))
    
    platt_one_svc_test = function(model){
      library(caret)
      res = predict(model, x, type = 'response')
      res = ifelse(as.numeric(res) == 1, 0, 1)
      
      train_control <- trainControl(method = "cv", number = 10)
      model2 <- train(y ~ res,
                      data = data.frame(y,res),
                      trControl = train_control,
                      method = "glm",
                      family=binomial())
      res2 = predict(model, x_test, type = 'response')
      res2 = ifelse(as.numeric(res2) == 1, 0, 1)
      return(predict(model2, newdata = data.frame(res = res2), type = 'prob')[,"1"])
    }
    p2 = platt_one_svc_test(obj)
    cf_test = confusionMatrix(as.factor(ifelse(p2 > cutoff4, 1,0)),as.factor(y_test))
    test_output[[4]] = t(data.frame(c('oc-wsvm',cf_test[[4]])))
    
    
    enu_out = list()
    enu = seq(0.1,0.9,by = 0.1)
    for (enu_iter in 1:length(enu)){
      cf = confusionMatrix(as.factor(ifelse(p1 > enu[enu_iter], 1,0)),y)
      tmp2 = t(data.frame(c(enu[enu_iter],cf[[4]])))
      colnames(tmp2)[1] = 'Cutoff'
      enu_out[[enu_iter]] = as.data.frame(tmp2)
    }
    write.csv(rbindlist(enu_out), paste(y_output,'_cutoff_diagnosis_ocsvm_ps_ml.csv',sep=""))
    
    dev.off()
    titles = c('SVM','WSVM','OCSVM','OC-WSVM')
    aucs = c(auc1, auc2, auc3, auc4)
    cutoffs = c(cutoff1, cutoff2, cutoff3, cutoff4)
    values = c(value1, value2, value3, value4)
    for (items in 1:4){
      test_output[[items]] = as.data.frame(test_output[[items]])
    }
    test_output = rbindlist(test_output)
    write.csv(test_output, paste(y_output,'_svm_ml_test.csv',sep=""))
    write.csv(as.data.frame(rbind(titles, aucs, cutoffs, values)),paste(y_output, '_result_svm_all_ml.csv',sep=''))
    
    return(rbind(titles, aucs, cutoffs, values))
  }
  
  svm_output(mskae_result_svm,'mskAE')
  svm_output(porosis_result_svm,'osteoporosis')
  svm_output(necrosis_result_svm,'osteonecrosis')
  svm_output(frac_result_svm,'pathologicfrac')
  
  
  steroid_analysis_weight = function(target, ps, target_test, min_dose = 0, end_dose = Inf, pulse = F, output_name = 'output', weight = ps){
    target = cbind(target, ps)
    output = c(NA, NA, NA, NA)
    output_p = c(NA, NA, NA, NA)
    output_n = c(NA, NA, NA, NA)
    output_all = c(NA, NA, NA, NA)
    output_auc = c(NA, NA, NA, NA)
    test_output = list()
    
    if (pulse == F){
      target = target[target$day_average > min_dose & target$day_average <= end_dose,]
    } else {
      target = target[target$is_pulse == T,]
    }
    
    jpeg(paste(output_name,'.jpg',sep=''),width = 1880, height = 1000)
    par(mfrow = c(4,5))
    
    output_n[1] = sum(target$mskAE)
    if (output_n[1] != 0){
      hist(unlist(target[target$mskAE == 1,"drug_sum_original"]),main = 'mskAE', xlab = 'Prednisone (mg)')
      hist(unlist(target[target$mskAE == 1,"drug_sum"]),main = 'Bonefracture (log)', xlab = 'Prednisone log(mg)')
      target$mskAE = factor(target$mskAE)
      
      design.ps <- svydesign(ids=~1, weights=~target$ps, data=target)
      
      
      model = svyglm(mskAE ~ drug_sum, design = design.ps, family = binomial(link = 'logit'))
      
      
      #exp(model$coefficients)
      p <- predict(model, newdata=target, type="response")
      p = as.data.frame(p)$response
      pr <- prediction(p, target$mskAE)
      prf <- performance(pr, measure = "tpr", x.measure = "fpr")
      plot(prf, main = "ROC Curve")
      output_auc[1] = performance(pr, "auc")@y.values[[1]][1]
      
      predictions = pr
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
      
      enu_out = list()
      enu = seq(0.1,0.9,by = 0.1)
      for (enu_iter in 1:length(enu)){
        cf = confusionMatrix(as.factor(ifelse(p > enu[enu_iter], 1,0)),target$mskAE)
        tmp2 = t(data.frame(c(enu[enu_iter],cf[[4]])))
        colnames(tmp2)[1] = 'Cutoff'
        enu_out[[enu_iter]] = as.data.frame(tmp2)
      }
      write.csv(rbindlist(enu_out), paste(output_name,'_cutoff_diagnosis_mskAE_weighted_ml.csv',sep=""))
      
      #1/(1+exp(-(model$coefficients[2]*300+model$coefficients[1])))
      p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
      out = (log(p/(1-p)) - coef(model)[1])/coef(model)[2]
      
      p_test <- predict(model, newdata=target_test, type="response")
      p_test = as.data.frame(p_test)$response
      cf_test = confusionMatrix(as.factor(ifelse(p_test > p, 1,0)),as.factor(target_test$mskAE))
      test_output[[1]] = t(data.frame(c('mskAE',cf_test[[4]])))
      
      
      
      X1_range = seq(from=min(target$drug_sum), to=max(target$drug_sum)*1.05, by=0.1)
      logits = coef(model)[1]+coef(model)[2]*X1_range
      probs = exp(logits)/(1 + exp(logits))
      plot(exp(X1_range), probs, type = 'l',ylim = c(0,1), main = 'Probablity Plot', xlab = 'Prednisone (mg)', ylab = 'Probablity')
      abline(h = p, col = 'red', lty = 'dashed')
      
      
      
      output_p[1] = p
      output[1] = exp(out)
      output_all[1] = nrow(target)
    }
    
    output_n[2] = sum(target$osteoporosis)
    if (output_n[2] != 0){
      hist(unlist(target[target$osteoporosis == 1,"drug_sum_original"]),main = 'osteoporosis', xlab = 'Prednisone (mg)')
      hist(unlist(target[target$osteoporosis == 1,"drug_sum"]),main = 'Bonefracture (log)', xlab = 'Prednisone log(mg)')
      target$osteoporosis = factor(target$osteoporosis)
      
      design.ps <- svydesign(ids=~1, weights=~target$ps, data=target)
      model = svyglm(osteoporosis ~ drug_sum, design = design.ps, family = binomial(link = 'logit'))
      
      
      #exp(model$coefficients)
      p <- predict(model, newdata=target, type="response")
      p = as.data.frame(p)$response
      pr <- prediction(p, target$osteoporosis)
      prf <- performance(pr, measure = "tpr", x.measure = "fpr")
      plot(prf, main = "ROC Curve")
      output_auc[2] = performance(pr, "auc")@y.values[[1]][1]
      
      predictions = pr
      plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values),
           type="l", lwd=1, ylab="Sensitivity", xlab="Cutoff")
      par(new=TRUE)
      plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values),
           type="l", lwd=1, col='red', ylab="", xlab="", main = "Cutoff Analysis")
      #axis(4, at=seq(0,1,0.2),labels=z)
      mtext("Specificity",side=4, padj=-2, col='red')
      
      sens = cbind(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values))
      spec = cbind(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values))
      
      enu_out = list()
      enu = seq(0.1,0.9,by = 0.1)
      for (enu_iter in 1:length(enu)){
        cf = confusionMatrix(as.factor(ifelse(p > enu[enu_iter], 1,0)),target$osteoporosis)
        tmp2 = t(data.frame(c(enu[enu_iter],cf[[4]])))
        colnames(tmp2)[1] = 'Cutoff'
        enu_out[[enu_iter]] = as.data.frame(tmp2)
      }
      write.csv(rbindlist(enu_out), paste(output_name,'_cutoff_diagnosis_osteoporosis_weighted_ml.csv',sep=""))
      
      morp = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
      
      #1/(1+exp(-(model$coefficients[2]*300+model$coefficients[1])))
      p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
      out = (log(p/(1-p)) - coef(model)[1])/coef(model)[2]
      
      p_test <- predict(model, newdata=target_test, type="response")
      p_test = as.data.frame(p_test)$response
      cf_test = confusionMatrix(as.factor(ifelse(p_test > p, 1,0)),as.factor(target_test$osteoporosis))
      test_output[[2]] = t(data.frame(c('mskAE',cf_test[[4]])))
      
      X1_range = seq(from=min(target$drug_sum), to=max(target$drug_sum)*1.05, by=0.1)
      logits = coef(model)[1]+coef(model)[2]*X1_range
      probs = exp(logits)/(1 + exp(logits))
      plot(exp(X1_range), probs, type = 'l',ylim = c(0,1), main = 'Probablity Plot', xlab = 'Prednisone (mg)', ylab = 'Probablity')
      abline(h = p, col = 'red', lty = 'dashed')
      
      output_p[2] = p
      output[2] = exp(out)
      output_all[2] = nrow(target)
    }
    
    
    output_n[3] = sum(target$osteonecrosis)
    if (output_n[3] != 0){
      hist(unlist(target[target$osteonecrosis == 1,"drug_sum_original"]),main = 'osteonecrosis', xlab = 'Prednisone (mg)')
      hist(unlist(target[target$osteonecrosis == 1,"drug_sum"]),main = 'Bonefracture (log)', xlab = 'Prednisone log(mg)')
      target$osteonecrosis = factor(target$osteonecrosis)
      
      design.ps <- svydesign(ids=~1, weights=~target$ps, data=target)
      model = svyglm(osteonecrosis ~ drug_sum, design = design.ps, family = binomial(link = 'logit'))
      
      
      #exp(model$coefficients)
      p <- predict(model, newdata=target, type="response")
      p = as.data.frame(p)$response
      pr <- prediction(p, target$osteonecrosis)
      prf <- performance(pr, measure = "tpr", x.measure = "fpr")
      plot(prf, main = "ROC Curve")
      output_auc[3] = performance(pr, "auc")@y.values[[1]][1]
      
      predictions = pr
      plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values),
           type="l", lwd=1, ylab="Sensitivity", xlab="Cutoff")
      par(new=TRUE)
      plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values),
           type="l", lwd=1, col='red', ylab="", xlab="", main = "Cutoff Analysis")
      #axis(4, at=seq(0,1,0.2),labels=z)
      mtext("Specificity",side=4, padj=-2, col='red')
      
      sens = cbind(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values))
      spec = cbind(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values))
      
      enu_out = list()
      enu = seq(0.1,0.9,by = 0.1)
      for (enu_iter in 1:length(enu)){
        cf = confusionMatrix(as.factor(ifelse(p > enu[enu_iter], 1,0)),target$osteonecrosis)
        tmp2 = t(data.frame(c(enu[enu_iter],cf[[4]])))
        colnames(tmp2)[1] = 'Cutoff'
        enu_out[[enu_iter]] = as.data.frame(tmp2)
      }
      write.csv(rbindlist(enu_out), paste(output_name,'_cutoff_diagnosis_osteonecrosis_weighted_ml.csv',sep=""))
      
      morp = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
      
      #1/(1+exp(-(model$coefficients[2]*300+model$coefficients[1])))
      p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
      out = (log(p/(1-p)) - coef(model)[1])/coef(model)[2]
      
      p_test <- predict(model, newdata=target_test, type="response")
      p_test = as.data.frame(p_test)$response
      cf_test = confusionMatrix(as.factor(ifelse(p_test > p, 1,0)),as.factor(target_test$osteonecrosis))
      test_output[[3]] = t(data.frame(c('osteonecrosis',cf_test[[4]])))
      
      X1_range = seq(from=min(target$drug_sum), to=max(target$drug_sum)*1.05, by=0.1)
      logits = coef(model)[1]+coef(model)[2]*X1_range
      probs = exp(logits)/(1 + exp(logits))
      plot(exp(X1_range), probs, type = 'l',ylim = c(0,1), main = 'Probablity Plot', xlab = 'Prednisone (mg)', ylab = 'Probablity')
      abline(h = p, col = 'red', lty = 'dashed')
      
      output_p[3] = p
      output[3] = exp(out)
      output_all[3] = nrow(target)
    }
    
    output_n[4] = sum(target$pathologicfrac)
    if (output_n[4] != 0){
      hist(unlist(target[target$pathologicfrac == 1,"drug_sum_original"]),main = 'pathologicfrac', xlab = 'Prednisone (mg)')
      hist(unlist(target[target$pathologicfrac == 1,"drug_sum"]),main = 'Bonefracture (log)', xlab = 'Prednisone log(mg)')
      target$pathologicfrac = factor(target$pathologicfrac)
      
      design.ps <- svydesign(ids=~1, weights=~target$ps, data=target)
      model = svyglm(pathologicfrac ~ drug_sum, design = design.ps, family = binomial(link = 'logit'))
      
      
      #exp(model$coefficients)
      p <- predict(model, newdata=target, type="response")
      p = as.data.frame(p)$response
      pr <- prediction(p, target$pathologicfrac)
      prf <- performance(pr, measure = "tpr", x.measure = "fpr")
      plot(prf, main = "ROC Curve")
      output_auc[4] = performance(pr, "auc")@y.values[[1]][1]
      
      predictions = pr
      plot(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values),
           type="l", lwd=1, ylab="Sensitivity", xlab="Cutoff")
      par(new=TRUE)
      plot(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values),
           type="l", lwd=1, col='red', ylab="", xlab="", main = "Cutoff Analysis")
      #axis(4, at=seq(0,1,0.2),labels=z)
      mtext("Specificity",side=4, padj=-2, col='red')
      
      sens = cbind(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values))
      spec = cbind(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values))
      
      enu_out = list()
      enu = seq(0.1,0.9,by = 0.1)
      for (enu_iter in 1:length(enu)){
        cf = confusionMatrix(as.factor(ifelse(p > enu[enu_iter], 1,0)),target$pathologicfrac)
        tmp2 = t(data.frame(c(enu[enu_iter],cf[[4]])))
        colnames(tmp2)[1] = 'Cutoff'
        enu_out[[enu_iter]] = as.data.frame(tmp2)
      }
      write.csv(rbindlist(enu_out), paste(output_name,'_cutoff_diagnosis_pathologicfrac_weighted_ml.csv',sep=""))
      
      morp = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
      
      #1/(1+exp(-(model$coefficients[2]*300+model$coefficients[1])))
      p = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]
      out = (log(p/(1-p)) - coef(model)[1])/coef(model)[2]
      
      
      p_test <- predict(model, newdata=target_test, type="response")
      p_test = as.data.frame(p_test)$response
      cf_test = confusionMatrix(as.factor(ifelse(p_test > p, 1,0)),as.factor(target_test$pathologicfrac))
      test_output[[4]] = t(data.frame(c('pathologicfrac',cf_test[[4]])))
      
      X1_range = seq(from=min(target$drug_sum), to=max(target$drug_sum)*1.05, by=0.1)
      logits = coef(model)[1]+coef(model)[2]*X1_range
      probs = exp(logits)/(1 + exp(logits))
      plot(exp(X1_range), probs, type = 'l',ylim = c(0,1), main = 'Probablity Plot', xlab = 'Prednisone (mg)', ylab = 'Probablity')
      abline(h = p, col = 'red', lty = 'dashed')
      
      output_p[4] = p
      output[4] = exp(out)
      output_all[4] = nrow(target)
    }
    
    output_return = rbind(output_auc,output_p, output, output_n, output_all)
    colnames(output_return) = c('mskAE','Osteonecrosis','Osteoporosis','PathologicFacture')
    rownames(output_return) = c('AUC','Cutoff Probablity','Cutoff Value','Number of Items','Total Items')
    dev.off()
    par(mfrow = c(1,1))
    for(items in 1:4){
      test_output[[items]] = as.data.frame(test_output[[items]])
    }
    test_output = rbindlist(test_output)
    write.csv(test_output, paste(output_name,'_weighted_ml_test.csv',sep=""))
    write.csv(output_return, paste(output_name,'_weighted_ml.csv',sep=""))
    return(output_return)
  }
  
  
  steroid_analysis_weight(result, ps, result_test,output_name = 'output_all')
  # steroid_analysis_weight(result, ps, min_dose = 0, end_dose = 7.5, output_name = 'output_low_dose')
  # steroid_analysis_weight(result, ps, min_dose = 7.5, end_dose = 30, output_name = 'output_medium_dose')
  # steroid_analysis_weight(result, ps, min_dose = 30, end_dose = 100, output_name = 'output_high_dose')
  # steroid_analysis_weight(result, ps, min_dose = 100, end_dose = Inf, output_name = 'output_very_high_dose')
  # steroid_analysis_weight(result, ps, pulse = T, output_name = 'output_pulse')
  
}

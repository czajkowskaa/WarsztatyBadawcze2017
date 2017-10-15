library("randomForest")
## FUNKCJE ##
feature_selection_step<-function(glm_model1,train_df)
{
  step_model<-step(glm_model1,trace = FALSE)
  step_model_important_variables<-names(step_model$coefficients)
  step_model_important_variables<-step_model_important_variables[-1]
  step_model_important_variables<- unique(substr(step_model_important_variables,1,2))
  train_df_step_selected<-train_df[,c(step_model_important_variables,"y")]
  list(train_df_step_selected,step_model)
}

rf_step_prediction<-function(train_df_step_selected)
{
  rF_step<-randomForest(y ~.,train_df_step_selected,ntree=1000,norm.votes=FALSE)
  pred_rF_step<-predict(rF_step,test_df[,names(train_df_step_selected)[-length(names(train_df_step_selected))]],type="prob")
  ordered_pred_rF_step<-pred_rF_step[rev(order(pred_rF_step[,2])),]
  pred_rF_step
}

###WYKONANIE ###
train_df<-read.csv("zbior_uczacy.txt",sep=';')
train_df<-na.omit(train_df)
test_df<-read.csv("zbior_testowy.txt",sep=';')
glm_model<-glm(y~.,train_df,family="binomial")

feature_selection_result<-feature_selection_step(glm_model,train_df)
train_df_step_selected<-feature_selection_result[[1]]
rf_step_prediction_result<-rf_step_prediction(train_df_step_selected)


result_df<-cbind(score=pred_rF_step[,2],test_df)
write.table(result_df,"agata_czajkowska.txt",sep=";",row.names = FALSE)

result_file<-read.csv("agata_czajkowska.txt",sep=';')

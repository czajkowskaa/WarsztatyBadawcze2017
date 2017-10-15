## warsztaty badawcze 
##pakiety
library("dplyr")
library("ggplot2")
library("ggpubr")
library("randomForest")
library("xgboost")
##projekt 1

## wczytanie danych
setwd("E:\\MINI\\SEMESTR 9\\WARSZTATY BADAWCZE\\PROJEKTY\\PROJEKT 1")
trainOriginal_df<-read.csv("zbior_uczacy.txt",sep=';')
str(trainOriginal_df)
set.seed(123456)
##losowy podzial na zbior testowy i treningowy
n<-nrow(trainOriginal_df)
random_order<-sample(seq(1,n))
test_df<-trainOriginal_df[random_order[1:(n/5)],]
train_df<-trainOriginal_df[random_order[((n/5)+1):n],]
### I. wyznaczenie zalenosci miedzy zmiennymi

## chisq_test-zalozenie y z jest niezalezne od z na poziomie 0.05 , jesli p-value >0.05 to nie ma podstaw do odrzucenia chipotezy
##wybranie zmiennych typu factor

get_independence<-function(train_df)
{
train_df_factors<-select_if(train_df[,-1],is.factor)
train_df_numerics<-select_if(train_df,is.numeric)
chisq_p_values_factors<-rep(0,ncol(train_df_factors))
aov_p_values_numerics<-rep(0,ncol(train_df_numerics))

##zmienne typu factor
for(i in 1:length(chisq_p_values_factors))
{
  chisq_p_values_factors[i]<-chisq.test(train_df$y,train_df_factors[,i])$p.value
}

independence_df_factors<-data.frame(name=names(train_df_factors),value=chisq_p_values_factors)
ordered_independence_df_factors<-independence_df_factors[order(independence_df_factors$value),]
ordered_independence_df_factors$independent<-ordered_independence_df_factors$value>0.05
##zmienne typu numeric
## analiza anova -zalozenie zmienne sa skorelowane 
for(i in 1:length(t_test_p_values_numerics))
{
  aov<-aov(train_df_numerics[,i]~train_df$y)
  aov_p_values_numerics[i]<-summary(aov)[[1]][[1,"Pr(>F)"]]
}

independence_df_numerics<-data.frame(name=names(train_df_numerics),value=aov_p_values_numerics)
ordered_independence_df_numerics<-independence_df_numerics[order(independence_df_numerics$value),]
ordered_independence_df_numerics$independent<-ordered_independence_df_numerics$value<0.05

 list(factors=ordered_independence_df_factors,numerics=ordered_independence_df_numerics)
}

## wizualizacja ktore sa zalezne 
independent_variables<-get_independence(train_df)
vizualise_indepnedent_variables<-function(independent_variables)
{
  
factors<-ggplot(independent_variables$factors,aes(x=name,y=value,color=value>0.05))+
  geom_point(size=2)+
  geom_hline(yintercept=0.05,linetype="dashed")+
  geom_text(aes(label=name),vjust=1,hjust=1)+
  xlab("Zmienne")+
  ylab("P_wartość")+
  guides(color=guide_legend(title="Niezależne"))+
  guides(label=guide_legend(title="Niezależne"))
  


numerics<-ggplot(independent_variables$numerics,aes(x=name,y=value,color=value<0.05))+
  geom_point(size=2)+
  geom_text(aes(label=name),vjust=1,hjust=1)+
  geom_hline(yintercept=0.05,linetype="dashed")+
  xlab("Zmienne")+
  ylab("P_wartość")+
  guides(color=FALSE)



ggarrange(numerics,factors,labels=c("Numerics","Factors"),ncol=2,nrow=1)
}

vizualise_indepnedent_variables(independent_variables)

vizualise_indepnedent_variables2<-function(independent_variables)
{
factors<-ggplot(independent_variables$factors,aes(x=name,y=0.01))+
  scale_y_discrete(limits=0:0.1)+
  geom_tile(aes(fill=independent))+
  coord_fixed(ratio=0.7)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  guides(fill=FALSE)
  
  
numerics<-ggplot(independent_variables$numerics,aes(x=name,y=0.01))+
  scale_y_discrete(limits=0:0.1)+
  geom_tile(aes(fill=independent))+
  coord_fixed(ratio=1.2)+
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  guides(fill=FALSE)

ggarrange(numerics,factors,labels=c("Numerics","Factors"),ncol=1,nrow=2)
}

vizualise_indepnedent_variables2(independent_variables)

raw<-vizualise_indepnedent_variables(independent_variables)
ind_sum<-vizualise_indepnedent_variables2(independent_variables)
ggarrange(raw,ind_sum,ncol=1,nrow=2)

### II.SELEKCJA ZMIENNYCH
glm_model1<-glm(y~.,train_df_no_na,family="binomial")

train_df_no_na<-na.omit(train_df)

feature_selection_glm<-function(train_df_no_na,glm_model1)
{
glm_important_variables<-summary(glm_model1)$coef[,4]<0.05
glm_important_variables<-glm_important_variables[glm_important_variables]
glm_important_variables<-glm_important_variables[-1]
glm_important_variables_names<- unique(substr(names(glm_important_variables),1,2))
train_df_glm_selected<-train_df_no_na[,c(glm_important_variables_names,"y")]
train_df_glm_selected
}

feature_selection_step<-function(glm_model1)
{
  step_model<-step(glm_model1)
  step_model_important_variables<-names(step_model$coefficients)
  step_model_important_variables<-step_model_important_variables[-1]
  step_model_important_variables<- unique(substr(step_model_important_variables,1,2))
  train_df_step_selected<-train_df_no_na[,c(step_model_important_variables,"y")]
  list(step_model,train_df_step_selected)
}


## do step selection 2 [[2]]
vizualize_feature_selection(glm_selection,step_selection,train_df)
{
  names_glm<-names(glm_selection)
  names_step<-names(step_selection)
  names_df<-names(train_df[-1])
  feature_selt_vis_step_df<-data.frame(names=names_df,method=rep("step",length(names_df)),value=names_df %in% names_step)
  feature_selt_vis_glm_df<-data.frame(names=names_df,method=rep("glm",length(names_df)),value=names_df %in% names_glm)
  feautre_select_vis_df<-rbind(feature_selt_vis_step_df,feature_selt_vis_glm_df)
  

  
  ggplot(feautre_select_vis_df,aes(x=names,y=method))+
  geom_tile(aes(fill=value))+
  coord_fixed(ratio=2.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.y = element_blank(),axis.title.x = element_blank())+
  scale_fill_manual(values=c("#a6bddb","#a1d99b"))+
  ggtitle("Zmienne wybrane do modeli")+
  guides(fill=guide_legend(title="W modelu"))
}

### III.KLASYFIKACJA

#a) random forest + glm

rF_glm<-randomForest(y ~.,train_df_glm_selected,ntree=1000,norm.votes=FALSE)
pred_rF_glm<-predict(rF_glm,test_df[,names(train_df_glm_selected)],type="prob")
ordered_pred_rF_glm<-pred_rF_glm[rev(order(pred_rF_glm[,2])),]
varImpPlot(rF_glm)
#b) random forest +step
rF_step<-randomForest(y ~.,train_df_step_selected,ntree=1000,norm.votes=FALSE)
pred_rF_step<-predict(rF_step,test_df[,names(train_df_step_selected)],type="prob")
ordered_pred_rF_step<-pred_rF_step[rev(order(pred_rF_step[,2])),]
varImpPlot(rF_step)

##c) xgboost + glm
xg_boost_glm_train_new<-model.matrix(~.+0,data=train_df_glm_selected[,-ncol(train_df_glm_selected)],with=F)
xg_boost_glm_train<-xgb.DMatrix(data=xg_boost_glm_train_new,label=train_df_glm_selected$y)
                                
xg_boost_glm_test_new<-model.matrix(~.+0,data=test_df[,names(train_df_step_selected)],with=F)
xg_boost_glm_test<-xgb.DMatrix(data=xg_boost_glm_test_new)

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

xgbcv_glm <- xgb.cv( params = params, data = xg_boost_glm_train, nrounds = 100, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)

#c)glm +glm

glm_glm<-glm(y~.,data=train_df_glm_selected,family='binomial')
glm_glm_pred<-predict(glm_glm,test_df[,names(train_df_glm_selected)],type="response")
order_glm_glm_pred<-glm_glm_pred[rev(order(glm_glm_pred))]

#d) glm + step
glm_step<-glm(y~.,data=train_df_step_selected,family='binomial')
glm_step_pred<-predict(glm_step,test_df[,names(train_df_step_selected)],type="response")
order_glm_step_pred<-glm_step_pred[rev(order(glm_step_pred))]

## IV. Jakosc klasyfikacji


calculate_10percent_accuracy<-function(test_df,results)
{
  test_df_positive_class<-test_df[test_df$y=="klasa +",]
  predictions<-head(results,(nrow(test_df)/10))
  accuracy_vect<-names(predictions) %in% row.names(test_df_positive_class)
  true_pred<-accuracy_vect[accuracy_vect]
  length(true_pred)/length(accuracy_vect)
}

## b) random forest + glm
level_rf_glm<-calculate_10percent_accuracy(test_df,ordered_pred_rF_glm[,2])
## a) random forest + step
level_rf_step<-calculate_10percent_accuracy(test_df,ordered_pred_rF_step[,2])
## c) glm +glm
level_glm_glm<-calculate_10percent_accuracy(test_df,order_glm_glm_pred)
## d)
level_glm_step<-calculate_10percent_accuracy(test_df,order_glm_step_pred)

######

imp_indep<-independent_variables$factors[independent_variables$factors$independent==FALSE,]
imp_indep_number<-independent_variables$numerics[independent_variables$numerics$independent==FALSE,]
imp_indep_names<-c(as.character(imp_indep$name),as.character(imp_indep_number$name))

train_df_imp_selected<-train_df[,c(imp_indep_names,"y")]

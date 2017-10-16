##pakiety
library("dplyr")
library("ggplot2")
library("randomForest")
library("knitr")
trainOriginal_df<-read.csv("zbior_uczacy.txt",sep=';')
set.seed(123456)
##losowy podzial na zbior testowy i treningowy
n<-nrow(trainOriginal_df)
random_order<-sample(seq(1,n))
test_df<-trainOriginal_df[random_order[1:(n/5)],]
train_df<-trainOriginal_df[random_order[((n/5)+1):n],]

### I. wyznaczenie zalenosci miedzy zmiennymi

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
  for(i in 1:length(aov_p_values_numerics))
  {
    aov<-aov(train_df_numerics[,i]~train_df$y)
    aov_p_values_numerics[i]<-summary(aov)[[1]][[1,"Pr(>F)"]]
  }
  
  independence_df_numerics<-data.frame(name=names(train_df_numerics),value=aov_p_values_numerics)
  ordered_independence_df_numerics<-independence_df_numerics[order(independence_df_numerics$value),]
  ordered_independence_df_numerics$independent<-ordered_independence_df_numerics$value<0.05
  
  list(factors=ordered_independence_df_factors,numerics=ordered_independence_df_numerics)
}

vizualise_indepnedent_variables<-function(independent_variables)
{
  
  factors<-ggplot(independent_variables$factors,aes(x=name,y=value,color=value>0.05))+
    geom_point(size=2)+
    geom_hline(yintercept=0.05,linetype="dashed")+
    xlab("Zmienne")+
    ylab("P_wartosc")+
    guides(color=guide_legend(title="Niezalezne"))+
    guides(label=guide_legend(title="Niezalezne"))+
    scale_color_manual(values=c("#999999", "#E69F00"),labels=c("Nie","Tak"))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  
  numerics<-ggplot(independent_variables$numerics,aes(x=name,y=value,color=value<0.05))+
    geom_point(size=2)+
    geom_hline(yintercept=0.05,linetype="dashed")+
    xlab("Zmienne")+
    ylab("P_wartosc")+
    guides(color=FALSE)+
    scale_color_manual(values=c("#999999", "#E69F00"),labels=c("Nie","Tak"))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ggarrange(numerics,factors,labels=c("Zm. liczbowe","Zm. kategoryczne"),ncol=2,nrow=1)
}



vizualise_indepnedent_variables2<-function(independent_variables)
{
  factors<-ggplot(independent_variables$factors,aes(x=name,y=0.01))+
    scale_y_discrete(limits=0:0.1)+
    geom_tile(aes(fill=independent))+
    coord_fixed(ratio=0.7)+
    scale_fill_manual(values=c("#999999", "#E69F00"))+
    theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.title.y = element_blank(),
          axis.title.x = element_blank())+
    guides(fill=FALSE)
  
  
  numerics<-ggplot(independent_variables$numerics,aes(x=name,y=0.01))+
    scale_y_discrete(limits=0:0.1)+
    geom_tile(aes(fill=independent))+
    coord_fixed(ratio=1.2)+
    scale_fill_manual(values=c("#999999", "#E69F00"))+
    theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),axis.title.y = element_blank(),
          axis.title.x = element_blank())+
    guides(fill=FALSE)
  
  #ggarrange(numerics,factors,labels=c("Zm. liczbowe","Zm. kategoryczne"),ncol=1,nrow=2)
}

### II.SELEKCJA ZMIENNYCH

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
  step_model<-step(glm_model1,trace = FALSE)
  step_model_important_variables<-names(step_model$coefficients)
  step_model_important_variables<-step_model_important_variables[-1]
  step_model_important_variables<- unique(substr(step_model_important_variables,1,2))
  train_df_step_selected<-train_df_no_na[,c(step_model_important_variables,"y")]
  list(train_df_step_selected,step_model)
}

vizualize_feature_selection<-function(glm_selection,step_selection,train_df)
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
    scale_fill_manual(values=c("#a6bddb","#a1d99b"),labels=c("Nie","Tak"))+
    ggtitle("Zmienne wybrane do modeli")+
    guides(fill=guide_legend(title="W modelu"))
}
### III.Klasyfikacja

#a) random forest + glm

rf_glm_prediction<-function(train_df_glm_selected)
{
  rF_glm<-randomForest(y ~.,train_df_glm_selected,ntree=1000,norm.votes=FALSE)
  pred_rF_glm<-predict(rF_glm,test_df[,names(train_df_glm_selected)],type="prob")
  ordered_pred_rF_glm<-pred_rF_glm[rev(order(pred_rF_glm[,2])),]
  list(rF_glm,ordered_pred_rF_glm)
}
  
#b) random forest +step
rf_step_prediction<-function(train_df_step_selected)
{
rF_step<-randomForest(y ~.,train_df_step_selected,ntree=1000,norm.votes=FALSE)
pred_rF_step<-predict(rF_step,test_df[,names(train_df_step_selected)],type="prob")
ordered_pred_rF_step<-pred_rF_step[rev(order(pred_rF_step[,2])),]
list(rF_step,ordered_pred_rF_step)
}


#c)glm +glm
glm_glm_prediction<-function(train_df_glm_selected)
{
glm_glm<-glm(y~.,data=train_df_glm_selected,family='binomial')
glm_glm_pred<-predict(glm_glm,test_df[,names(train_df_glm_selected)],type="response")
order_glm_glm_pred<-glm_glm_pred[rev(order(glm_glm_pred))]
order_glm_glm_pred
}
#d) glm + step
glm_step_prediction<-function(train_df_step_selected)
{
glm_step<-glm(y~.,data=train_df_step_selected,family='binomial')
glm_step_pred<-predict(glm_step,test_df[,names(train_df_step_selected)],type="response")
order_glm_step_pred<-glm_step_pred[rev(order(glm_step_pred))]
}


## IV. Jakosc klasyfikacji


calculate_20percent_accuracy<-function(test_df,results)
{
  test_df_positive_class<-test_df[test_df$y=="klasa +",]
  predictions<-head(results,(nrow(test_df)/5))
  accuracy_vect<-names(predictions) %in% row.names(test_df_positive_class)
  true_pred<-accuracy_vect[accuracy_vect]
  length(true_pred)/length(accuracy_vect)
}

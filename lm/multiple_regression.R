library(stringr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(stringr)
library(jtools)
library(ggstance)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ppcor)
library(tibble)

rm(list = ls()) # clean the variables environment

#### PARAMETERS ######
#### parameters to set before we run the code ####:
# which file to analyze:
file = read.csv('international_first_run all_condotions.csv')
# what conditions will be the predictors (names should be as they appear in the file)
predictors = c("VGG", "SGPT")
# What are the predictors' names (these names will be used in the figures and models)
# (you can choose names that are different from the original names in the file, but keep the same order as you entered the conditions above)
predictors_names = c("VGG16", "SGPT")
# what conditions will be predicted (names should be as they appear in the file)
predicted = c("semantic_pics","semantic_names")
# What are the predicted variables' names 
# (you can choose names that are different from the original names in the file, but keep the same order as you entered the conditions above)
predicted_names = c("semantic_images","semantic_names")
# do you want to filter according to vividness?
# if no, set to 'null', if you want to keep high set to 'H' if low set to 'L'
vividness = 'null'
# important!! - the conditions that vividness is relevant to them - 
# use the suffix '_viv' in the 'predicted_names', and make sure not to use this
# suffix in conditions that do not care for vividness.

# please read (all of) this:
# you can also choose the colors of the bars of the plot if you want, 
# by changing the values of colors in the pallet
# if you don't want to choose the colors please set the pallet to 'null' (remove the comment from that line)
# if you keep the pallet, 
# the code will choose the first n colors from it (according to the number of predictors)
# in the plot - R will arrange the predictors according to the ABC, so take this into account when you name the
# predictors (in the 'predictors names' vector and in the colors pallet)
# because they wont necessarily be aligned
# the usual colors we choose for predictors are: 
# for semantic predictor- red ('brown3' in the pallet)
# for visual predictor- blue ('dodgerblue3' in the pallet)
# for visual-semantic predictor- purple ('mediumorchid3' in the pallet)

# color_pallet = c('mediumorchid3','brown3','dodgerblue3')
color_pallet = c('dodgerblue3','brown3')

#################################################

# clean and prepare the data:
file$condition = sub(".*/./", "", file$condition) 
# we only want to analyze trials that should not be rejected
file = subset(file, reject_trial == FALSE)
# we analyze the averaged data across participants in each condition:
relevant_data <- file %>% group_by(pair, condition) %>% summarise(keyPressed = mean(keyPressed))


# we will create a df that we can use to conduct the ml:
pairs = data.frame(unique(relevant_data$pair))
colnames(pairs) = c('pair')
for (i in (1:length(predictors))){
  predictor = predictors[i]
  predictor_name = predictors_names[i]
  predictor_df = subset(relevant_data, condition == predictor)
  # we create a df that includes only the pairs that are included for all variables
  predictor_df <- subset(predictor_df, pair %in% pairs$pair)
  pairs <- subset(pairs, pair %in% predictor_df$pair)
  if (all(pairs$pair == predictor_df$pair)){ # very important: we make sure all pairs are in the same order
    pairs[predictor_name] = predictor_df$keyPressed
  }
}

all_results_corr = data.frame()
all_results_ml = data.frame()
for (i in (1:length(predicted))){
  predicted_var = predicted[i]
  predicted_name = predicted_names[i]
  predicted_df = subset(file, condition == predicted_var)

  if (str_detect(predicted_name, '_viv')){
    # then we need to subset the sf according to the vividness
    if (vividness != 'null'){
      if (vividness == 'H'){
        predicted_df = subset(predicted_df, vividness_img1 < 3)
        predicted_df = subset(predicted_df, vividness_img2 < 3)
      }
      else{ # then vividness = 'L'
        predicted_df = subset(predicted_df, vividness_img1 >= 3)
        predicted_df = subset(predicted_df, vividness_img2 >= 3)
      }
    }
  }
  predicted_df <- predicted_df %>% group_by(pair, condition) %>% summarise(keyPressed = mean(keyPressed))
  
  # we create a df that includes only the pairs that are included for all variables
  predicted_df <- subset(predicted_df, pair %in% pairs$pair)
  pairs_unique <- subset(pairs, pair %in% predicted_df$pair)
  if (all(pairs_unique$pair == predicted_df$pair)){ # very important: we make sure all pairs are in the same order
    pairs_unique[predicted_name] = predicted_df$keyPressed
  }
  
  correlations = cor(pairs_unique[, c(predictors_names, predicted_name)])
  
  # specific correlations with this predicted variable:
  sub_correl = subset(correlations, select = predicted_name)
  sub_correl= data.frame(sub_correl)
  sub_correl <- tibble::rownames_to_column(sub_correl, "predictor")
  sub_correl = subset(sub_correl, predictor %in% predictors_names)
  
  #print lm results:
  print(noquote(" "))
  print(noquote(paste("####### predicting:", predicted_name, "########")))
  print(noquote(" "))
  general_formula = paste(predicted_name, '~')
  # we need to add each predictor to the formula
  for (j in (1:(length(predictors)))){
    predictor = predictors_names[j]
    if (j > 1){
      general_formula = paste(general_formula,predictor,sep = '+')  
    }else{
      general_formula = paste(general_formula,predictor)
    }
  }
  fmla <- as.formula(general_formula)
  model = lm(fmla, data = pairs_unique)
  sum = summ(model, scale = TRUE)
  print(noquote(" "))
  print(noquote("##### normalized results multiple model ######"))
  print(noquote(" "))
  print(sum)
  
  #plot the normalized coefficients:
  sum_df = data.frame(sum$coeftable)
  sum_df = sum_df[-1,] # removing the intercept line
  normelized_betas = data.frame(row.names(sum_df), sum_df$Est.)
  colnames(normelized_betas) = c('predictor', 'normelized_beta')

  normelized_betas$predicted = predicted_name
  
  # plot the correlations
  colnames(sub_correl) = c('predictor', 'correlation')
  
  sub_correl$predicted = predicted_name
  
  all_results_corr = rbind(all_results_corr, sub_correl)
  all_results_ml = rbind(all_results_ml, normelized_betas)
}

all_results_corr$predictor <- factor(all_results_corr$predictor,                                    # Change ordering manually
                  levels = predictors_names)

all_results_corr$predicted <- factor(all_results_corr$predicted,                                    # Change ordering manually
                                     levels = predicted_names)

p<-ggplot(all_results_corr, aes(x=predicted, y=correlation, fill=predictor)) +
  geom_bar(stat="identity",width = 0.8, position=position_dodge())+
  scale_fill_manual(values = head(color_pallet,3))+
  theme_classic()+
  geom_text(aes(label = round(correlation, digits = 2)), size = 5,vjust = 1.5, position = position_dodge(.9))+
  theme(axis.title = element_text(size = 20),
        axis.text.y = element_text(size = 13), axis.text.x = element_text(size = 13))+
  ylim(-0.1,1)
print(p)


all_results_ml$predictor <- factor(all_results_ml$predictor,                                    # Change ordering manually
                                     levels = predictors_names)

all_results_ml$predicted <- factor(all_results_ml$predicted,                                    # Change ordering manually
                                     levels = predicted_names)

p<-ggplot(all_results_ml, aes(x=predicted, y=normelized_beta, fill=predictor)) +
  geom_bar(stat="identity",width = 0.8, position=position_dodge())+
  scale_fill_manual(values = head(color_pallet,3))+
  theme_classic()+
  geom_text(aes(label = round(normelized_beta, digits = 2)), size = 5,vjust = 1.5, position = position_dodge(.9))+
  theme(axis.title = element_text(size = 20),
        axis.text.y = element_text(size = 13), axis.text.x = element_text(size = 13))+
  ylim(-0.1,1.1)
print(p)


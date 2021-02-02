#########################################################################################
# Prepared for David Utassy's DA3 assignment 1.
#
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
rm(list=ls())


library(rattle)
library(tidyverse)
library(caret)
library(ranger)
library(Hmisc)
library(knitr)
library(kableExtra)
library(xtable)


data_in <- "/Users/utassydv/Documents/workspaces/CEU/my_repos/data_analysis_prediction/airbnb_tokyo/data/clean/airbnb_tokio_cleaned.csv"
data_out <- ""
output <- "/Users/utassydv/Documents/workspaces/CEU/my_repos/data_analysis_prediction/airbnb_tokyo/out/"
#-----------------------------------------------------------------------------------------

#########################################################################################
#
# PART I
# Loading and preparing data ----------------------------------------------
#
#########################################################################################



data <- read_csv(data_in) %>%
  mutate_if(is.character, factor) %>%
  filter(!is.na(usd_price_day))


count_missing_values <- function(data) {
  num_missing_values <- map_int(data, function(x) sum(is.na(x)))
  num_missing_values[num_missing_values > 0]
}

count_missing_values(data)

# Sample definition and preparation ---------------------------------------

# We focus on normal apartments, 2<n<6

table(data$f_property_type)
data <- data %>% filter(f_property_type == "Apartment")
#data <- data %>% filter(f_room_type == "Entire home/apt")

data <- data %>% filter(n_accommodates <= 6 & n_accommodates >= 2)

# copy a variable - purpose later, see at variable importance
data <- data %>% mutate(n_accommodates_copy = n_accommodates)

# basic descr stat -------------------------------------------
#skimr::skim(data)
summary(data$usd_price_day)
Hmisc::describe(data$usd_price_day)
describe(data$f_room_type)
describe(data$f_property_type)
table(data$f_number_of_reviews)

# create train and holdout samples -------------------------------------------
# train is where we do it all, incl CV

set.seed(2801)

# First pick a smaller than usual training set so that models run faster and check if works
# If works, start anew without these two lines

#try <- createDataPartition(data$usd_price_day, p = 0.2, list = FALSE)
#data <- data[try, ]



train_indices <- as.integer(createDataPartition(data$price, p = 0.7, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)

# Define models: simpler, extended -----------------------------------------------------------

# Basic Variables inc neighnourhood
basic_vars <- c(
  "n_accommodates", "n_beds", "n_days_since", "f_neighbourhood_cleansed", "f_room_type")

# reviews
reviews <- c("n_number_of_reviews", "flag_n_number_of_reviews" ,"n_review_scores_rating", "flag_review_scores_rating")

# Dummy variables
amenities <-  grep("^d_.*", names(data), value = TRUE)

#interactions for the LASSO
#TODO: do something about it

# with boroughs
X1  <- c("f_room_type*f_neighbourhood_cleansed", "f_room_type*f_neighbourhood_cleansed",
         "n_accommodates*f_neighbourhood_cleansed", "d_elevator*f_neighbourhood_cleansed")


predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, reviews, amenities)
predictors_E <- c(basic_vars, reviews, amenities, X1)


#########################################################################################
#
# PART II
# RANDOM FORESTS -------------------------------------------------------
#
#########################################################################################



# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


# set tuning
tune_grid <- expand.grid(
  .mtry = c(3,4,5),               #controlling complexity of individual trees, number of features used at splitting sqrt(features)
  .splitrule = "variance",
  .min.node.size = c(5, 10)       #min num observations in the terminating nodes
)


# simpler model for model A (1)
set.seed(1234)
system.time({
  rf_model_1 <- train(
    formula(paste0("usd_price_day ~", paste0(predictors_1, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})
rf_model_1
write.csv(rf_model_1$results, paste0(output, "rf_model_1.csv"), row.names = F)

# set tuning for benchamrk model (2)
tune_grid <- expand.grid(
  .mtry = c(5, 7, 9),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

set.seed(1234)
system.time({
  rf_model_2 <- train(
    formula(paste0("usd_price_day ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model_2
write.csv(rf_model_2$results, paste0(output, "rf_model_2.csv"), row.names = F)

#auto tuning first
#set.seed(1234)
#system.time({
#  rf_model_2auto <- train(
#    formula(paste0("usd_price_day ~", paste0(predictors_2, collapse = " + "))),
#    data = data_train,
#    method = "ranger",
#    trControl = train_control,
#    importance = "impurity"
#  )
#})
#rf_model_2auto 
#rf_model_2auto <-rf_model_2
#write.csv(rf_model_2auto$results, paste0(output, "rf_model_2auto.csv"), row.names = F)




# evaluate random forests -------------------------------------------------

results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2
    #model_2b = rf_model_2auto
    
  )
)
summary(results)

# Save outputs -------------------------------------------------------

# Show Model B rmse shown with all the combinations
rf_tuning_modelB <- rf_model_2$results %>%
  dplyr::select(mtry, min.node.size, RMSE) %>%
  dplyr::rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)

kable(x = rf_tuning_modelB, format = "latex", digits = 2, caption = "CV RMSE") %>%
  add_header_above(c(" ", "vars" = 3)) %>%
  cat(.,file="/Users/utassydv/Documents/workspaces/CEU/my_repos/data_analysis_prediction/airbnb_tokyo/out/rf_tuning_modelB.tex")
write.csv(rf_tuning_modelB, paste0(output, "rf_tuning_modelB.csv"), row.names = F)

# Turning parameter choice 1
result_1 <- matrix(c(
  rf_model_1$finalModel$mtry,
  rf_model_2$finalModel$mtry,
  rf_model_1$finalModel$min.node.size,
  rf_model_2$finalModel$min.node.size
  
),
nrow=2, ncol=2,
dimnames = list(c("Model A", "Model B"),
                c("Min vars","Min nodes"))
)
kable(x = result_1, format = "latex", digits = 3) %>%
  cat(.,file="/Users/utassydv/Documents/workspaces/CEU/my_repos/data_analysis_prediction/airbnb_tokyo/out/rf_models_turning_choices.tex")
write.csv(result_1, paste0(output, "rf_parameter_selection.csv"), row.names = T)

# Turning parameter choice 2
result_2 <- matrix(c(mean(results$values$`model_1~RMSE`),
                     mean(results$values$`model_2~RMSE`)
),
nrow=2, ncol=1,
dimnames = list(c("Model A", "Model B"),
                c(results$metrics[2]))
)


kable(x = result_2, format = "latex", digits = 3) %>%
  cat(.,file="/Users/utassydv/Documents/workspaces/CEU/my_repos/data_analysis_prediction/airbnb_tokyo/out/rf_models_rmse.tex")
write.csv(result_2, paste0(output, "rf_models_compare.csv"), row.names = T)

#########################################################################################
#
# PART III
# MODEL DIAGNOSTICS -------------------------------------------------------
#
#########################################################################################


#########################################################################################
# Variable Importance Plots -------------------------------------------------------
#########################################################################################
# first need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}


# variable importance plot
# 1) full varimp plot, full
# 2) varimp plot grouped
# 3) varimp plot , top 10
# 4) varimp plot  w copy, top 10


rf_model_2_var_imp <- importance(rf_model_2$finalModel)/1000
rf_model_2_var_imp_df <-
  data.frame(varname = names(rf_model_2_var_imp),imp = rf_model_2_var_imp) %>%
  mutate(varname = gsub("f_neighbourhood_cleansed", "Borough:", varname) ) %>%
  mutate(varname = gsub("f_room_type", "Room type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))
write.csv(rf_model_2_var_imp_df, paste0(output, "rf_model_2_var_imp_df.csv"), row.names = F)


##############################
# 1) full varimp plot, above a cutoff
##############################

# to have a quick look
plot(varImp(rf_model_2))
#TODO continue
source("/Users/utassydv/Documents/workspaces/CEU/my_repos/data_analysis_prediction/airbnb_tokyo/theme_bg.R")
cutoff = 600
rf_model_2_var_imp_plot <- ggplot(rf_model_2_var_imp_df[rf_model_2_var_imp_df$imp>cutoff,],
                                  aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1.5) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=1) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bg() +
  theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
        axis.title.x = element_text(size=6), axis.title.y = element_text(size=6))
rf_model_2_var_imp_plot
#save_fig("rf_varimp1",output, "large")
#save_fig("ch16-figure-1-rf-varimp-base",output, "large")

##############################
# 2) full varimp plot, top 10 only
##############################


# have a version with top 10 vars only
rf_model_2_var_imp_plot_b <- ggplot(rf_model_2_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bg() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_2_var_imp_plot_b
#save_fig("rf_varimp1_b",output, "small")
#save_fig("ch16-figure-2b-rf-varimp-top10",output, "small")


##############################
# 2) varimp plot grouped
##############################
# grouped variable importance - keep binaries created off factors together

varnames <- rf_model_2$finalModel$xNames
f_neighbourhood_cleansed_varnames <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)
f_cancellation_policy_varnames <- grep("f_cancellation_policy",varnames, value = TRUE)
f_bed_type_varnames <- grep("f_bed_type",varnames, value = TRUE)
f_property_type_varnames <- grep("f_property_type",varnames, value = TRUE)
f_room_type_varnames <- grep("f_room_type",varnames, value = TRUE)

groups <- list(f_neighbourhood_cleansed=f_neighbourhood_cleansed_varnames,
               f_room_type = f_room_type_varnames,
               n_days_since = "n_days_since",
               n_accommodates = "n_accommodates",
               n_beds = "n_beds",
               n_number_of_reviews = "n_number_of_reviews",
               n_review_scores_rating = "n_review_scores_rating",
               d_elevator = "d_elevator",
               d_cookingbasics = "d_cookingbasics",
               d_tv = "d_tv",
               d_keypad = "d_keypad",
               d_dryer = "d_dryer",
               d_lockbox = "d_lockbox"
               
)

rf_model_2_var_imp_grouped <- group.importance(rf_model_2$finalModel, groups)
rf_model_2_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_2_var_imp_grouped),
                                            imp = rf_model_2_var_imp_grouped[,1])  %>%
  mutate(imp_percentage = imp/sum(imp))
write.csv(rf_model_2_var_imp_grouped, paste0(output, "rf_model_2_var_imp_grouped_df.csv"), row.names = T)

rf_model_2_var_imp_grouped_plot <-
  ggplot(rf_model_2_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bg() +
  theme(axis.text.x = element_text(size=10), axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=10), axis.title.y = element_text(size=10))
rf_model_2_var_imp_grouped_plot
save_fig("rf_varimp_grouped1",output, "large")
#save_fig("ch16-figure-2a-rf-varimp-group",output, "small")



#########################################################################################
# Partial Dependence Plots -------------------------------------------------------
#########################################################################################

# TODO
# : somehow adding scale screws up. ideadlly both graphs y beween 70 and 130,
# n:accom should be 1,7 by=1

# FIXME
# should be on holdout, right? pred.grid = distinct_(data_train, "), --> pred.grid = distinct_(data_holdout, )

pdp_n_acc <- pdp::partial(rf_model_2, pred.var = "n_accommodates", pred.grid = distinct_(data_holdout, "n_accommodates"), train = data_train)
pdp_n_acc_plot <- pdp_n_acc %>%
  autoplot( ) +
  geom_point(color=color[1], size=2) +
  geom_line(color=color[1], size=1) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1))+
  theme_bg()
pdp_n_acc_plot
save_fig("rf_pdp_n_accom", output, "large")
#save_fig("ch16-figure-3a-rf-pdp-n-accom", output, "small")

describe(data_holdout_w_prediction$n_accommodates)


pdp_n_roomtype <- pdp::partial(rf_model_2, pred.var = "f_room_type", pred.grid = distinct_(data_holdout, "f_room_type"), train = data_train)
pdp_n_roomtype_plot <- pdp_n_roomtype %>%
  autoplot( ) +
  geom_point(color=color[1], size=2) +
  ylab("Predicted price") +
  xlab("Room type") +
  scale_y_continuous(limits=c(60,120), breaks=seq(60,120, by=10)) +
  theme_bg()
pdp_n_roomtype_plot
#save_fig("rf_pdp_roomtype", output, "small")
#save_fig("ch16-figure-3b-rf-pdp-roomtype", output, "small")

pdp_n_borough <- pdp::partial(rf_model_2, pred.var = "f_neighbourhood_cleansed", pred.grid = distinct_(data_holdout, "f_neighbourhood_cleansed"), train = data_train)
pdp_n_borough_plot <- pdp_n_borough %>%
  autoplot( ) +
  geom_point(color=color[1], size=2) +
  ylab("Predicted price") +
  xlab("Borough") +
  scale_y_continuous(limits=c(60,120), breaks=seq(60,120, by=10)) +
  #theme_bg() +
  theme(axis.text.x = element_text(angle = 90))
pdp_n_borough_plot
save_fig("rf_pdp_n_borough_plot", output, "large")
# Subsample performance: RMSE / mean(y) ---------------------------------------
# NOTE  we do this on the holdout set.

# ---- cheaper or more expensive flats - not used in book
data_holdout_w_prediction <- data_holdout %>%
  mutate(predicted_price = predict(rf_model_2, newdata = data_holdout))



######### create nice summary table of heterogeneity
a <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 3, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, usd_price_day),
    mean_price = mean(usd_price_day),
    rmse_norm = RMSE(predicted_price, usd_price_day) / mean(usd_price_day)
  )
a
#TODO real boroughs
b <- data_holdout_w_prediction %>%
  filter(f_neighbourhood_cleansed %in% c("Fuchu Shi", "Kokubunji Shi", "Hino Shi")) %>%
  group_by(f_neighbourhood_cleansed) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, usd_price_day),
    mean_price = mean(usd_price_day),
    rmse_norm = rmse / mean_price
  )
b
c <- data_holdout_w_prediction %>%
  group_by(f_room_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, usd_price_day),
    mean_price = mean(usd_price_day),
    rmse_norm = rmse / mean_price
  )


d <- data_holdout_w_prediction %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, usd_price_day),
    mean_price = mean(usd_price_day),
    rmse_norm = RMSE(predicted_price, usd_price_day) / mean(usd_price_day)
  )

# Save output
colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")

line1 <- c("Apartment size", "", "", "")
line2 <- c("Borough", "", "", "")
line3 <- c("Room type", "", "", "")

result_3 <- rbind(line1, a, line3, c, line2, b, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
            `RMSE/price` = as.numeric(`RMSE/price`))

options(knitr.kable.NA = '')
kable(x = result_3, format = "latex", booktabs=TRUE, linesep = "",digits = c(0,2,1,2), col.names = c("","RMSE","Mean price","RMSE/price")) %>%
  cat(.,file= paste0(output, "performance_across_subsamples.tex"))
options(knitr.kable.NA = NULL)
write.csv(result_3, paste0(output,"normalized_rmse.csv"), row.names = F)

##########################################

#########################################################################################
#
# PART IV
# HORSERACE: compare with other models -----------------------------------------------
#
#########################################################################################



# OLS with dummies for area
# using model B

set.seed(1234)
system.time({
  ols_model <- train(
    formula(paste0("usd_price_day ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))
write.csv(ols_model_coeffs_df, paste0(output,"ols_model_coeffs_df.csv"), row.names = F)

# * LASSO
# using extended model w interactions

set.seed(1234)
system.time({
  lasso_model <- train(
    formula(paste0("usd_price_day ~", paste0(predictors_E, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 0.25, by = 0.01)),
    trControl = train_control
  )
})

lasso_coeffs <- coef(
  lasso_model$finalModel,
  lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(lasso_coefficient = `1`)  # the column has a name "1", to be renamed

lasso_coeffs_non_null <- lasso_coeffs[!lasso_coeffs$lasso_coefficient == 0,]

regression_coeffs <- merge(ols_model_coeffs_df, lasso_coeffs_non_null, by = "variable", all=TRUE)
regression_coeffs %>%
  write.csv(file = paste0(output, "regression_coeffs.csv"), row.names = F)

# CART
set.seed(1234)
system.time({
  cart_model <- train(
    formula(paste0("usd_price_day ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "rpart",
    tuneLength = 10,
    trControl = train_control
  )
})

fancyRpartPlot(cart_model$finalModel, sub = "")
save_fig(cart_tree,"cart_tree", output, "large")
# GBM  -------------------------------------------------------
gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), # complexity of the tree
                         n.trees = (4:10)*50, # number of iterations, i.e. trees
                         shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)


set.seed(1234)
system.time({
  gbm_model <- train(formula(paste0("usd_price_day ~", paste0(predictors_2, collapse = " + "))),
                     data = data_train,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)
})
gbm_model


# much more tuning

#  faster, for testing
#gbm_grid2 <-  expand.grid(interaction.depth = c( 5, 7, 9, 11), # complexity of the tree
#                          n.trees = (1:10)*50, # number of iterations, i.e. trees
#                          shrinkage = c(0.05, 0.1), # learning rate: how quickly the algorithm adapts
#                          n.minobsinnode = c(10,20) # the minimum number of training set samples in a node to commence splitting
#)


# the next will be in final model, loads of tuning
#gbm_grid2 <-  expand.grid(interaction.depth = c(1, 3, 5, 7, 9, 11), # complexity of the tree
#                          n.trees = (1:10)*50, # number of iterations, i.e. trees
#                          shrinkage = c(0.02, 0.05, 0.1, 0.15, 0.2), # learning rate: how quickly the algorithm adapts
#                          n.minobsinnode = c(5,10,20,30) # the minimum number of training set samples in a node to commence splitting
#)
# !!!THIS model is tested, not giving a better result than RF, and takes hours to run!!!

#set.seed(1234)
#system.time({
#  gbm_model2 <- train(formula(paste0("usd_price_day ~", paste0(predictors_2, collapse = " + "))),
#                      data = data_train,
#                      method = "gbm",
#                      trControl = train_control,
#                      verbose = FALSE,
#                      tuneGrid = gbm_grid2)
#})
#gbm_model2


# and get prediction rmse and add to next summary table

# ---- compare these models

final_models <-
  list("OLS" = ols_model,
       "LASSO (model w/ interactions)" = lasso_model,
       "CART" = cart_model,
       "Random forest (smaller model)" = rf_model_1,
       "Random forest" = rf_model_2,
       "GBM (basic tuning)"  = gbm_model)

results <- resamples(final_models) %>% summary()


# Save output --------------------------------------------------------
# Model selection is carried out on this CV RMSE

result_4 <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

kable(x = result_4, format = "html", digits = 3, booktabs=TRUE, linesep = "") %>%
  cat(.,file= paste0(output,"horse_race_of_models_cv_rmse.html"))
write.csv(result_4, paste0(output, "horse_race_of_models_cv_rmse.csv"), row.names = TRUE)
          



# evaluate preferred model on the holdout set -----------------------------

result_5 <- map(final_models, ~{
  RMSE(predict(.x, newdata = data_holdout), data_holdout[["usd_price_day"]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("Holdout RMSE" = ".")

kable(x = result_5, format = "latex", digits = 3, booktabs=TRUE, linesep = "") %>%
  cat(.,file= paste0(output,"horse_race_of_models_houldout_rmse.tex"))
write.csv(result_5, paste0(output, "horse_race_of_models_houldout_rmse.csv"), row.names = TRUE)
#ch16-table-1-rf-models-turning-choices
#ch16-table-2-performance-across-subsamples
#ch16-table-3-horse-race-of-models-cv-rmse

# Predicted values
predictionlev_holdout_pred <- as.data.frame(predict(rf_model_2, newdata = data_holdout, interval="predict"))
predictionlev_holdout_conf <- as.data.frame(predict(rf_model_2, newdata = data_holdout, interval="confidence"))



predictionlev_holdout <- cbind(data_holdout[,c("usd_price_day","n_accommodates")],
                               predictionlev_holdout_pred)


# Create data frame with the real and predicted values
d <- data.frame(ylev=predictionlev_holdout[,"usd_price_day"], predlev=predictionlev_holdout[,3] )
# Check the differences

# Plot predicted vs price
level_vs_pred <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev), color = color[1], size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  #geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[2], se=F, size=0.8, na.rm=T)+
  geom_segment(aes(x = 0, y = 0, xend = 350, yend =350), size=0.5, color=color[2], linetype=2) +
  coord_cartesian(xlim = c(0, 250), ylim = c(0, 250)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  labs(y = "Price (US dollars)", x = "Predicted price  (US dollars)") +
  theme_bg() 
level_vs_pred
save_fig("ch14-figure-8a-level-vs-pred", output, "small")

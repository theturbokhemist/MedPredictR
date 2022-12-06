data <- read.csv("insurance.csv")
data <- data[complete.cases(data),]

#linear regression
tc <- trainControl(method = "cv", number = 20)
lm1_cv <- train(Charges~., data = data, method = "lm", preProcess = c("center", "scale"),
                trControl = tc) 
lm1_cv
lm1_cv$results

#step regression
step_model <- train(Charges ~ ., data = data, preProcess = c("center", "scale"),
                    method = "lmStepAIC", trControl = tc, trace = FALSE)
step_model$results
step_model

#tr <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
#ridge regression/lasso/elastic net
glmnet_model <- train(Charges ~ ., data = data, 
                      preProcess = c("center", "scale"),
                      method = "glmnet", trControl = tc)

glmnet_model$results
glmnet_model

#gradient boost
tg <- expand.grid(shrinkage = seq(0.1, 1, by = 0.2), 
                  interaction.depth = c(1, 3, 7, 10),
                  n.minobsinnode = c(2, 5, 10),
                  n.trees = c(100, 300, 500, 1000))

# Verbose is a parameter sent to the underlying modeling function
gbm_model <- train(Charges ~ ., data = data, 
                   method = "gbm", tuneGrid =tg, verbose = FALSE)

gbm_model$results
gbm_model

#gradient boost
gbm_model2 <- train(Charges ~ ., data = data, preProcess = c("center", "scale"),  trControl = tc,
                   method = "gbm", tuneLength = 4, verbose = FALSE)

gbm_model2$results
gbm_model2$bestTune

#xg boost
xg.boost <- train(Charges ~ ., data = data, preProcess = c("center", "scale"),
                  method = "xgbTree")

xg.boost$results
#neuralnet
svm_model <- train(Charges ~ ., data = data, preProcess = c("center", "scale"),
                              method = "svmPoly")


svm_model$results
####RMSE Plotting
df_RMSE <- data.frame(Model = c(0), RMSE = c(0), RMSESD = c(0))
df_RMSE[1,] <- c("Gradient Boost", gbm_model2$results[which.min(gbm_model2$results$RMSE), c("RMSE", "RMSESD")])
df_RMSE[2,] <- c("GLM (Elastic Net)", glmnet_model$results[which.min(glmnet_model$results$RMSE), c("RMSE", "RMSESD")])
df_RMSE[3,] <- c("Stepwise Regression", step_model$results[1, c("RMSE", "RMSESD")])
df_RMSE <- df_RMSE[order(df_RMSE$RMSE),]
df_RMSE$Model <- factor(df_RMSE$Model, levels = df_RMSE$Model)

plot_RMSE <- ggplot(df_RMSE, aes(x=Model, y=RMSE, fill=Model)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=RMSE-RMSESD, ymax=RMSE+RMSESD), width=.2,
                position=position_dodge(.9)) 

####MAE Plotting
df_MAE <- data.frame(Model = c(0), MAE = c(0), MAESD = c(0))
df_MAE[1,] <- c("Gradient Boost", gbm_model2$results[which.min(gbm_model2$results$MAE), c("MAE", "MAESD")])
df_MAE[2,] <- c("GLM (Elastic Net)", glmnet_model$results[which.min(glmnet_model$results$MAE), c("MAE", "MAESD")])
df_MAE[3,] <- c("Stepwise Regression", step_model$results[1, c("MAE", "MAESD")])
df_MAE <- df_MAE[order(df_MAE$MAE),]
df_MAE$Model <- factor(df_MAE$Model, levels = df_MAE$Model)

plot_MAE <- ggplot(df_MAE, aes(x=Model, y=MAE, fill=Model)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=MAE-MAESD, ymax=MAE+MAESD), width=.2,
                position=position_dodge(.9)) 



####R2 Plotting
df_R2 <- data.frame(Model = c(0), R2 = c(0), R2SD = c(0))
df_R2[1,] <- c("Gradient Boost", gbm_model2$results[which.max(gbm_model2$results$Rsquared), c("Rsquared", "RsquaredSD")])
df_R2[2,] <- c("GLM (Elastic Net)", glmnet_model$results[which.max(glmnet_model$results$Rsquared), c("Rsquared", "RsquaredSD")])
df_R2[3,] <- c("Stepwise Regression", step_model$results[1, c("Rsquared", "RsquaredSD")])
df_R2 <- df_R2[order(df_R2$R2, decreasing = T),]
df_R2$Model <- factor(df_RMSE$Model, levels = df_RMSE$Model)
df_R2

plot_R2 <- ggplot(df_R2, aes(x=Model, y=R2, fill=Model)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=R2-R2SD, ymax=R2+R2SD), width=.2,
                position=position_dodge(.9)) 



metricPlotsList <- list("RMSE" = plot_RMSE, "MAE" = plot_MAE, "RSquared" = plot_R2)
metricPlotsList["RMSE"]

##

if (interactive()) {
  
  ui <- fluidPage(
    numericInput("obs", "Observations:", 10, min = 1, max = 101, step = 5),
    verbatimTextOutput("value")
  )
  server <- function(input, output) {
    output$value <- renderText({ input$obs })
  }
  shinyApp(ui, server)
}
suppressMessages(library(shiny))
suppressMessages(library(shinythemes))
suppressMessages(library(dplyr))
suppressMessages(library(readr))
suppressMessages(library(ggplot2))
suppressMessages(library(purrr))
suppressMessages(library(shinydashboard))
suppressMessages(require(formattable))
suppressMessages(require(tibble))
suppressMessages(require(DT))
suppressMessages(require(stringr))
suppressMessages(require(lubridate))
suppressMessages(require(rvest))
suppressMessages(require(httr))
suppressMessages(require(twitteR))
suppressMessages(require(plotly))
suppressMessages(require(data.table))
suppressMessages(require(viridis))
suppressMessages(require(shinyBS))
suppressMessages(require(tidyr))
suppressMessages(require(ggridges))
suppressMessages(require(GGally))
suppressMessages(library(fmsb))
suppressMessages(library(RColorBrewer))
suppressMessages(library(scales))
suppressMessages(library(caret))
suppressMessages(library(shinycssloaders))
suppressMessages(library(data.table))
suppressMessages(library(shinyWidgets))
suppressMessages(library(e1071))
suppressMessages(library(randomForest))
suppressMessages(library(parallel))
suppressMessages(library(doParallel))
suppressMessages(library(tictoc))
suppressMessages(library(shinycssloaders))
suppressMessages(library(mboost))
suppressMessages(library(import))
suppressMessages(library(gbm))
suppressMessages(library(lattice))
#Packages for model building
suppressMessages(library(rpart))
suppressMessages(library(party))
suppressMessages(library(plyr))
suppressMessages(library(partykit))
suppressMessages(library(arm))
suppressMessages(library(earth))
suppressMessages(library(ipred))
suppressMessages(library(xgboost))
suppressMessages(library(gbm))
suppressMessages(library(evtree))
suppressMessages(library(randomGLM))
suppressMessages(library(nnet))
suppressMessages(library(randomForest))
suppressMessages(library(foreach))
suppressMessages(library(ranger))
suppressMessages(library(extraTrees))
suppressMessages(library(inTrees))
suppressMessages(library(RRF))
suppressMessages(library(bartMachine))
suppressMessages(library(LogicReg))
suppressMessages(library(bst))
suppressMessages(library(h2o))
suppressMessages(library(kernlab))
suppressMessages(library(randomGLM))
suppressMessages(library(msaenet))
suppressMessages(library(pls))
suppressMessages(library(RSNNS))
suppressMessages(library(kknn))
suppressMessages(library(LiblineaR))

 fixture <- read_csv('D:/afl/shinyapps.io4/fixture.csv')
#fixture <- read_csv('fixture.csv')

#res <- read.csv('D:/afl/shinyapps.io4/res.csv')
cps <- read.csv('D:/afl/shinyapps.io4/cps.csv') %>% dplyr::filter(Status == 'Home') %>% dplyr::rename(Home.Team = 'Team', Away.Team = 'Opposition')
#cps <- read.csv('cps.csv') %>% dplyr::filter(Status == 'Home') %>% dplyr::rename(Home.Team = 'Team', Away.Team = 'Opposition')

rs <- cps %>% arrange(Date) %>% distinct(Round.Season)
rs1 <- fixture %>% arrange(Date) %>% distinct(Round.Season)


cps$Round.Season <- factor(cps$Round.Season, levels = rs$Round.Season)
fixture$Round.Season <- factor(fixture$Round.Season, levels = rs1$Round.Season)

levels_rnd <- levels(fixture$Round.Season)
rnds_noFinals <- levels_rnd[!levels_rnd %like% 'Final']
closest_round <- which(abs(difftime(fixture$Date, Sys.Date())) == min(abs(difftime(fixture$Date, Sys.Date()))) )
cl_rnd <- fixture[fixture$Date == fixture$Date[closest_round], ]$Round.Season


teams_sorted <- sort(unique(fixture$Home.Team))

# suppressMessages(library(plyr))
# detach("package:plyr", unload=TRUE)
# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_77")
# suppressMessages(library(rJava))
# suppressMessages(library(bartMachine))

#increase max file upload size to 30Mb
options(shiny.maxRequestSize=30*1024^2)

# fixture <- read_csv('F:/afl/shinyapps.io4/fixture.csv')
#fixture <- read_csv('D:/afl/shinyapps.io4/fixture.csv')
# fixture <- read_csv('fixture.csv')

#res <- read.csv('F:/afl/shinyapps.io4/res.csv')
#cps <- read.csv('F:/afl/shinyapps.io4/cps.csv')
# res <- read.csv('D:/afl/shinyapps.io4/res.csv')
# cps <- read.csv('D:/afl/shinyapps.io4/cps.csv')
#write.csv(cps %>% sample_n(60), 'D:/afl/shinyapps.io4/cps_small.csv')
# train_pre <- cps %>% sample_n(100)
# train_pre <- read.csv('C:/Users/User/Downloads/trainData.csv', header = T) %>% dplyr::select(-Team)
#train_pre <- read.csv('C:/Users/User/Downloads/trainData (4).csv', header = T) %>% dplyr::select(-Team)
# train_pre <- read.csv('D:/Downloads/trainData.csv', header = T)
# train_pre <- read.csv('D:/Downloads/trainData (1).csv', header = T)


# res <- read.csv('res.csv')
# cps <- read.csv('cps.csv')

#rs <- cps %>% arrange(Date) %>% distinct(Round.Season)
#rs1 <- fixture %>% arrange(Date) %>% distinct(Round.Season)


#cps$Round.Season <- factor(cps$Round.Season, levels = rs$Round.Season)
#fixture$Round.Season <- factor(fixture$Round.Season, levels = rs1$Round.Season)


#levels_rnd <- levels(fixture$Round.Season)
#rnds_noFinals <- levels_rnd[!levels_rnd %like% 'Final']
# latest_rnd <- as.numeric(str_remove(str_extract(rnds_noFinals[length(rnds_noFinals)], '^..'), '_') )
# latest_season <- (as.numeric(str_remove(str_extract(rnds_noFinals[length(rnds_noFinals)], '\\d\\d\\d\\d$'), '_') )-1)


#closest_round <- which(abs(difftime(fixture$Date, Sys.Date())) == min(abs(difftime(fixture$Date, Sys.Date()))) )

#cl_rnd <- fixture[fixture$Date == fixture$Date[closest_round], ]$Round.Season

header <- dashboardHeader(title = "Match Footy")


#' model_list <- list(
#'    'Bagged CART' = 'treebag', 
#'    'Bagged MARS' = 'bagEarth',
#'    'Bagged MARS using gCV Pruning' = 'bagEarthGCV',
#'    'Bayesian Generalised Linear Model' = 'bayesglm', 
#'    'Boosted Generalised Additive Model' = 'gamboost',
#'    'Boosted Generalised Linear Model' = 'glmboost', 
#'    # Too slow
#'    #'Boosted Tree' = 'blackboost',
#'    'CART' = 'rpart',
#'    'Conditional Inference Tree' = 'ctree2',
#'    'eXtreme Gradient Boosting' = 'xgbDART',
#'    'Generalised Linear Model' = 'glm',
#'    'Multivariate Adaptive Regression Spline' = 'earth',
#'    'Stochastic Gradient Boosting' = 'gbm',
#'    'Tree Models from Genetic Algorithms' = 'evtree',
#'    'Bagged Model' = 'bag',
#'    'Conditional Inference Random Forest' = 'cforest',
#'    'Ensembles of Generalised Linear Models' = 'randomGLM',
#'    'Model Averaged Neural Network' = 'avNNet',
#'    'Parallel Random Forest' = 'parRF',
#'    'Random Forest' = 'ranger', 
#'    'Random Forest by Randomisation' = 'extraTrees',
#'    'Random Forest Rule-Based Model' = 'rfRules',
#'    'Regularised Random Forest' = 'RRF',
#'    'Bayesian Additive Regression Trees' = 'bartMachine',
#'    'Logic Regression' = 'logreg',
#'    'Boosted Linear Model' = 'BstLm',
#'    'Boosted Smoothing Spline' = 'bstSm',
#'    'Gradient Boosting Machines' = 'gbm_h2o',
#'    'Gaussian Process' = 'gaussprLinear',
#'    'Gaussian Process with Polynomial Kernel' = 'gaussprPoly',
#'    'Gaussian Process with Radial Basis Function Kernel' = 'gaussprRadial',
#'    'Ensembles of Generalised Linear Models' = 'randomGLM',
#'    'glmnet' = 'glmnet_h2o',
#'    'Multi-Step Adaptive MCP-Net' = 'msaenet',
#'    'Partial Least Squares' = 'kernelpls',
#'    'Support Vector Machines with Spectrum String Kernel' = 'svmSpectrumString',
#'    'Multi-Layer Perceptron' = 'mlp',
#'    'Neural Network' = 'nnet',
#'    'Radial Basis Function Network' = 'rbf',
#'    'k-Nearest Neighbours' = 'kknn',
#'    'L2 Regularised Support Vector Machine (dual) with Linear Kernel' = 'svmLinear3'
#'    )
model_list <- list(
   "Bagged CART" = "treebag",
   "Bayesian Generalised Linear Model" = "bayesglm",
   "Boosted Generalised Linear Model" = "glmboost",
   "CART" = "rpart",
   "Conditional Inference Tree" = "ctree2",
   "Generalised Linear Model" = "glm",
   "Multivariate Adaptive Regression Spline" = "earth",
   "Stochastic Gradient Boosting" = "gbm",
   "Boosted Linear Model" = "BstLm",
   "Gaussian Process with Radial Basis Function Kernel" = "gaussprRadial",
   "Partial Least Squares" = "kernelpls",
   "Multi-Layer Perceptron" = "mlp",
   "k-Nearest Neighbours" = "kknn",
   "L2 Regularised Support Vector Machine (dual) with Linear Kernel" = "svmLinear3")




# packages <- c('party', 'xgboost', 'plyr', 'earth', 'gbm', 'evtree', 'logicFS', 'randomGLM', 'nnet', 'randomForest', 'foreach',
#               'import', 'ranger', 'extraTrees', 'inTrees', 'RRF', 'bartMachine', 'LogicReg', 'bst', 'h2o', 'kernlab', 'randomGLM',
#               'msaenet', 'pls', 'RSNNS', 'FCNN4R', 'nnet', 'kknn', 'LiblineaR')
# install.packages(packages)



#vec <- c('glmboost', 'blackboost', 'rpart')



# output_column <- tabsetPanel(type = "tabs",
#                   tabPanel("Training Data Preview", 
#                            fluidRow(
#                                  br(),
#                                  column(width = 11, DT::dataTableOutput("table"))
#                                     )
#                            )
#                   )
# output_column_test <- tabsetPanel(type = "tabs",
#                   tabPanel("Testing Data Preview", 
#                            fluidRow(
#                                  br(),
#                                  column(width = 11, DT::dataTableOutput("tableTest"))
#                                     )
#                            )
#                   )

sidebar <- dashboardSidebar(
      sidebarMenu(
         tags$li(class = "dropdown",
                 tags$a(href="https://matchfooty.com", target="_blank",
                        tags$img(height = "195px",
                                 src="https://scontent.fbne6-1.fna.fbcdn.net/v/t1.0-9/87156067_102324474702375_3793180717710573568_n.png?_nc_cat=103&_nc_sid=a61e81&_nc_ohc=lGGnaPhIO4oAX8jymVo&_nc_ht=scontent.fbne6-1.fna&oh=850c190c61b781fbd901d3a00d01e01c&oe=5EFBDFF9")
                 )
         ),
         fileInput(inputId='importTrainData', label=NULL, buttonLabel = 'Import Train Data'),
         fileInput(inputId='importTestData', label=NULL, buttonLabel = 'Import Test Data')
      ),
      collapsed = F)


#output$output_column <- renderUI({
   
 #output_column <-   
   # if( !is.null(trainData()) ){
   # 
   # column(12, br(), uiOutput('trainDim'), br(),
   #        tabsetPanel(type = "tabs", id = 'trainingPreview',
   #                    tabPanel("Training Data Preview",
   #                             fluidRow(
   #                                br(),
   #                                column(width = 11, DT::dataTableOutput("table"))
   #                             )
   #                    )),
   #        tabsetPanel(type = 'tabs', id = 'trainingResults',
   #                    tabPanel("Training Model Results",
   #                             fluidRow(
   #                                br(),
   #                                column(width = 11, withSpinner( DT::dataTableOutput("trainTable") )),
   #                                column(11, plotOutput(outputId = 'train_plot1')),
   #                                column(11, plotOutput(outputId = 'train_plot2'))
   #                                )
   #                             )
   #                    )
   #        )
   # )
   
   # }
#})


# body <- dashboardBody(
#    tabsetPanel(type = "tabs", id = 'body',
#                tabPanel("Training Data",
#                   fluidRow(uiOutput('input_column'),
#                            fluidRow(),
#                            uiOutput('output_column'))
#                            #output_column)
#                   ),
#                tabPanel("Testing Data",
#                         fluidRow(uiOutput('output_column_test'))
#                         ),
#                tabPanel("Model Predictions",
#                         fluidRow(uiOutput('input_column_predictTab'))
#                )
#                )
#    )


ui <- dashboardPage(
      skin = 'green',
      header = header, 
      sidebar = sidebar,   
      body = uiOutput('body')
)

#team_diff_func(5, '1_2019', RNDS = c('1_2019', '2_2019', '3_2019', '4_2019', '5_2019', '6_2019', '7_2019'))
server <- function(input, output, session){

   
   trainData <- reactive({
      
      inFile <- input$importTrainData
      if(is.null(inFile)) NULL
      else read.csv(inFile$datapath)
      })
   testData <- reactive({
      
      inFile1 <- input$importTestData
      if(is.null(inFile1)) NULL
      else read.csv(inFile1$datapath)
      })

    
   #vec <- c(model_list[[1]], model_list[[8]])
   #vec <- c(model_list[[8]])
   models <- eventReactive(
      {
         input$importTraiData
         input$button5   
      }, 
      {
      
      tdf <- model_list %>% 
      enframe(name = 'model', value = 'method') %>%
      unnest(cols = c('method')) %>% 
         inner_join(tibble('method' = c(input$models1, input$models2) ), by = 'method' ) %>% 
          #inner_join(tibble('method' = vec)) %>% 
      as.data.frame()



   train.x.func <- function(x){x[, !(colnames(x) %in% isolate(input$selectY) ) ]}
   # train.x.func <- function(x){x[, !(colnames(x) == 'Win') ]}
   # train.x.func <- function(x){x[, !(colnames(x) %in% 'Margin') ]}
   train.y.func <- function(x){
      
      
      # x <- train_pre; data <- x[, colnames(x) == 'Win']
      # x <- train_pre; data <- x[, colnames(x) == 'Margin']
      
      data <- x[, colnames(x) == isolate(input$selectY) ]
      
      #convert to factor if the variable is for classification
      if( length(unique(data)) <= 2 ){

         data <- factor(data)
      }
      return(data)
   }
   
   #train_data$params
         train_data <- list(
              isolate(trainData1())
            # train_pre #%>% mutate(Win = sample(c(0,1), nrow(train_pre), replace = T))
            ) %>%
            enframe(name = 'id', value = 'rawdata') %>%
            transmute(
               Train.X = map(rawdata, ~train.x.func(.x)),
               Train.Y = map(rawdata, ~train.y.func(.x)),
               ) %>%
            transmute(
               params = map2(Train.X, Train.Y,  ~ list(X = .x, Y = .y))
                  )
         # train_data <- train_data$params[[1]] %>% enframe(name = 'id', value = 'params')

         model.func <- function(method, data){
            # caret::train(x = data$X, y = data$Y, method = method, preProcess = preProcess(method = c('center', 'scale', 'knnImpute', 'corr', 'zv', 'nzv')))
            caret::train(x = data$X, y = data$Y, method = method)
         }

         # train_models.win <- map2(tdf$method, train_data$params.win, ~model.func(.x, .y))
         # train_models.margin <- map2(tdf$method, train_data$params.margin, ~model.func(.x, .y))
         
         withProgress(message = 'Training Models', value = sample(seq(0.3, 0.7, 0.1), 1), {     
         train_models <- purrr::map2(train_data$params, tdf$method, ~model.func(.y, .x))
         })

    
         #Are the model results for classification or regression
         if( train_models[[1]]$modelType != 'Regression' ){
            
            train_models <- train_models %>%
               enframe(name = 'model', value = 'modelFits') %>%
               mutate(
                  Accuracy=map_dbl(modelFits,~max(.x$results$Accuracy)),
                  Kappa=map_dbl(modelFits,~max(.x$results$Kappa[.x$results$Accuracy == max(.x$results$Accuracy)])[1] ),
                  AccuracySD=map_dbl(modelFits,~min(.x$results$AccuracySD[.x$results$Accuracy == max(.x$results$Accuracy)])[1]),
                  KappaSD=map_dbl(modelFits,~min(.x$results$KappaSD[.x$results$Accuracy == max(.x$results$Accuracy)])[1] ),
                  bestTune=map(modelFits,~.x$bestTune)
               ) %>%
               mutate(model = tdf$model) %>% 
               #dplyr::select(model, Accuracy, Kappa, AccuracySD, KappaSD) %>% 
               dplyr::arrange(desc(Accuracy)) 
            
         }else{
            
            train_models <- train_models %>%
               enframe(name = 'model', value = 'modelFits') %>%
               mutate(
                  RMSE=map_dbl(modelFits,~min(.x$results$RMSE)),
                  RMSESD= map_dbl(modelFits,~min(.x$results$RMSESD[.x$results$RMSE == min(.x$results$RMSE)])[1] ),
                  Rsq=map_dbl(modelFits,~max(.x$results$Rsquared[.x$results$RMSE == min(.x$results$RMSE)])[1] ),
                  RsqSD=map_dbl(modelFits,~min(.x$results$RsquaredSD[.x$results$RMSE == min(.x$results$RMSE)])[1] ),
                  bestTune=map(modelFits,~.x$bestTune) 
               ) %>%
               dplyr::mutate(model = tdf$model) %>% 
               #dplyr::select(model, RMSE, RMSESD, Rsq, RsqSD) %>% 
               dplyr::arrange(RMSE)
         }

         # train_models <- format(train_models, digits = 2)
         # train_models <- train_models %>%
         #    mutate_if(is.numeric, round(digits = 2))
         train_models
   })
   
   modelsTest <- eventReactive(
      {
         input$button5
         input$importTestData
         models()
      },
      {
      
      tdf <- model_list %>% 
      enframe(name = 'model', value = 'method') %>%
      unnest(cols = c('method')) %>%
         inner_join(data.frame(method = c(input$models1, input$models2) ), by = 'method' ) %>% 
         #inner_join(tibble(method = c('earth', 'kernelpls', 'mlp') ), by = 'method' ) %>% 
      as.data.frame()


#testData <- read.csv('C:/Users/User/Downloads/testData (13).csv')
#testData <- read.csv('C:/Users/User/Downloads/testData (14).csv')
#testData <- read.csv('C:/Users/User/Downloads/testData.csv')
#testData1 <- read.csv('C:/Users/User/Downloads/testData.csv')
   
if( length( unique( testData1()[[isolate(input$selectY)]] ) ) <= 2 ){
   #Classification
   
   obs <- testData1() %>% dplyr::select(Round.Season, Win) %>%  dplyr::group_by(Round.Season) %>% nest(data = c(Win))
   #obs <- testData1 %>% dplyr::select(Round.Season, Win) %>%  dplyr::group_by(Round.Season) %>% nest(data = c(Win))
   obs <- purrr::map(obs$data, function(x)factor(x$Win))
   
   pred_data <- testData1() %>% 
       dplyr::select(names(trainData1()), -c( isolate(input$selectY) ), Round.Season ) %>% 
      dplyr::group_by(Round.Season) %>% 
      nest()
   # pred_data <- testData %>% dplyr::select(names(testData), -c( 'Win', 'Team' ) ) %>%
   #    dplyr::group_by(Round.Season) %>%
   #    tidyr::nest()
   
   preds <- purrr::map2( pred_data$data, models()$modelFits, ~as.factor(predict(.y, newdata = .x)) )
   #preds <- purrr::map2( train_models, pred_data$data[1], ~as.factor(predict(.y, newdata = .x)) )
   
   
   conf <- purrr::map2(preds, obs, ~caret::confusionMatrix(.x, .y))
   
   spec <- purrr::map2(preds, obs, ~caret::specificity(.x, .y))
   sens <- purrr::map2(preds, obs, ~caret::sensitivity(.x, .y))
   negPredVal <- purrr::map2(preds, obs, ~caret::negPredValue(.x, .y))
   posPredVal <- purrr::map2(preds, obs, ~caret::posPredValue(.x, .y))
   
   
   test_results <- purrr::map(conf, ~.x$overall )
   
   Kappa <- purrr::map2(preds, obs, ~Kappa.test(.x, .y))
   KappaCI <- purrr::map(Kappa, ~.x$Result$conf.int)
   KappaCI_l <- purrr::map(KappaCI, ~.x[1]); KappaCI_u <- purrr::map(KappaCI, ~.x[2]) 
   
   
   test_results <- purrr::map2(purrr::map2(test_results, KappaCI_l, ~c(.x, KappaLower = .y)), KappaCI_u, ~c(.x, KappaUpper = .y))
   names_test1 <- names(test_results[[1]])
   
   stat_add <- function(x, y, stat){
      names1 <- names(x)
      x <- append(x, y)
      names(x) <- append(names1, stat)
      return(x)
   }
   
   test_results <- purrr::map2(test_results, sens, ~stat_add(.x, .y, 'Sensitivity'))
   test_results <- purrr::map2(test_results, sens, ~stat_add(.x, .y, 'Specificity'))
   test_results <- purrr::map2(test_results, sens, ~stat_add(.x, .y, 'Negative Predictive Value'))
   test_results <- purrr::map2(test_results, sens, ~stat_add(.x, .y, 'Positive Predictive Value'))
   
   
   }else{
#Regression
   marg_pred_func <- function(x, y)as.numeric(predict(x, y))
   obs <- testData1() %>% dplyr::select(Round.Season, Margin) %>%  dplyr::group_by(Round.Season) %>% nest(data = c(Margin))
   #obs <- testData %>% dplyr::select(Round.Season, Margin) %>%  dplyr::group_by(Round.Season) %>% nest(data = c(Margin))
   obs <- purrr::map(obs$data, function(x)x$Margin)
   
   
   pred_data <- testData1() %>% 
      dplyr::select(names(trainData1()), -c( isolate(input$selectY) ), Round.Season ) %>% 
      dplyr::group_by(Round.Season) %>% 
      nest()
   
   # pred_data <- testData %>% dplyr::select(names(testData), -c( 'Margin', 'Team' ) ) %>%
   #    dplyr::group_by(Round.Season) %>%
   #    tidyr::nest()
   
   preds <- purrr::map2( list(pred_data), models()$modelFits, ~marg_pred_func(.y, .x))
   #preds <- purrr::map2( pred_data$data, train_models, ~as.factor(predict(.y, newdata = .x)) )
   
   test_results <- purrr::map2(obs, preds, ~postResample(.y, .x))
}   

test_res_names <- purrr::map(test_results, ~names(.x))[[1]]


            test_models <- test_results %>%
               enframe(name = 'model', value = 'value') %>%
               tidyr::unnest(cols = 'value') %>% 
               mutate(stat = test_res_names) %>%
               dplyr::mutate(model = rep(tdf$model, each = length(test_res_names)) ) %>%
               mutate(value = round(value, 2)) %>% 
               tidyr::pivot_wider(names_from = 'stat', values_from = c('value') ) %>% 
               as.data.frame()

            options(digits = 2)
      test_models <- format(test_models, digits = 2)
      
      list(test_models = test_models, preds = preds)
         
   })


   output$trainTable <- renderDataTable(options = list(scrollX = TRUE),
      
         models() %>% 
            dplyr::select( -c('modelFits', 'bestTune') ) %>% 
            dplyr::mutate_if(is.numeric, round, 2) %>% 
            as.data.frame()
   )
   output$testTable <- renderDataTable(options = list(scrollX = TRUE),
      
         if( 'AccuracyNull' %in% names(modelsTest()$test_models) ){
            
            modelsTest()$test_models %>% dplyr::select(-c('AccuracyNull', 'AccuracyPValue', 'McnemarPValue')) %>% 
               as.data.frame()
         }else{
            
            modelsTest()$test_models %>% as.data.frame()
         }
      
         
   )
   
   output$trainDim <- renderUI({
      
      if(!is.null(trainData())){
         #keep nrows the rows of the original dataset imported and update cols to change when user selects/deselects features
         h4(paste('The training data has', nrow(trainData()), 'rows &', ncol(trainData1()), 'variables'))
         
      }
   })
   output$testDim <- renderUI({
      
      if(!is.null(testData()) & all( c(input$selectX1, input$selectX2, input$selectX3, input$selectY) %in% names(testData()) ) ){
         
         h4(paste('The testing data has', nrow(testData()), 'rows &', ncol(testData1()), 'variables'))
         }else if( is.null(testData()) ){
            
            h5(paste('Test Data not yet imported'))
         }else{
            
            h5(paste('Error: Test Data does not have the same features as Training Data'))
         }
   })
   
   output$table <- DT::renderDataTable(options = list(scrollX = TRUE),
                                       
                                       trainData1()[seq_len(50), ]
   )
   output$tableTest <- DT::renderDataTable(options = list(scrollX = TRUE),
                                       
                                       testData() %>% dplyr::select(input$selectY, Team, Round.Season, everything())
   )
   
   
   # observeEvent(input$button5, {
   #    
   #    removeUI('#table')
   # })
 
      
      
      
   
   
   
   
   
   output$output_column_test <- renderUI({
      
      if( !is.null(testData1()) ){
         column(12, br(), uiOutput('testDim'), br(),
                tabsetPanel(type = "tabs",
                            tabPanel("Testing Data Preview",
                                     fluidRow(
                                        br(),
                                        column(width = 11, DT::dataTableOutput("tableTest"))
                                        )
                                     ),
                            tabPanel("Testing Model Results",
                                     fluidRow(
                                        br(),
                                        column(width = 11, DT::dataTableOutput("testTable") ),
                                        column(11, plotOutput(outputId = 'test_plot1')),
                                        column(11, plotOutput(outputId = 'test_plot2')) 
                                     )
                                     )
                            )
                )
      }
   })
   
  
   output$train_plot1 <- renderPlot({
      
      m_names <- model_list %>% as.character()
      
      if( any(names(models()) %like% 'Accuracy') ){
         
         models() %>%
            dplyr::select(model, Accuracy, Kappa, AccuracySD, KappaSD) %>% 
            dplyr::mutate(model = factor(model),
                   Accuracy = as.numeric(Accuracy),
                   AccuracySD = as.numeric(AccuracySD)) %>% 
            ggplot(aes(model, col = model)) +
            geom_point(aes(y = Accuracy), size = 2) +
            geom_errorbar(aes(ymin = (Accuracy-AccuracySD),ymax= (Accuracy+AccuracySD)),size=.5,width=.15) + 
            ggthemes::theme_clean() +
            theme(axis.text.x = element_text(angle = 90))
      }else{
         
         models() %>%
            dplyr::select(model, RMSE, RMSESD, Rsq, RsqSD) %>%  
            dplyr::mutate(model = factor(model),
                          RMSE = as.numeric(RMSE),
                          RMSESD = as.numeric(RMSESD)) %>% 
            ggplot(aes(model, col = model)) +
            geom_point(aes(y = RMSE), size = 2) + 
            geom_errorbar(aes(ymin = RMSE-RMSESD,ymax= RMSE+RMSESD),size=.5,width=.15) +
            ggthemes::theme_clean() +
            theme(axis.text.x = element_text(angle = 90))
      }
   })
   output$train_plot2 <- renderPlot({
      
      m_names <- model_list %>% as.character()
      
      if( any(names(models()) %like% 'Accuracy') ){
         
         models() %>%
            dplyr::mutate(model = factor(model),
                   Kappa = as.numeric(Kappa),
                   KappaSD = as.numeric(KappaSD)) %>% 
            ggplot(aes(model, col = model)) +
            geom_point(aes(y = Kappa), size = 3) +
            geom_errorbar(aes(ymin = (Kappa-KappaSD),ymax= (Kappa+KappaSD)),size=.5,width=.15) + 
            ggthemes::theme_clean() +
            theme(axis.text.x = element_text(angle = 90))
      }else{
         
         models() %>%
            dplyr::mutate(model = factor(model),
                          Rsq = as.numeric(Rsq),
                          RsqSD = as.numeric(RsqSD)) %>% 
            ggplot(aes(model, col = model)) +
            geom_point(aes(y = Rsq), size = 3) + 
            geom_errorbar(aes(ymin = Rsq-RsqSD,ymax= Rsq+RsqSD),size=.5,width=.15) +
            ggthemes::theme_clean() +
            theme(axis.text.x = element_text(angle = 90))
      }
   })
   
   output$test_plot1 <- renderPlot({
      
      if(!is.null(modelsTest())){
      m_names <- model_list %>% as.character()
      
      if( 'Accuracy' %in% names(modelsTest()$test_models) ){
         
         modelsTest()$test_models %>%
            #dplyr::select(model, Accuracy, Kappa, AccuracySD, KappaSD) %>% 
            dplyr::mutate(model = factor(model),
                   Accuracy = as.numeric(Accuracy),
                   AccuracyLower = as.numeric(AccuracyLower),
                   AccuracyUpper = as.numeric(AccuracyUpper),
                   Kappa = as.numeric(Kappa),
                   KappaLower = as.numeric(KappaLower),
                   KappaUpper = as.numeric(KappaUpper)) %>% 
            ggplot(aes(model, col = model)) +
            geom_point(aes(y = Accuracy), size = 3) +
            geom_errorbar(aes(ymin = AccuracyLower,ymax= AccuracyUpper),size=.5,width=.25) + 
            ggthemes::theme_clean() +
            theme(axis.text.x = element_text(angle = 90))
      }else{
         
         modelsTest()$test_models %>%
            dplyr::mutate(model = factor(model),
                          RMSE = as.numeric(RMSE)) %>% 
            #dplyr::select(model, RMSE, RMSESD, Rsq, RsqSD) %>%  
            #tidyr::pivot_longer(cols = c('RMSE', 'Rsquared', 'MAE'), names_to = 'metric', values_to = 'value') %>% 
            # dplyr::mutate(model = factor(model),
            #               metric = factor(metric),
            #               value = as.numeric(value)) %>% 
            ggplot(aes(x=model, y = RMSE, col = model)) +
            geom_point(size = 3) + 
            #geom_errorbar(aes(ymin = RMSE-RMSESD,ymax= RMSE+RMSESD),size=.5,width=.25) + 
            ylab('') + xlab('model') +
            #facet_wrap(~metric, scales = 'free'   ) +
            ggthemes::theme_clean() +
            theme(axis.text.x = element_text(angle = 90))
      }
      }
   })
   
   output$test_plot2 <- renderPlot({
      
      if(!is.null(modelsTest())){
      m_names <- model_list %>% as.character()
      
      if( 'Accuracy' %in% names(modelsTest()$test_models) ){
         
         modelsTest()$test_models %>%
            #dplyr::select(model, Accuracy, Kappa, AccuracySD, KappaSD) %>% 
            dplyr::mutate(model = factor(model),
                   Accuracy = as.numeric(Accuracy),
                   AccuracyLower = as.numeric(AccuracyLower),
                   AccuracyUpper = as.numeric(AccuracyUpper),
                   Kappa = as.numeric(Kappa),
                   KappaLower = as.numeric(KappaLower),
                   KappaUpper = as.numeric(KappaUpper)) %>% 
            ggplot(aes(model, col = model)) +
            geom_point(aes(y = Kappa), size = 3) +
            geom_errorbar(aes(ymin = KappaLower,ymax= KappaUpper),size=.5,width=.25) + 
            ggthemes::theme_clean() +
            theme(axis.text.x = element_text(angle = 90))
      }else{
         
         modelsTest()$test_models %>%
            dplyr::mutate(model = factor(model),
                          Rsquared = as.numeric(Rsquared)) %>% 
            ggplot(aes(x=model, y = Rsquared, col = model)) +
            geom_point(size = 3) + 
            #geom_errorbar(aes(ymin = RMSE-RMSESD,ymax= RMSE+RMSESD),size=.5,width=.25) + 
            ylab('') + xlab('model') +
            #facet_wrap(~metric, scales = 'free'   ) +
            ggthemes::theme_clean() +
            theme(axis.text.x = element_text(angle = 90))
      }
      }
   })
   
      
   
   # output$input_column <- renderUI({
   #    
   #    if(!is.null(testData1())){
   #    
   #    
   #          box(width=12,title = 'Model Building', solidHeader = T, status = 'danger', collapsed = F, collapsible = T,
   #           
   #           column(6, uiOutput('response')), br(),br(),br(),br(),br(),
   #           # Spreads the features out nice and evenly across the page
   #           column(12,
   #           column(4, uiOutput('features1')),br(),br(),column(4, uiOutput('features2')),column(4, uiOutput('features3'))
   #           ),
   #           br(),br(),br(),br(),br(),
   #           column(12,
   #           column(4, uiOutput('models1')),br(),br(),column(4, uiOutput('models2'))
   #           ), 
   #           column(2, 
   #                  shinyWidgets::actionBttn(inputId = 'button5', color = 'danger', style = 'unite',
   #                                           label = h2('Enter')), br(), br()
   #           )
   #       )
   #       
   # } 
   # })
   
   
   
   output$input_column_predictTab <- renderUI({
      
      test_rnds <- testData()$Round.Season
      test_rnds <- factor(test_rnds, levels = levels_rnd)
      
      if(is.null( models() )){
      
         fluidPage()
      }else{
      
         # fluidPage(
         #    box(width=4,title = uiOutput('title_update'), solidHeader = T, status = 'success', collapsed = T, collapsible = T,
         #        column(8, 
         #               selectInput(inputId = 'round', label = 'Predict Round', choices = test_rnds, selected = pred_rnd()[1])
         #        ),
         #        column(3, 
         #               shinyWidgets::actionBttn(inputId = 'button_pred', color = 'success', style = 'unite',
         #                                        label = h2('Enter')), br(), br()
         #        )
         #    )
         # )
   } 
   })

      
   output$features1 <- renderUI({
      
      #vars1 <- names(cps)[names(cps) != 'Margin']
      vars1 <- names(trainData())[ names(trainData()) != input$selectY ]
      l_vars1 <- length(vars1)
      index1 <- ceiling(l_vars1 / 3)
      features1 <- vars1[ seq_len(index1) ]
      
      checkboxGroupInput('selectX1', h4(strong('Select Features')), choices = features1,
                         selected = features1
      )
   })
   output$features2 <- renderUI({
      
      vars1 <- names(trainData())[ names(trainData()) != input$selectY ]
      l_vars1 <- length(vars1)
      index1 <- ceiling(l_vars1 / 3)
      features1 <- vars1[ seq_len(index1) ]
      
      vars2 <- names(trainData())[ !names(trainData()) %in% c(input$selectY, features1) ]
      # vars2 <- names(cps)[ !names(cps) %in% c('Margin', features1) ]
      l_features1 <- length(features1)
      
      features2 <- vars2[ seq_len(index1) ]
      
      checkboxGroupInput('selectX2', '', choices = features2,
                         selected = features2
      )
   })
   output$features3 <- renderUI({
      
      vars1 <- names(trainData())[ names(trainData()) != input$selectY ]
      l_vars1 <- length(vars1)
      index1 <- ceiling(l_vars1 / 3)
      features1 <- vars1[ seq_len(index1) ]
      
      vars2 <- names(trainData())[ !names(trainData()) %in% c(input$selectY, features1) ]
      # vars2 <- names(cps)[ !names(cps) %in% c('Margin', features1) ]
      l_features1 <- length(features1)
      
      features2 <- vars2[ seq_len(index1) ]
      
      features3 <- names(trainData())[ !names(trainData()) %in% c(input$selectY, features1, features2) ]
      #features3 <- names(cps)[ !names(cps) %in% c('Margin', features1, features2) ]
      
      checkboxGroupInput('selectX3', '', choices = features3,
                         selected = features3
      )
   })
   
   output$models_a <- renderUI({
      
      checkboxGroupInput('models1', h4(strong('Select Models')), choices = model_list[1:7])
   })
   output$models_b <- renderUI({
      
      checkboxGroupInput('models2', label = NULL, choices = model_list[8:14])
   })
   
   
   output$response <- renderUI({
      
      selected <- tail(names(trainData()), 1)
      selectInput('selectY', h4(strong('Select Response')), choices = names(trainData()), 
                  selected = selected
                  )
   })
   
   # pred_rnd <- eventReactive(input$button, {
   #    isolate(input$round)
   # })
   
   trainData1 <- reactive({
      
      if( !is.null(trainData()) ){
      trainData() %>% dplyr::select(input$selectY, input$selectX1, input$selectX2, input$selectX3) %>% as.data.frame()
      }
   })
   testData1 <- eventReactive(
      {
         input$importTrainData
         input$button5
         input$importTestData
      },
      {
         
         if( all( c(input$selectX1, input$selectX2, input$selectX3, input$selectY) %in% names(testData()) ) ){
            
            testData() %>% dplyr::select(input$selectY, input$selectX1, input$selectX2, input$selectX3) %>% as.data.frame()
         }
      
   })
   
   # output$title_update <- renderUI({
   #    
   #    rnd <- pred_rnd()
   #    #rnd <- 'Semi Final_2018'
   #    
   #    rnd_number <- str_split(rnd, '_')[[1]][1]
   #    rnd_season <- str_split(rnd, '_')[[1]][2]
   #    
   #    ifelse(rnd %in% rnds_noFinals, paste0('Prediction for Round ', rnd_number, ', ', rnd_season),
   #           paste0('Prediction for ', rnd_number, ', ', rnd_season)
   #           )
   #    
   # })
   #pred_rnd <- reactive({ input$round })
   
   test_prediction_table <- eventReactive(
      {
         input$importTrainData
         input$button5
         input$importTestData
         }, {
      
      #resulted matches
      #if(!is.null(modelsTest()) ){

      testDataRnds <- unique(testData()$Round.Season)
      #testDataRnds <- c('20_2019', '21_2019', '22_2019', '23_2019')
      # if(testDataRnds %in% cps$Round.Season){
         
         data <- cps %>% 
            dplyr::filter(Round.Season %in% testDataRnds) %>% 
            #dplyr::filter(Round.Season %in% '23_2019') %>% 
            dplyr::filter(Status == 'Home') %>% 
            dplyr::distinct(Round.Season, Home.Team, Away.Team, Margin) %>%  
            dplyr::mutate(Home.Team = as.character(Home.Team), Away.Team = as.character(Away.Team)) %>% 
            dplyr::mutate(`Actual Winner` = ifelse(Margin >=0, as.character(Home.Team), as.character(Away.Team) )) %>% 
            dplyr::rename(`Home Team` = Home.Team, `Away Team` = Away.Team)
         
         # if( length( unique( testData1()[[input$selectY]] ) ) <= 2 ){
            #classification   
            model_preds <- data.frame(preds = modelsTest()$preds %>% unlist) %>% 
         #model_preds <- tibble(preds = preds %>% unlist) %>% 
               dplyr::mutate(Model = rep(models()$model, each = length(teams_sorted))) %>% 
               #dplyr::mutate(Model = rep(names(models), each = 18)) %>% 
               dplyr::mutate(Team = rep(teams_sorted, length(models()$model)) )
               #dplyr::mutate(Team = rep(teams_sorted, length(models)) )
               
            data <- rep(list(data), length(models()$model)) %>% 
            #data <- rep(list(data), length(names(models))) %>% 
               bind_rows %>% 
               dplyr::mutate(Model = rep(models()$model, each = nrow(data))) %>%
               #dplyr::mutate(Model = rep(names(models), each = nrow(data))) %>%
               left_join(model_preds, by = c('Home Team' = 'Team', 'Model')) %>% 
               dplyr::mutate(`Predicted Winner` = ifelse(preds == 1, `Home Team`, `Away Team`)) %>% 
               dplyr::select(Round.Season, `Home Team`, `Away Team`, Margin, `Actual Winner`, `Predicted Winner`, Model) %>% 
               tidyr::pivot_wider(names_from = 'Model', values_from = c(`Predicted Winner`) ) %>% as.data.frame()
            
            
            
               
      #          
      #    }else{
      #       #Regression
      #       
      #    }
      #    #unresulted matches
      # }else if(testDataRnds %in% fixture$Round.Season){
      #    
      #    data <- fixture %>% 
      #       dplyr::filter(Round.Season == testDataRnds) %>% 
      #       dplyr::distinct(Round.Season, Home.Team, Away.Team) %>% 
      #       dplyr::rename(`Home Team` = Home.Team, `Away Team` = Away.Team)
      #    
      #    if( length( unique( testData1()[[input$selectY]] ) ) <= 2 ){
      #       #classification   
      #       
      #    }else{
      #       #Regression
      #       
      #    }
      #    #unresulted matches
      #    
      # }
            data %>% as.data.frame()
            
     # }
      
   })
  
   output$predTable <- DT::renderDataTable(options = list(scrollX = TRUE),
                       test_prediction_table()
                       )
      
   
   observeEvent(input$button5, {
      updateTabsetPanel(session, "trainingPreview",
                        selected = "trainingResults")
   })
   

   
#    output$output_column <- renderUI({
# 
#    if( !is.null(trainData1()) ){
#        
#       column(12, br(), uiOutput('trainDim'), br(),
#              tabsetPanel(type = "tabs", id = 'trainingPreview',
#                          tabPanel("Training Data Preview",
#                                   fluidRow(
#                                      br(),
#                                      column(width = 11, DT::dataTableOutput("table"))
#                                   )
#                          )),
#              tabsetPanel(type = 'tabs', id = 'trainingResults',
#                          tabPanel("Training Model Results",
#                                   fluidRow(
#                                      br(),
#                                      column(width = 11, withSpinner( DT::dataTableOutput("trainTable") )),
#                                      column(11, plotOutput(outputId = 'train_plot1')),
#                                      column(11, plotOutput(outputId = 'train_plot2'))
#                                   )
#                          )
#              )
#       )
#     }
# })
   
   output$body <- renderUI({
         
         dashboardBody(
            fluidPage(
               fluidRow(br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                        br(),br(),br(),br()),
               fluidRow(br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                        br(),br(),br(),br()),
               fluidRow(br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                        br(),br(),br(),br())
               
            )
         )
   })
   
   observeEvent(input$importTrainData,{
      
         output$body <- renderUI({
            
            dashboardBody(
               tabsetPanel(type = "tabs", id = 'body',
                           tabPanel("Training Data",
                                    fluidRow(
                                       #uiOutput('input_column'),
                                       box(width=12,title = 'Model Building', solidHeader = T, status = 'danger', collapsed = F, collapsible = T,
                                           
                                           column(6, uiOutput('response')), br(),br(),br(),br(),br(),
                                           # Spreads the features out nice and evenly across the page
                                           column(12,
                                                  column(4, uiOutput('features1')),br(),br(),column(4, uiOutput('features2')),column(4, uiOutput('features3'))
                                           ),
                                           br(),br(),br(),br(),br(),
                                           column(12,
                                                  column(4, uiOutput('models_a')),br(),br(),column(4, uiOutput('models_b'))
                                           ), 
                                           column(2, 
                                                  shinyWidgets::actionBttn(inputId = 'button5', color = 'danger', style = 'unite',
                                                                           label = h2('Enter')), br(), br()
                                           )
                                       ),
                                             fluidRow(),
                                             #uiOutput('output_column')
                                       column(12, br(), uiOutput('trainDim'), br(),
                                              tabsetPanel(type = "tabs", id = 'trainingPreview',
                                                          tabPanel("Training Data Preview",
                                                                   fluidRow(
                                                                      br(),
                                                                      column(width = 11, DT::dataTableOutput("table"))
                                                                   )
                                                          )),
                                              tabsetPanel(type = 'tabs', id = 'trainingResults',
                                                          tabPanel("Training Model Results",
                                                                   fluidRow(
                                                                      br(),
                                                                      column(width = 11, withSpinner( DT::dataTableOutput("trainTable") )),
                                                                      column(11, plotOutput(outputId = 'train_plot1')),
                                                                      column(11, plotOutput(outputId = 'train_plot2'))
                                                                   )
                                                          )
                                              )
                                       )
                                       )
                                    #output_column)
                           ),
                           tabPanel("Testing Data",
                                    fluidRow(uiOutput('output_column_test'))
                           ),
                           tabPanel("Model Predictions",
                                    #fluidRow(uiOutput('input_column_predictTab')),
                                    fluidRow(DT::dataTableOutput('predTable') )
                           )
               )
            )
         })
      
   })
         
   
         
         
   
}


shinyApp( ui = ui, server = server )

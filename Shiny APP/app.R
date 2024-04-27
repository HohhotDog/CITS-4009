library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ROCit)
library(pROC)


ui <- navbarPage(
  title = "CITS4009 Project 2",
  h5("Mingyu LIAN: 24046428 & Chensu YANG: 24035732"),
  tabPanel(
    "Single Variable Model Evaluation",
    h3("ROC Plot for Single Variables", style = "color:Darkred"),
    p("The ROC Plot shows the performance of each single variable model. There are 9 models in total will be compared, each of the model have its corresponding single variable. The closer the curve is to the top left corner, the better the model is."),
    p("There is also a red point line shows the null model performance, which can be treated as the worst model. Normally, each of the single variable model should have a better performance than the null model, which means the curve should be above the red point line.
      "),
    h5("Please use the multi-select box to choose the variables you want to plot.",style = "color:grey"),
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("selected_columns", "Choose Variable to Plot:",
                           choices = c("region", "lst mth view", "uploads", "lst mth subscribe","subscribers","views","Vtreat Missing","numeric date","category"),
                           selected = c("region","lst mth view", "uploads", "lst mth subscribe")) ,
        p('__________________________________'),
        h4("Conclusion"),
        p("From the observations, 4 variables: region, last month view, last month subcribe have notable better performance than other variables."),
        tags$a(href="https://www.youtube.com/watch?v=E-a7WDmpd4Q", "Click Here to Watch the Guide Video!")),
      mainPanel(
        plotOutput("ROC")
      )
    )
  ),
  tabPanel(
    "Multi-Variable Models Evaluation",
    h3("Model Indicators Comparison", style = "color:Darkred"),
    p("This comparison chart compares the peformance of 3 classifiers with its best variables combination respectively on test set. They are: Decision Tree Classifier with Variable Combination 2, 
    Logistic Regression Classifier with Variable Combination 3 and XGBoost Classifier with Variable Combination 2.Besides, There are 4 common used indicators in the charts, 
      they are: dev.norm, f1, precision and recall. 
      The higher the indicator value is, the better the model is."),
    p("You can compare the performance of the 3 models by choosing the indicator you want to compare."),
    
    h5("Please use the Radio Point to choose which indicator you would like to compare",style = "color:grey"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("indicator", "Choose an Indicator:",
                     choices = c("dev.norm", "f1", "precision", "recall"),
                     selected = c("dev.norm")) ,
        p('__________________________________'),
        h4("Conclusion"),
        p("From the observations, the third model (XGBoost Classifier with Variable Combination 2) has a reletively good performance, especially for f1 indicator. 
          Therefore, we choose this set as the final best model"),
        tags$a(href="https://www.youtube.com/watch?v=E-a7WDmpd4Q", "Click Here to Watch the Guide Video!")),
      mainPanel(
        plotOutput("perfPlot")
      )
    )
  ),
  tabPanel(
    "K-Means Clustering",
    h3("Clustering Plot (Two-Dimensional Display)", style = "color:Darkred"),
    p("This clustering plot shows the result of K-Means Clustering. The plot is a two-dimensional display of the data.
      The x-axis is the first principal component, and the y-axis is the second principal component. 
      The color of the points represents the cluster that the data point belongs to. The number of clusters is determined by the user. "),
    
    h5("Please use the slider choose the number of clusters you want to plot.",style = "color:grey"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("selectn", label="Choose k as clusters number:",
                    min=2, max=10,value=2) ,
        p('__________________________________'),
        h4("Conclusion"),
        p("From the evaluation method metioned in the report, 
          the clustering perform best when k=2. When check the k=2, 
          it shows a reletively clear clusters than k = others. However, there is still a huge overlap between two different clusters,
          which indicates it might not a good clustering. The reason might be traced from the amount limitation of obersvations. Therefore, the clustering might not be a good method for this dataset."),
        tags$a(href="https://www.youtube.com/watch?v=E-a7WDmpd4Q", "Click Here to Watch the Guide Video!")),
      mainPanel(
        plotOutput("cluster")
      )
    )
  )
)


server <- function(input, output) {
  output$ROC <- renderPlot({
    plot_roc <- function(predcol, outcol, colour_id=10, overlaid=F){ 
      ROCit_obj <- rocit(score=predcol, class=outcol==pos) 
      par(new=overlaid) 
      plot(ROCit_obj, 
           col = c(colour_id, 10), legend = FALSE, YIndex = FALSE, values = FALSE) 
    }
    
    if ("region" %in% input$selected_columns)
      plot_roc(shiny1$predregion, shiny1[,outcome],colour_id=1)  
    if ("lst mth view" %in% input$selected_columns)
      plot_roc(shiny1$predlast_month_view, shiny1[,outcome], colour_id=2, overlaid=T) 
    if ("uploads" %in% input$selected_columns)
      plot_roc(shiny1$preduploads, shiny1[,outcome],colour_id=3,overlaid=T) 
    if ("lst mth subscribe" %in% input$selected_columns)
      plot_roc(shiny1$predlast_month_subscribe, shiny1[,outcome], colour_id=4, overlaid=T)
    if ("subscribers" %in% input$selected_columns)
      plot_roc(shiny1$predsubscribers, shiny1[,outcome], colour_id=5, overlaid=T) 
    if ("views" %in% input$selected_columns)
      plot_roc(shiny1$predviews, shiny1[,outcome], colour_id=6, overlaid=T) 
    if ("Vtreat Missing" %in% input$selected_columns)
      plot_roc(shiny1$predsubscribers_for_last_30_days_isBAD, shiny1[,outcome], colour_id=7, overlaid=T) 
    if ("numeric date" %in% input$selected_columns)
      plot_roc(shiny1$prednumeric_date, shiny1[,outcome], colour_id=8, overlaid=T) 
    if ("category" %in% input$selected_columns)
      plot_roc(shiny1$predcategory, shiny1[,outcome], colour_id=13, overlaid=T)
    
    legend("bottomright", legend = input$selected_columns, col = 1:(length(input$selected_columns) + 1), lty = 1)
  })
  
  output$cluster <- renderPlot({
    k <- input$selectn
    groups <- kmeans(scaled_df, k, nstart=100, iter.max=100)$cluster
    kmclust.project2D <- cbind(project2D, cluster=as.factor(groups))
    kmclust.hull <- find_convex_hull(kmclust.project2D, groups)
    ggplot(kmclust.project2D, aes(x=PC1, y=PC2)) + geom_point(aes(shape=cluster, color=cluster)) +
      geom_polygon(data=kmclust.hull, aes(group=cluster, fill=cluster), alpha=0.4, linetype=0) + 
      labs(title = sprintf("Clustering Plot - K = %d", k)) +
      scale_x_continuous(name = "Principal Component 1") + scale_y_continuous(name = "Principal Component 2") +
      theme(legend.position="bottom", text=element_text(size=15),
            plot.title = element_text(hjust = 0.5, size = 19, face = "bold",color="darkred"),  
            axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13))
  })
  output$perfPlot <- renderPlot({
    data_to_plot <- all_perf_long[all_perf_long$metric == input$indicator,]
    ggplot(data_to_plot, aes(x=model_type, y=value, color=model_type)) +
      geom_point(size=8, position=position_dodge(0.8),show.legend = FALSE) +
      labs(title="Indicator Performance for 3 Classifiers", y="Indicator Value",x="") +
      theme_minimal() +
      theme(legend.position="bottom",text=element_text(size=15),
            plot.title = element_text(hjust = 0.5, size = 19, face = "bold",color="darkred"),  
            axis.title.y = element_text(size = 16, face = "bold",color="darkgray"), axis.text.x = element_text(size = 15, face = "bold") ,axis.text.y = element_text(size = 15, face = "bold") )
  })
}

shinyApp(ui = ui, server = server)
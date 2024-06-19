library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyverse)
library(readxl)
library(reshape)
library(plotly)
library(tidyr)
library(readxl)
library(sf)
library(plotly)
library(shinydashboardPlus)
library(dashboardthemes)
library(xgboost)
library(caret)
library(ggplot2)
library(pROC)
library(ggcorrplot)

df <- read.csv("telecom_churn.csv", sep = ";")
lang0 <- df[df$churn==0,"langganan_data"]
lang1 <- df[df$churn==1,"langganan_data"]
lang <- df[, "langganan_data"]

ui <- dashboardPage(
  skin = "purple-light",
  dashboardHeader(title = "Telco Churn Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("house")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("table-columns"),
               menuSubItem("Visualisasi", tabName = "visualisasi"),
               menuSubItem("Pemodelan", tabName = "pemodelan")),
      menuItem("Database", tabName = "database", icon = icon("database"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    tabItems(
      tabItem(tabName = "beranda",
              titlePanel(
                carousel(width = 12,
                         id = "mycarousel",
                         carouselItem(
                           tags$img(src = "https://github.com/SFikri-Project/sehatirakyatdashboard.github.io/raw/main/home.jpg")
                         ))
              
              )
      ),
      tabItem(tabName = "dashboard"),
      tabItem(tabName = "visualisasi",
              fluidRow(box(title = h1(strong("Aktivitas Pelanggan Telco Dalam Se-Bulan")), status = "primary", solidHeader = FALSE,
                           width = 12)),
              fluidRow(box(title = strong("Filter Pelanggan"),
                           selectInput("Churndf", " ",
                                       choices = c("All" = "all", "Churn" = 1, "No-Churn" = 0)),
                           width = 12
              )),
              fluidRow(
                infoBoxOutput(outputId = "Roam", width = 4),
                infoBoxOutput(outputId = "DayMins", width = 4),
                infoBoxOutput(outputId = "MonthlyCharge", width = 4)
              ),
              fluidRow(
                box(title = tags$b("Persentase Berlangganan Paket Data"), width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("BAR1DF", height = 300)),
                box(title = tags$b("Jumlah Menghubungi Customer Service"), width = 6, status = "primary", solidHeader = TRUE,
                    plotOutput("BAR2DF", height = 300))
              ),
              fluidRow(
                box(title = tags$b(tags$strong("Jumlah Denda Pelanggan (Dollar)"), style = "text-align:center;"),width = 6, status = "primary", solidHeader = TRUE,
                    sliderInput(input = "num",
                                label = "Pilih Panjang Bin",
                                value = 10, min = 1, max = 20),
                    plotOutput("HISTDF", height = 300)
                ),
                box(title = tags$b(tags$strong("Distribusi Variabel")), 
                    style = "text-align:center;", width = 6, status = "primary", solidHeader = TRUE,
                    selectInput(inputId = "selvio", 
                                label = "Pilih Variabel", 
                                choices = c("Lama Langganan" = "lama_langganan", "Penggunaan Data" = "penggunaan_data", 
                                            "Durasi Telepon" = "durasi_telpon","Banyak Telepon"="banyak_telpon", "Biaya Bulanan" = "biaya_bulanan", "Denda" = "denda", "Penggunaan Paket Roaming" = "durasi_min")),
                    plotOutput("violin", height = 320)
                )
              ),
              fluidRow(
                box(title = tags$b("Matriks Korelasi"), width = 12, status = "primary", solidHeader = TRUE,
                    DTOutput("correlation_table")
                ),
                box(title = tags$b("Pilih Variabel"), width = 12, 
                    box(checkboxGroupInput("selval1", label = "Pilih Variabel",
                                           choices = c("Lama Langganan" = "lama_langganan", "Contract Renewal" = "perbarui", "Penggunaan Data" = "penggunaan_data", "Telepon CS" = "telpon_cs", "Penggunaan Paket Roaming" = "durasi_min"),
                                           selected = c("penggunaan_data", "lama_langganan")),  
                        width = 6),
                    box(checkboxGroupInput("selval2", label = "Select Variabel",
                                           choices = c("Durasi Telepon" = "durasi_telpon","Banyak Telepon"="banyak_telpon", "Biaya Bulanan" = "biaya_bulanan", "Denda" = "denda"),
                                           selected = c("durasi_telpon", "biaya_bulanan")),  
                        width = 6)
                )
              )
      ),
      tabItem(tabName = "pemodelan",
              tabsetPanel(
                tabPanel("Regresi",
                         box(width = 6,title = "Prediksi Biaya Langganan Seluler Per Bulan",
                             fluidRow(
                               column(width = 6,
                                      numericInput("num1",
                                                   label = "Masukkan lama langganan:",
                                                   value = NA),
                                      numericInput("num2",
                                                   label = "Masukkan penggunaan data:",
                                                   value = NA),
                                      numericInput("num3",
                                                   label = "Masukkan durasi telpon:",
                                                   value = NA)
                               ),
                               column(width = 6,
                                      numericInput("num4",
                                                   label = "Masukkan banyak telpon:",
                                                   value = NA),
                                      numericInput("num5",
                                                   label = "Masukkan denda:",
                                                   value = NA),
                                      numericInput("num6",
                                                   label = "Masukkan durasi minimum:",
                                                   value = NA)
                               )
                             ),
                             fluidRow(
                               column(width = 12,
                                      actionButton("predict_btn", "Prediksi", class = "btn-primary"),
                                      br(), br()
                               )
                             ),
                             fluidRow(
                               uiOutput("prediksiBox")
                             ),
                             fluidRow(
                               box(width = 12,title = "Analisis Residual",
                                   plotOutput("res"))
                             )),
                         box(width = 6, height = 760, title="Data",
                             DT::dataTableOutput("tbl")),
                         fluidRow(
                           box(width = 12,title = "Analisis Regresi",
                               uiOutput("reglin"))),
                         fluidRow(
                           box(width = 12,
                               verbatimTextOutput("summary"))
                         )
                ),
                tabPanel("Klasifikasi",
                         fluidRow(
                           box(title = "Input Fitur", width = 6,
                               numericInput("lama_langganan", "Lama Langganan:", value = NA, min = 0),
                               numericInput("perbarui", "Perbarui:", value = NA, min = 0),
                               numericInput("penggunaan_data", "Penggunaan Data:", value = NA, min = 0),
                               numericInput("telpon_cs", "Telpon CS:", value = NA, min = 0),
                               numericInput("durasi_telpon", "Durasi Telpon:", value = NA, min = 0),
                               numericInput("banyak_telpon", "Banyak Telpon:", value = NA, min = 0),
                               numericInput("biaya_bulanan", "Biaya Bulanan:", value = NA, min = 0),
                               numericInput("denda", "Denda:", value = NA, min = 0),
                               numericInput("durasi_min", "Durasi Min:", value = NA, min = 0),
                               actionButton("predict_btn_klf", "Prediksi")
                           ),
                           box(title = "Hasil Prediksi", width = 6,
                               plotOutput("probability_pie"),
                               textOutput("interpretation")
                           )
                         ),
                         fluidRow(
                           box(title = "Ringkasan Model", width = 12,
                               verbatimTextOutput("model_summary"),
                               textOutput("accuracy_output"),
                               plotOutput("confusion_matrix_plot")
                           )
                         ))
              )),
      tabItem(tabName = "database",
              tabBox(id = "t2", width = 12,
                     tabPanel("Data", icon = icon("address-card"), dataTableOutput("dfb")),
                     tabPanel("Struktur", icon = icon("address-card"), verbatimTextOutput("structure")),
                     tabPanel("Summary", icon = icon("address-card"), verbatimTextOutput("sumari"))
              ))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  ### VISUALISASI
  output$Roam <- renderInfoBox({
    value <- if (input$Churndf == "0") {
      mean(df$durasi_min[df$churn == 0], na.rm = TRUE)
    } else if (input$Churndf == "1") {
      mean(df$durasi_min[df$churn == 1], na.rm = TRUE)
    } else {
      mean(df$durasi_min, na.rm = TRUE)
    }
    value = round(value,2)
    infoBox(tags$p("Durasi Roaming (Menit)", style = "font-weight:bold;"),
            value = paste(value, "Menit"),
            color = "red",
            fill = TRUE,
            icon = icon("globe"))
  })
  
  output$DayMins <- renderInfoBox({
    value <- if (input$Churndf == "0") {
      mean(df$durasi_telpon[df$churn == 0], na.rm = TRUE)
    } else if (input$Churndf == "1") {
      mean(df$durasi_telpon[df$churn == 1], na.rm = TRUE)
    } else {
      mean(df$durasi_telpon, na.rm = TRUE)
    }
    value = round(value,2)
    infoBox(tags$p("Durasi Telepon (Menit)", style = "font-weight:bold;"),
            value = paste(value,"Menit"),
            color = "green",
            fill = TRUE,
            icon = icon("phone-alt"))
  })
  
  
  output$MonthlyCharge <- renderInfoBox({
    value <- if (input$Churndf == "0") {
      mean(df$biaya_bulanan[df$churn == 0], na.rm = TRUE)
    } else if (input$Churndf == "1") {
      mean(df$biaya_bulanan[df$churn == 1], na.rm = TRUE)
    } else {
      mean(df$biaya_bulanan, na.rm = TRUE)
    }
    value = round(value,2)
    infoBox(tags$p("Biaya Bulanan", style = "font-weight:bold;"),
            value = paste("$", value,"/Month"),
            color = "yellow",
            fill = TRUE,
            icon = icon("money-check-alt"))
  })
  
  output$BAR1DF <- renderPlot({

    
    if (input$Churndf == "0") {
      lang_data <- lang0
    } else if (input$Churndf == "1") {
      lang_data <-lang1
    } else {
      lang_data <- lang
    }
    
    proportions <- prop.table(table(lang_data))
    
    pie_data <- data.frame(lang_data = factor(names(proportions)),
                           proportion = as.numeric(proportions))
    
    ggplot(pie_data, aes(x = "", y = proportion, fill = lang_data)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) + 
      theme_minimal() +
      labs(x = NULL, y = NULL, fill = "Status Berlangganan Data") +
      scale_fill_manual(values = c("#6495ed", "#E71D36"), 
                        labels = c("Tidak Berlangganan", "Berlangganan")) +
      geom_text(aes(label = scales::percent(proportion)), 
                position = position_stack(vjust = 0.5)) + 
      guides(fill = guide_legend(title = "Status Belangganan Data"))
  })
  
  
  output$BAR2DF <- renderPlot({
    value <- if (input$Churndf == "0") {
      df[df$churn == 0, ]
    } else if (input$Churndf == "1") {
      df[df$churn == 1, ]
    } else {
      df
    }
    
    ggplot(value, aes(x = factor(telpon_cs))) +  # Convert perbarui to factor
      geom_bar(fill = "#007bb8") +
      theme_minimal() +
      labs(x = "Jumlah Menghubungi CS", y = "Jumlah Pelanggan")
  })
  
  
  output$HISTDF <- renderPlot({
    value <- if (input$Churndf == "0") {
      df[df$churn == 0, ]
    } else if (input$Churndf == "1") {
      df[df$churn == 1, ]
    } else {
      df
    }
    
    ggplot(value) +
      aes(x = denda) +
      geom_histogram(bins = input$num, fill = "#007bb8") +
      theme_minimal() +
      labs(x = "Denda (Dollar)", y = "Jumlah Pelanggan") +
      theme(axis.text = element_text(size = 20))
  })
  
  output$correlation_table <- renderDataTable({
    df <- if (input$Churndf == "0") {
      df[df$churn == 0, ]
    } else if (input$Churndf == "1") {
      df[df$churn == 1, ]
    } else {
      df
    }
    selected_vars <- unique(c(input$selval1, input$selval2))
    filtered_df <- df %>%
      select(all_of(selected_vars))
    
    # Ensure only numeric columns are selected
    numeric_cols <- sapply(filtered_df, is.numeric)
    filtered_df <- filtered_df[, numeric_cols]
    
    corr_matrix <- cor(filtered_df, use = "complete.obs")
    corr_matrix_rounded <- round(corr_matrix, digits = 3)
    datatable(corr_matrix_rounded, options = list(scrollX = TRUE))
  })
  
  
  
  output$violin <- renderPlot({
    df_filtered <- if (input$Churndf == "0") {
      df[df$churn == 0, ]
    } else if (input$Churndf == "1") {
      df[df$churn == 1, ]
    } else {
      df
    }
    ggplot(df_filtered) +
      aes(x = as.factor(churn), y = !!sym(input$selvio), fill = as.factor(churn)) +  # Ensure dynamic column reference and fill
      geom_violin() +
      scale_fill_manual(values = c("#6495ed", "#E71D36")) +  # Add specified colors
      labs(x = "Churn Status", y = input$selvio, title = "Violin Plot by Churn Status") +
      theme_minimal()
  })
  
  ### PEMODELAN
  
  ## REGRESI
  x1 <- df$lama_langganan
  x2 <- df$langganan_data
  x3 <- df$durasi_telpon
  x4 <- df$banyak_telpon
  x5 <- df$denda
  x6 <- df$durasi_min
  y <- df$biaya_bulanan
  model <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6)
  koefisien <- coef(model)
  
  observeEvent(input$predict_btn, {
    output$prediksiBox <- renderUI({
      num1 <- input$num1
      num2 <- input$num2
      num3 <- input$num3
      num4 <- input$num4
      num5 <- input$num5
      num6 <- input$num6
      prediksi <- koefisien[1] + koefisien[2] * num1 + koefisien[3] * num2 + koefisien[4] * num3 + koefisien[5] * num4 + koefisien[6] * num5 + koefisien[7] * num6
      box(width = 12, title = "Hasil Prediksi Biaya Bulanan", status = "primary", solidHeader = TRUE,
          paste("$", round(prediksi, 2)))
      
    })
    
    output$tbl <- DT::renderDataTable({
      df
    })
    
    output$res = renderPlot({
      par(mfrow = c(2, 2))
      plot(model, which = c(1:3, 5))
    })
    
    output$reglin = renderUI({
      
      withMathJax(
        paste0("\\(\\hat{\\beta} = (X^TX)^-1 X^TY \\)"),
        br(),
        paste0("\\(\\hat{\\beta}_0 = \\) ", round(model$coef[[1]], 3)),
        br(),
        paste0("\\(\\hat{\\beta}_1 = \\)", round(model$coef[[2]], 3)),
        br(),
        paste0("\\(\\hat{\\beta}_2 = \\) ", round(model$coef[[3]], 3)),
        br(),
        paste0("\\(\\hat{\\beta}_3 = \\) ", round(model$coef[[4]], 3)),
        br(),
        paste0("\\(\\hat{\\beta}_4 = \\)", round(model$coef[[5]], 3)),
        br(),
        paste0("\\(\\hat{\\beta}_5 = \\)", round(model$coef[[6]], 3)),
        br(),
        paste0("\\(\\hat{\\beta}_6 = \\)", round(model$coef[[7]], 3)),
        br(),
        paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x1 + \\hat{\\beta}_2 x2 + \\hat{\\beta}_3 x3 + \\hat{\\beta}_4 x4 =\\ + \\hat{\\beta}_5 x5 =\\ + \\hat{\\beta}_6 x6 =\\) ", 
               round(model$coef[[1]], 3), " + ", 
               round(model$coef[[2]], 3), "\\( x1 \\)", " + ", 
               round(model$coef[[3]], 3), "\\( x2 \\)", " + ", 
               round(model$coef[[4]], 3), "\\( x3 \\)", " + ", 
               round(model$coef[[5]], 3), "\\( x4 \\)", " + ",
               round(model$coef[[6]], 3), "\\( x5 \\)", " + ",
               round(model$coef[[7]], 3), "\\( x6 \\)", " + ")
      )
    })
    
    output$summary = renderPrint({
      summary(model)
    })
    
  })
  
  output$structure = renderPrint(
    str(df)
  )
  output$sumari = renderPrint(
    summary(df)
  )
  output$dfb = renderDataTable(
    df,extensions = 'Buttons',options=list(dom='Bfrtip',buttons=list('copy','pdf','excel','csv','print'))
  )
  
  
  ## KLASIFIKASI
  
  df$churn <- as.factor(df$churn)
  set.seed(123)
  trainIndex <- createDataPartition(df$churn, p = .8, list = FALSE, times = 1)
  df=df[,-4]
  dfTrain <- df[trainIndex,]
  dfTest  <- df[-trainIndex,]
  dtrain <- xgb.DMatrix(data = as.matrix(dfTrain[, -which(names(dfTrain) == "churn")]), label = as.numeric(dfTrain$churn) - 1)
  
  params <- list(
    objective = "binary:logistic",
    eval_metric = "error"
  )
  model_xgboost <- xgboost(data = dtrain, params = params, nrounds = 100, verbose = 0)
  
  pred_xgboost <- predict(model_xgboost, xgb.DMatrix(data = as.matrix(dfTest[, -which(names(dfTest) == "churn")])))
  pred_xgboost_class <- ifelse(pred_xgboost > 0.5, "1", "0")
  conf_matrix_xgboost <- confusionMatrix(as.factor(pred_xgboost_class), dfTest$churn)
  accuracy_xgboost <- conf_matrix_xgboost$overall['Accuracy']
  
  
  observeEvent(input$predict_btn_klf, {
    
    new_data <- data.frame(
      lama_langganan = as.numeric(input$lama_langganan),
      perbarui = as.numeric(input$perbarui),
      penggunaan_data = as.numeric(input$penggunaan_data),
      telpon_cs = as.numeric(input$telpon_cs),
      durasi_telpon = as.numeric(input$durasi_telpon),
      banyak_telpon = as.numeric(input$banyak_telpon),
      biaya_bulanan = as.numeric(input$biaya_bulanan),
      denda = as.numeric(input$denda),
      durasi_min = as.numeric(input$durasi_min)
    )
    
    colnames(new_data) <- colnames(dfTrain[, -which(names(dfTrain) == "churn")])
    
    dnew <- xgb.DMatrix(data = as.matrix(new_data))
    prob_churn <- predict(model_xgboost, dnew)
    
    output$probability_pie <- renderPlot({
      df_pie <- data.frame(
        Category = c("Churn", "No-Churn"),
        Probability = c(prob_churn, 1 - prob_churn)
      )
      ggplot(df_pie, aes(x = "", y = Probability, fill = Category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        geom_text(aes(label = scales::percent(Probability)), position = position_stack(vjust = 0.5)) +
        labs(title = "Probabilitas Churn") +
        scale_fill_manual(values = c("#FF9999", "#66B2FF"))
    })
    
    output$interpretation <- renderText({
      paste("Interpretasi: Pelanggan berpotensi untuk churn sebesar", 
            round(prob_churn * 100, 2), "% dan tidak churn sebesar", 
            round((1 - prob_churn) * 100, 2), "%.")
    })
    
    output$model_summary <- renderPrint({
      summary(model_xgboost)
    })
    
    output$accuracy_output <- renderText({
      paste("Akurasi Model:", round(accuracy_xgboost, 4))
    })
    
    output$confusion_matrix_plot <- renderPlot({
      fourfoldplot(conf_matrix_xgboost$table, color = c("#CC6666", "#99CC99"), conf.level = 0, margin = 1, main = "Confusion Matrix")
    })
  })
}


shinyApp(ui, server)


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
                )

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

  })
}


shinyApp(ui, server)


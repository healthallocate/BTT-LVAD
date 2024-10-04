# Load required packages


library(shiny)
library(survival)
library(dplyr)
library(ggplot2)

# Load data
load("hr_data.Rdata")

model = coxph(Surv(t_start, t_stop, outcome) ~ durable_LVAD + transplant_under_90 + transplant_under_365 + transplant_over_365 + durable_LVAD*transplant_under_90 + durable_LVAD*transplant_under_365 + durable_LVAD*transplant_over_365 + durable_LVAD*(age_at_listing + diabetes + dialysis + creatinine) + transplant_over_365*(age_at_listing + diabetes + dialysis + creatinine) + transplant_under_90*(age_at_listing + diabetes + dialysis + creatinine) + transplant_under_365*(age_at_listing + diabetes + dialysis + creatinine) , data = data)



# Define UI
ui <- fluidPage(
  titlePanel("Survival Curve Before and After Transplant"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Age at Listing", value = 50, min = 0, max = 100),
      numericInput("transplant_time", "Time to Transplant (days)", value = 90, min = 0, max = 3650),
      numericInput("lvad_time", "Time to Transplant (days) - BTT_LVAD", value = 365, min = 0, max = 3650),
      numericInput("creatinine", "Creatinine", value = 1),
      checkboxInput("diabetes", "Diabetes", value = FALSE),
      checkboxInput("dialysis", "Dialysis", value = FALSE)
    ),
    mainPanel(
      plotOutput("survPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to simulate the survival curve before and after transplant
  simulated_survival <- reactive({
    # Get inputs from UI
    age <- input$age
    diabetes <- input$diabetes
    dialysis <- input$dialysis
    transplant_time <- input$transplant_time
    lvad_time <- input$lvad_time
    creatinine <- input$creatinine

    
    # Define data for the patient before transplant
    no_lvad_data <- data.frame(
      durable_LVAD = 0,
      transplant = 0,
      age_at_listing = age,
      diabetes = diabetes,
      dialysis = dialysis,
      creatinine = creatinine,
      transplant_under_90 = 0,
      transplant_under_365 = 0,
      transplant_over_365 = 0,
      CAN_LISTING_CTR_CD = "OHOU"
    )
    
    no_lvad_tx_data <- data.frame(
      durable_LVAD = 0,
      transplant = 1,
      age_at_listing = age,
      diabetes = diabetes,
      dialysis = dialysis,
      creatinine = creatinine,
      transplant_under_90 = c(1, 0, 0),
      transplant_under_365 = c(0, 1, 0),
      transplant_over_365 = c(0, 0, 1),
      CAN_LISTING_CTR_CD = "OHOU"
    )
    
    lvad_data <- data.frame(
      durable_LVAD = 1,
      transplant = 0,
      age_at_listing = age,
      diabetes = diabetes,
      dialysis = dialysis,
      creatinine = creatinine,
      transplant_under_90 = 0,
      transplant_under_365 = 0,
      transplant_over_365 = 0, 
      CAN_LISTING_CTR_CD = "OHOU"
    )
    
    lvad_tx_data <- data.frame(
      durable_LVAD = 1,
      transplant = 1,
      age_at_listing = age,
      diabetes = diabetes,
      dialysis = dialysis,
      creatinine = creatinine,
      transplant_under_90 = c(1, 0, 0),
      transplant_under_365 = c(0, 1, 0),
      transplant_over_365 = c(0, 0, 1),
      CAN_LISTING_CTR_CD = "OHOU"
    )
    
    # Get survival probabilities for pre-transplant and post-transplant periods
    no_lvad_fit <- survfit(model, newdata = no_lvad_data)
    no_lvad_tx_fit <- survfit(model, newdata = no_lvad_tx_data)
    lvad_fit <- survfit(model, newdata = lvad_data)
    lvad_tx_fit <- survfit(model, newdata = lvad_tx_data)
    
    
    # Convert survival fits to data frames
    # Convert survival fits to data frames
    no_lvad_df <- data.frame(time = no_lvad_fit$time, surv = no_lvad_fit$surv, device = "No LVAD, tx")
    no_lvad_tx_df <- data.frame(time = no_lvad_tx_fit$time, surv = c(no_lvad_tx_fit$surv[1:90,1], no_lvad_tx_fit$surv[1:275,2], no_lvad_tx_fit$surv[1:2635, 3]), device = "No LVAD, tx")
    lvad_df <- data.frame(time = lvad_fit$time, surv = lvad_fit$surv, device = "LVAD, tx")
    lvad_tx_df <- data.frame(time = lvad_tx_fit$time, surv = c(lvad_tx_fit$surv[1:90,1], lvad_tx_fit$surv[1:275,2], lvad_tx_fit$surv[1:2635, 3]), device = "LVAD, tx")
    
    no_lvad_tx_df = no_lvad_tx_df %>% mutate(
      surv = case_when(
        time > 90 & time <= 365  ~ surv[time == 90] + surv - 1,
        time > 365 ~ surv[time == 90] + surv[time == 365] + surv - 2,
        TRUE ~ surv
      )
    )
    
    lvad_tx_df = lvad_tx_df %>% mutate(
      surv = case_when(
        time > 90 & time <= 365  ~ surv[time == 90] + surv - 1,
        time > 365 ~ surv[time == 90] + surv[time == 365] + surv - 2,
        TRUE ~ surv
      )
    )
    
    
    #filter based on time to transplant
    no_lvad_no_tx_data = no_lvad_df %>% mutate(
      device = "No LVAD, No tx"
    )
    lvad_no_tx_data = lvad_df %>% mutate(
      device = "LVAD, No tx"
    )
    no_lvad_df = subset(no_lvad_df, time <= transplant_time)
    no_lvad_tx_df = no_lvad_tx_df %>% mutate(
      time = time + transplant_time
    ) %>% filter(time >= transplant_time)
    lvad_df = subset(lvad_df, time <= lvad_time)
    lvad_tx_df = lvad_tx_df %>% mutate(
      time = time + lvad_time
    ) %>% filter(time >= lvad_time)
    
    # Combine the data frames
    combined_df <- bind_rows(no_lvad_no_tx_data, lvad_no_tx_data, no_lvad_df, no_lvad_tx_df, lvad_df, lvad_tx_df)
    
    combined_df = combined_df %>% mutate(
      surv = case_when(
        time > transplant_time & device == "No LVAD, tx" ~ surv[time == max(no_lvad_df$time) & device == "No LVAD, tx"] + surv - 1,
        time > lvad_time & device == "LVAD, tx" ~ surv[time == max(lvad_df$time) & device == "LVAD, tx"] + surv - 1,
        TRUE ~ surv
      )
    )
    
    return(combined_df)
  })
  
  # Render plot
  
  output$survPlot <- renderPlot({

    # Plot survival curve using ggplot2
    ggplot(simulated_survival(), aes(x = time, y = surv, color = device)) +
      geom_line(size = 0.5) +
      theme_minimal() +
      labs(
        title = "Survival Curve Before and After Transplant",
        x = "Time (days)",
        y = "Survival Probability"
      ) +
      scale_color_manual(values = c("No LVAD, tx" = "blue", "LVAD, tx" = "red", "No LVAD, No tx" = "green", "LVAD, No tx" = "orange")) +
      theme(legend.title = element_blank()) + xlim(c(0,3650)) + 
      geom_vline(xintercept = input$transplant_time, linetype = "dashed", color = "blue") +
      geom_vline(xintercept = input$lvad_time, linetype = "dashed", color = "red")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

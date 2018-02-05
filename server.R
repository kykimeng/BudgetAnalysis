all_trans <- readRDS("all_transactions.RDS")
budget <- read_rds("budget.RDS")

shinyServer(function(input, output, session) {
  observe({
    updateSelectInput(session, 'cat', 
                      choices = all_trans %>% arrange(NewCat) %>% select(NewCat) %>% 
                        unique() %>% pull(NewCat), 
                      selected = "Restaurants")
  })
  
  observe({
    updateSelectInput(session, 'mon', 
                      choices = all_trans %>% arrange(Month) %>% select(Month) %>% 
                        unique() %>% pull(Month))
  })
  
  curr_dat <- reactive({
    req(input$cat)
    curr_dat <- all_trans %>% filter(NewCat %in% input$cat)
    return(curr_dat)
  })
  
  plot_dat <- reactive({
    req(input$mode == "Total")
    all_cats <- all_trans %>% group_by(Month) %>% summarise(Total = sum(Amount))
    plot_dat <- curr_dat() %>% group_by(Month) %>% summarise(`Amount Spent` = sum(Amount))
    full_join(plot_dat, all_cats, by = "Month") %>%
      mutate_if(is.numeric, function(x) if_else(is.na(x), 0, x))
  })
  
  output$p_mon <- renderPlot({
    if (input$mode == "Total") {
      plot_dat <- plot_dat()
      curr_budget <- budget %>% filter(Category %in% input$cat) %>% summarise(Budget = sum(Budget)) %>% pull(Budget)
      plot_dat %>% 
        mutate(pct = `Amount Spent` / Total) %>% 
        ggplot(aes(x = Month)) + 
        geom_bar(aes(y = `Amount Spent`, fill = `Amount Spent` < curr_budget), stat = 'identity') +
        geom_label(aes(y = `Amount Spent`, label = paste0("$", prettyNum(round(`Amount Spent`, 0), big.mark = ",")))) + 
        geom_hline(yintercept = curr_budget) +
        geom_label(aes(x = Month[1] - 30, y = curr_budget, label = curr_budget)) +
        geom_line(aes(y = pct*max(plot_dat$`Amount Spent`)), color = "#0053A0") +
        geom_point(aes(y = pct*max(plot_dat$`Amount Spent`)), color = "#0053A0", size = 3) +
        scale_y_continuous(sec.axis = sec_axis(~.*100/max(plot_dat$`Amount Spent`), name = "Percent of Monthly Total")) +
        labs(x = "", y = "Total Spending",
             title = paste0("2017 Total Spending = ", 
                            prettyNum(round(sum(plot_dat$`Amount Spent`), 0), big.mark = ','), 
                            "; Average Monthly Spending = ", 
                            prettyNum(round(mean(plot_dat$`Amount Spent`), 0), big.mark = ','))) +
        scale_fill_manual(values = c("#ED1E2B", "#008144"), breaks = c(TRUE, FALSE)) + 
        theme(legend.position = "none")
    } else {
      plot_dat <- curr_dat() %>% group_by(Month) %>% 
        summarise(Amount = mean(Amount), N = n()) 
      plot_dat %>% 
        ggplot(aes(x = Month)) + 
        geom_bar(aes(y = Amount), fill = "#0053A0", stat = 'identity') +
        geom_label(aes(y = Amount, label = paste0("$", prettyNum(round(Amount, 1), big.mark = ",")))) +
        geom_label(aes(y = 1, label = N)) +
        geom_hline(yintercept = mean(plot_dat$Amount), linetype = 'dashed') +
        labs(x = "", y = "Average Spending", 
             title = paste0("Overall Average = ", round(curr_dat() %>% 
                                                          summarise(avg = sum(Amount) / n()) %>% 
                                                          pull(avg), 1), 
                            ", Number of Transactions = ", curr_dat() %>% summarise(N = n()) %>% 
                              pull(N)))
    }
  })
  
  output$all <- renderDataTable({
    req(curr_dat())
    return(curr_dat() %>% select(Date, Description, Amount, Category = NewCat))
  })
  
  output$overspend_cat <- renderValueBox({
    req(input$cat)
    curr_budget <- budget %>% filter(Category %in% input$cat) %>% 
      summarise(Budget = sum(Budget)) %>% pull(Budget)
    curr_exp <- mean(plot_dat()$`Amount Spent`)
    diff_spending <- curr_budget - curr_exp
    if (overspending < 0) {
      valueBox(
        paste0("$", prettyNum(round(diff_spending), big.mark = ",")),
        subtitle = "Average Monthly Over-spending",
        color = "red", 
        width = 3,
        icon = icon("money")
      )
    } else {
      valueBox(
        paste0("$", prettyNum(round(diff_spending), big.mark = ",")),
        subtitle = "Average Monthly Under-spending",
        color = "green", 
        width = 3,
        icon = icon("money")
      )
    }
  })
  
  curr_dat_mon <- reactive({
    req(input$mon) 
    all_trans %>% 
      filter(Month == as.Date(input$mon)) %>% 
      group_by(Category = NewCat) %>% 
      summarise(Amount = sum(Amount)) %>% 
      full_join(budget, by = "Category") %>% 
      mutate_if(is.numeric, function(x) if_else(is.na(x), 0, x))  %>% 
      mutate(pct = round(Amount*100 / sum(Amount), 2), 
             Category2 = sprintf('%s: %s%%', Category, pct))
  })
  
  output$p_by_mon <- renderPlot({
    curr_dat <- curr_dat_mon()
    curr_dat %>% 
      ggplot(aes(x = Category2)) +
      geom_bar(aes(y = Amount, fill = Category2), stat = 'identity') +
      geom_label(aes(y = Amount, label = paste0("$", prettyNum(round(Amount, 0), big.mark = ",")))) +
      geom_point(aes(y = Budget), color = "#0053A0", size = 3) +
      labs(x = "") +
      scale_x_discrete(breaks = curr_dat$Category2, labels = curr_dat$Category) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
      ggtitle(paste0("Total Spending This Month = ", 
                     prettyNum(round(sum(curr_dat$Amount), 0), big.mark = ","),
                     "; Total Budget = ", prettyNum(round(sum(budget$Budget), 0), big.mark = ",")))
  })
  
  output$overspend_mon <- renderValueBox({
    curr_dat <- curr_dat_mon()
    tot_spending <- sum(curr_dat$Amount)
    tot_budget <- sum(budget$Budget)
    diff_spending <- tot_budget - tot_spending
    if (diff_spending < 0) {
      valueBox(
        paste0("$", prettyNum(round(diff_spending), big.mark = ",")),
        subtitle = paste0(format(as.Date(input$mon), "%b %Y"), " Overspending"),
        icon = icon("money"),
        color = 'red', 
        width = 3
      )
    } else {
      valueBox(
        paste0("$", prettyNum(round(diff_spending), big.mark = ",")),
        subtitle = paste0(format(as.Date(input$mon), "%b %Y"), " Underspending"),
        icon = icon("money"),
        color = 'green', 
        width = 3
      )
    }
  })
  
  output$all_mon <- renderDataTable({
    req(input$mon) 
    all_trans %>% filter(Month == as.Date(input$mon)) %>% select(Date, Description, Amount, Category = NewCat)
  })
})
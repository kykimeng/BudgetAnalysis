# all_trans <- readRDS("all_transactions.RDS") %>% filter(!grepl("JHU STDNT ACCT SELF SRV", Description) & WorkTrans == 0)
budget <- readRDS("budget.RDS")

shinyServer(function(input, output, session) {
  observe({
    updateSelectInput(session, 'cat', 
                      choices = daily_spending %>% 
                        filter(!Category %in% c("Car Loan", "Student Loan", "Saving", "Brokerage", "Income")) %>% 
                        arrange(Category) %>% select(Category) %>% 
                        unique() %>% pull(Category), 
                      selected = "Restaurants")
    updateDateRangeInput(session, "dr_filter", min = min(daily_spending$Date), max = max(daily_spending$Date), 
                         start = min(daily_spending$Date), end = max(daily_spending$Date))
    updateCheckboxGroupInput(session, "cat_filter", choices = daily_spending %>% 
                               filter(!Category %in% c("Car Loan", "Student Loan", "Saving", "Brokerage", "Income")) %>% 
                               arrange(Category) %>% select(Category) %>% 
                               unique() %>% pull(Category), 
                             selected = daily_spending %>% 
                               filter(!Category %in% c("Car Loan", "Student Loan", "Saving", "Brokerage", "Income")) %>% 
                               arrange(Category) %>% select(Category) %>% 
                               unique() %>% pull(Category))
    mons <- daily_spending %>% arrange(Month) %>% select(Month) %>% 
      unique() %>% pull(Month)
    updateSelectInput(session, 'mon', choices = mons)
    updateSelectInput(session, 'mon_saving', choices = mons)
    updateSelectInput(session, 'mon_debt', choices = mons)
  })
  
  # by cat ----
  curr_dat <- reactive({
    req(input$cat, input$dr_filter)
    curr_dat <- daily_spending %>% filter(Category %in% input$cat & Date >= input$dr_filter[1] & Date <= input$dr_filter[2])
    return(curr_dat)
  })
  
  plot_dat <- reactive({
    req(input$cat)
    all_cats <- daily_spending %>% 
      filter(Date >= input$dr_filter[1] & Date <= input$dr_filter[2]) %>% 
      group_by(Month) %>% summarise(Total = sum(Amount))
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
    return(curr_dat())
  })
  
  output$overspend_cat <- renderValueBox({
    req(input$cat)
    curr_budget <- budget %>% filter(Category %in% input$cat & !Category %in% c("Car Loan", "Student Loan", "Saving", "Brokerage", "Income")) %>% 
      summarise(Budget = sum(Budget)) %>% pull(Budget)
    curr_exp <- mean(plot_dat()$`Amount Spent`)
    diff_spending <- curr_budget - curr_exp
    if (diff_spending < 0) {
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
    req(input$mon, input$cat_filter) 
    daily_spending %>% 
      filter(Month == as.Date(input$mon) & Category %in% input$cat_filter & !Category %in% c("Income", "Saving", "Student Loan", "Car Loan", "Brokerage")) %>% 
      group_by(Category = Category) %>% 
      summarise(Amount = sum(Amount)) %>% 
      full_join(budget %>% filter(Category %in% input$cat_filter), by = "Category") %>% 
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
                     "; Total Budget = ", prettyNum(round(sum(curr_dat$Budget), 0), big.mark = ",")))
  })
  
  output$overspend_mon <- renderValueBox({
    curr_dat <- curr_dat_mon()
    tot_spending <- sum(curr_dat$Amount)
    tot_budget <- budget %>% 
      filter(!Category %in% c("Car Loan", "Student Loan", "Saving", "Brokerage", "Income")) %>% 
      pull(Budget) %>% sum()
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
    daily_spending %>% filter(Month == as.Date(input$mon) & Category %in% input$cat_filter)
  })
  # saving -----
  curr_dat_saving <- reactive({
    req(input$mon_saving) 
    daily_spending %>% 
      filter(Month == as.Date(input$mon_saving) & Category %in% c("Saving", "Brokerage")) %>% 
      group_by(Category) %>% 
      summarise(Amount = sum(Amount)) %>% 
      full_join(budget %>% filter(Category %in% c("Saving", "Brokerage")), by = "Category") %>% 
      mutate_if(is.numeric, function(x) if_else(is.na(x), 0, x))  %>% 
      mutate(pct = round(Amount*100 / sum(Amount), 2), 
             Category2 = sprintf('%s: %s%%', Category, pct))
  })
  
  output$p_saving <- renderPlot({
    curr_dat <- curr_dat_saving()
    curr_dat %>% 
      ggplot(aes(x = Category2)) +
      geom_bar(aes(y = Amount, fill = Category2), stat = 'identity') +
      geom_label(aes(y = Amount, label = paste0("$", prettyNum(round(Amount, 0), big.mark = ",")))) +
      geom_point(aes(y = Budget), color = "#0053A0", size = 3) +
      labs(x = "") +
      scale_x_discrete(breaks = curr_dat$Category2, labels = curr_dat$Category) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
      ggtitle(paste0("Total Saving This Month = ", 
                     prettyNum(round(sum(curr_dat$Amount), 0), big.mark = ","),
                     "; Total Budget = ", prettyNum(round(sum(curr_dat$Budget), 0), big.mark = ",")))
  })
  
  output$net_saving <- renderValueBox({
    curr_dat <- curr_dat_saving() %>% filter(Category == "Saving")
    tot_saving <- sum(curr_dat$Amount)
    tot_budget <- budget %>% filter(Category %in% c("Saving")) %>% pull(Budget) %>% sum()
    net_saving <- tot_saving - tot_budget
    if (net_saving < 0) {
      valueBox(
        paste0("$", prettyNum(round(net_saving), big.mark = ",")),
        subtitle = paste0(format(as.Date(input$mon_saving), "%b %Y"), " Saving"),
        icon = icon("money"),
        color = 'red', 
        width = 6
      )
    } else {
      valueBox(
        paste0("$", prettyNum(round(net_saving), big.mark = ",")),
        subtitle = paste0(format(as.Date(input$mon_saving), "%b %Y"), " Saving"),
        icon = icon("money"),
        color = 'green', 
        width = 6
      )
    }
  })
  output$net_brokerage <- renderValueBox({
    curr_dat <- curr_dat_saving() %>% filter(Category %in% c("Brokerage"))
    tot_brokerage <- sum(curr_dat$Amount)
    tot_budget <- budget %>% filter(Category %in% c("Brokerage")) %>% pull(Budget) %>% sum()
    net_brokerage <- tot_brokerage - tot_budget
    if (net_brokerage < 0) {
      valueBox(
        paste0("$", prettyNum(round(net_brokerage), big.mark = ",")),
        subtitle = paste0(format(as.Date(input$mon_saving), "%b %Y"), " Brokerage"),
        icon = icon("money"),
        color = 'red', 
        width = 6
      )
    } else {
      valueBox(
        paste0("$", prettyNum(round(net_brokerage), big.mark = ",")),
        subtitle = paste0(format(as.Date(input$mon_saving), "%b %Y"), " Brokerage"),
        icon = icon("money"),
        color = 'green', 
        width = 6
      )
    }
  })
  
  output$all_saving <- renderDataTable({
    req(input$mon_saving) 
    daily_spending %>% filter(Month == as.Date(input$mon_saving) & Category %in% c("Saving", "Brokerage"))
  })
  # debt ----
  curr_dat_debt <- reactive({
    req(input$mon_debt) 
    daily_spending %>% 
      filter(Month == as.Date(input$mon_debt) & Category %in% c("Student Loan", "Car Loan")) %>% 
      group_by(Category) %>% 
      summarise(Amount = sum(Amount)) %>% 
      full_join(budget %>% filter(Category %in% c("Student Loan", "Car Loan")), by = "Category") %>% 
      mutate_if(is.numeric, function(x) if_else(is.na(x), 0, x))  %>% 
      mutate(pct = round(Amount*100 / sum(Amount), 2), 
             Category2 = sprintf('%s: %s%%', Category, pct))
  })
  
  output$p_debt <- renderPlot({
    curr_dat <- curr_dat_debt()
    curr_dat %>% 
      ggplot(aes(x = Category2)) +
      geom_bar(aes(y = Amount, fill = Category2), stat = 'identity') +
      geom_label(aes(y = Amount, label = paste0("$", prettyNum(round(Amount, 0), big.mark = ",")))) +
      geom_point(aes(y = Budget), color = "#0053A0", size = 3) +
      labs(x = "") +
      scale_x_discrete(breaks = curr_dat$Category2, labels = curr_dat$Category) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
      ggtitle(paste0("Total Paid This Month = ", 
                     prettyNum(round(sum(curr_dat$Amount), 0), big.mark = ","),
                     "; Total Budget = ", prettyNum(round(sum(curr_dat$Budget), 0), big.mark = ",")))
  })
  
  output$net_debt <- renderValueBox({
    curr_dat <- curr_dat_debt()
    tot_debt <- sum(curr_dat$Amount)
    tot_budget <- budget %>% filter(Category %in% c("Student Loan", "Car Loan")) %>% pull(Budget) %>% sum()
    net_debt <- tot_debt - tot_budget
    if (net_debt < 0) {
      valueBox(
        paste0("$", prettyNum(round(net_debt), big.mark = ",")),
        subtitle = paste0(format(as.Date(input$mon_debt), "%b %Y"), " Underpaid"),
        icon = icon("money"),
        color = 'red', 
        width = 6
      )
    } else {
      valueBox(
        paste0("$", prettyNum(round(net_debt), big.mark = ",")),
        subtitle = paste0(format(as.Date(input$mon_debt), "%b %Y"), " Overpaid"),
        icon = icon("money"),
        color = 'green', 
        width = 6
      )
    }
  })
  
  output$all_debt <- renderDataTable({
    req(input$mon_debt) 
    daily_spending %>% filter(Month == as.Date(input$mon_debt) & Category %in% c("Student Loan", "Car Loan"))
  })
})
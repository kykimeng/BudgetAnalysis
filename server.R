# change percentage and number of obs to line chart with a secondary axis
shinyServer(function(input, output, session) {
  all_trans <- readRDS("all_transactions.RDS")
  
  observe({
    updateSelectInput(session, 'cat', choices = all_trans[, sort(unique(kim_desc))], 
                      selected = "Restaurants")
  })
  
  observe({
    updateSelectInput(session, 'mon', choices = all_trans[, sort(unique(Month))])
  })
  
  curr_dat <- reactive({
    req(input$cat)
    curr_dat <- all_trans[kim_desc %in% input$cat]
    return(curr_dat)
  })
  
  output$year_total <- renderTable({
    req(curr_dat())
    curr_dat()[, .(`Amount Spent` = sum(Amount)), keyby = .(Year = as.character(Year))]
  })
  
  output$p_mon <- renderPlot({
    req(curr_dat())
    all_cats <- all_trans[, .(Total = sum(Amount)), Month]
    plot_dat <- curr_dat()[, .(`Amount Spent` = sum(Amount)), keyby = Month]
    plot_dat <- all_cats[plot_dat, on = .(Month)]
    plot_dat[, pct := `Amount Spent` / Total]
    ggplot(data = plot_dat, aes(x = Month)) +
      geom_bar(aes(y = `Amount Spent`), fill = "#0053A0", stat = 'identity') +
      geom_label(aes(y = `Amount Spent`, label = paste0("$", prettyNum(round(`Amount Spent`, 0), big.mark = ",")))) + 
      geom_hline(yintercept = plot_dat[, mean(`Amount Spent`)], linetype = "dashed") +
      geom_line(aes(y = pct*max(plot_dat$`Amount Spent`)), color = "#008144") +
      geom_point(aes(y = pct*max(plot_dat$`Amount Spent`)), color = "#008144") +
      scale_y_continuous(sec.axis = sec_axis(~.*100/max(plot_dat$`Amount Spent`), name = "Percent of Monthly Total")) +
      # geom_label(aes(x = Month, y = pct, label = scales::percent(pct))) +
      labs(x = "", y = "Total Spending")
  })
  
  output$p_mon_avg <- renderPlot({
    req(curr_dat())
    plot_dat <- curr_dat()[, .(Amount = sum(Amount) / .N, .N), keyby = Month]
    ggplot(data = plot_dat) +
      geom_bar(aes(x = Month, y = Amount), fill = "#0053A0", stat = 'identity') +
      geom_label(aes(x = Month, y = Amount, label = paste0("$", prettyNum(round(Amount, 1), big.mark = ",")))) +
      geom_label(aes(x = Month, y = 1, label = N)) +
      geom_hline(yintercept = plot_dat[, mean(Amount)], linetype = 'dashed') +
      labs(x = "", y = "Average Spending")
  })
  
  output$all <- renderDataTable({
    req(curr_dat())
    return(curr_dat()[, .(Date, Description, Amount, Category = kim_desc)])
  })
  
  output$p_by_mon <- renderPlot({
    req(input$mon) 
    curr_dat <- all_trans[Month == as.Date(input$mon), 
                          .(Amount = sum(Amount)), by = .(Category = kim_desc)]
    curr_dat[, pct := round(Amount*100 / sum(Amount), 2)]
    curr_dat[, Category2 := sprintf('%s: %s%%', Category, pct)]
    ggplot(data = curr_dat, aes(x = Category2)) +
      geom_bar(aes(y = Amount, fill = Category2), stat = 'identity') +
      geom_label(aes(y = Amount, label = paste0("$", prettyNum(round(Amount, 0), big.mark = ",")))) +
      labs(x = "") +
      scale_x_discrete(breaks = curr_dat$Category2, labels = curr_dat$Category) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
      ggtitle(paste0("Total Spending This Month = ", 
                     prettyNum(round(sum(curr_dat$Amount), 0), big.mark = ",")))
  })
  
  output$all_mon <- renderDataTable({
    req(input$mon) 
    all_trans[Month == as.Date(input$mon), .(Date, Description, Amount, Category = kim_desc)]
  })
  
  # output$pie <- renderPlot({
  #   req(input$mon)
  #   curr_dat <- all_trans[Month == as.Date(input$mon), 
  #                         .(Amount = sum(Amount)), keyby = .(Category = kim_desc)]
  #   curr_dat[, pct := round(Amount*100 / sum(Amount), 2)]
  #   curr_dat[, Category := sprintf('%s: %s%%', Category, pct)]
  #   ggplot(data = curr_dat) +
  #     geom_bar(aes(x = "", y = Amount, fill = Category), color = 'black',
  #              width = .7, stat = "identity") +
  #     labs(x = "")
  # })
})
#
# This app is intended to be a sandbox for exploring the
# properties of the Gambler's ruin problem
#
# Author: Bob Wooster
# website: https://bobwooster.github.io

# created: 02 Mar 2018
# updated: 29 Aug 2021

# load packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(scales)

# function definition ----------------------------------
# run sample path
run_sample_path <- function(start_amt,
                            target_amt,
                            win_prob = 0.5) {
    # start_amt: gambler's starting fortune
    # target_amt: gambler stop if he hit this target
    # win_prob: probability of winning a game
    
    # initialize fortune vector
    fortune <- numeric()
    fortune[1] <- start_amt
    current_fortune <- start_amt
    # initialize counter
    j <- 1
    
    # play games until gambler hits target or goes broke
    while (current_fortune > 0 & current_fortune < target_amt) {
        j <- j + 1
        outcome <-
            sample(c(-1, 1), 1, prob = c(1 - win_prob, win_prob))
        current_fortune <- current_fortune + outcome
        fortune[j] <- current_fortune
    }
    
    # return tibble
    tibble(play = 0:(j - 1),
           fortune = fortune)
}

# Define UI for application
ui <- fluidPage(
    theme = shinytheme("cosmo"),
    # Application title
    titlePanel("Gambler's ruin simulation"),
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            width = 3,
            tags$h3("Simulation parameters"),
            # target fortune input
            numericInput("target",
                         "Target fortune",
                         value = 50,
                         step = 1),
            # initial fortune input
            numericInput(
                "initial",
                "Starting fortune (should be less than or equal to target fortune)",
                value = 25,
                step = 1
            ),
            # probability of winning input
            numericInput(
                "w_prob",
                "Probability of winning a single play",
                min = 0,
                max = 1,
                value = 0.50,
                step = 0.001
            ),
            hr(),
            tags$h3("Run the application"),
            # run 1 button
            tags$i("Run one simulation"),
            actionButton(
                "run_1",
                "Run 1 simulation",
                icon = icon("step-forward"),
                width = "100%"
            ),
            # run 100 button
            tags$i(
                "Run 100 simulations: this can take a while if you chose a large target fortune"
            ),
            actionButton(
                "run_100",
                "Run 100 simulations",
                icon = icon("forward"),
                width = "100%"
            ),
            tags$i(
                "Reset all previous runs: this is advised if you change any simulation parameters"
            ),
            # reset button
            actionButton(
                "reset",
                "Reset simulation",
                icon = icon("redo-alt"),
                width = "100%"
            ),
            hr(),
            tags$h3("Plot options"),
            # show labels
            checkboxInput(
                "show_labs",
                "Show plot labels?",
                value = TRUE,
                width = "100%"
            ),
            # group outcome
            checkboxInput("group_outcome", "Group by outcome?",
                          value = TRUE)
        ),
        mainPanel(fluidRow(
            column(width = 9,
                   tags$h3("Random walk paths"),
                   # sample paths plot
                   plotOutput("paths_plot")),
            column(
                width = 3,
                tags$h3("Overview"),
                tags$i(
                    "On each play the gambler wins 1 or loses 1, and plays until he either reaches the target fortune or goes broke"
                ),
                hr(),
                tags$h3("Theoretical values"),
                # theoretical probs and # plays
                tableOutput("th_w_prob"),
                tableOutput("th_n_plays")
            )
        ),
        fluidRow(
            column(
                width = 8,
                tags$h3("Number of plays for each random walk"),
                # histogram of number of plays
                plotOutput("runs_hist")
            ),
            column(
                width = 4,
                tags$h3("Summary statistics"),
                # summary table
                tableOutput("run_summary"),
                hr(),
                # download simulation data
                downloadButton("download_sims",
                               "Download simulation data"),
                hr(),
                "Created by",
                tags$a(href = "https://bobwooster.github.io", "Bob Wooster"),
                br(),
                tags$a(href = "https://github.com/bobwooster/gamblers_ruin", "Github repo for this app")
            )
        ))
    )
)

# server function
server <- function(input, output, session) {
    thematic::thematic_shiny()
    
    # initialize simulation info
    vals <- reactiveValues(simul_df = tibble(
        walk = 0,
        play = 0,
        fortune = -1,
        outcome = "none"
    ))
    
    # run 1 simulation
    observeEvent(input$run_1, {
        # run the new game
        new_run <- run_sample_path(start_amt = input$initial,
                                    target_amt = input$target,
                                    win_prob = input$w_prob) %>%
            mutate(walk = max(vals$simul_df$walk) + 1,
                   outcome = if_else(tail(fortune, n = 1) == 0,
                                     "Failure",
                                     "Success"))
        
        # append the new run onto the existing ones
        vals$simul_df <- bind_rows(vals$simul_df, new_run)
    })
    
    # run 100 simulations
    observeEvent(input$run_100, {
        # compute the run numbers for the next hundred games
        recent_walk <- max(vals$simul_df$walk)
        new_walks <- (recent_walk + 1):(recent_walk + 100)
        
        # run 100 new games
        new_sims <- map_dfr(new_walks,
                             ~ run_sample_path(start_amt = input$initial,
                                               target_amt = input$target,
                                               win_prob = input$w_prob) %>%
                             mutate(walk = .x)) %>%
            group_by(walk) %>%
            mutate(outcome = if_else(tail(fortune, n = 1) == 0,
                                     "Failure",
                                     "Success")) %>%
            ungroup()
        
        # append the new results to the existing ones
        vals$simul_df <- bind_rows(vals$simul_df, new_sims)
    })

    # reset simulation info
    observeEvent(input$reset, {
        vals$simul_df <- tibble(
            walk = 0,
            play = 0,
            fortune = -1,
            outcome = "none"
        )
    })
    
    # summary of the simulation data
    run_summary <- reactive({
        vals$simul_df %>%
            filter(outcome != "none") %>%
            group_by(walk) %>%
            summarize(final_play = as.integer(tail(play, n = 1)),
                      final_fortune = as.integer(tail(fortune, n = 1)),
                      outcome = if_else(tail(fortune, n = 1) > 0,
                                        "Success",
                                        "Failure"),
                      .groups = "drop")
    })
    
    # table of run history
    run_table <- reactive({
        df <- run_summary()
        if (input$group_outcome == TRUE) {
            df %>%
                group_by(outcome) %>%
                rename(Outcome = outcome) %>%
                summarize(
                    n = n(),
                    Prop = n / nrow(df),
                    `Mean plays` = scales::comma(as.integer(round(mean(final_play)))),
                    `SD plays` =  scales::comma(as.integer(round(sd(final_play)))),
                    .groups = "drop"
                )
        } else {
            df %>%
                summarize(
                    n = n(),
                    `Win prob` = mean(outcome == "Success"),
                    `Mean plays` = scales::comma(as.integer(round(mean(final_play)))),
                    `SD plays` = scales::comma(as.integer(round(sd(final_play))))
                )
        }
    })
    
    # sample paths  plot
    output$paths_plot <- renderPlot({
        g <- vals$simul_df %>%
            filter(outcome != "none") %>%
            ggplot(
                aes(
                    x = play,
                    y = fortune
                )
            ) +
            geom_hline(yintercept = c(0, isolate(input$target)), col = c("red4", "gray30"))
        
        if(input$group_outcome == TRUE) {
            g <- g +
                geom_line(aes(col = outcome,group = walk)) +
                scale_color_manual(values = c(Failure = "red4",
                                              Success = "gray30")) +
                labs(x = "Plays", y = "Fortune", color = "Outcome")
        } else {
            g <- g +
                geom_line(aes(group = walk), col = "royalblue4") +
                labs(x = "Plays", y = "Fortune")
        }
            
        if(input$show_labs == TRUE) {
            g <- g + geom_label(inherit.aes = FALSE,
                                data = run_summary(),
                                aes(
                                    x = final_play,
                                    y = final_fortune,
                                    label = scales::comma(final_play,
                                                          accuracy = 1)
                                ))
        }
        g <- g +
            scale_x_continuous(labels = comma) +
            scale_y_continuous(labels = comma) +
            theme(axis.text = element_text(size = 14),
                  axis.title = element_text(size = 16),
                  legend.text = element_text(size = 14),
                  legend.title = element_text(size = 14),
                  legend.position = "bottom")
        
        g
    })
    
    # make histogram of runs
    output$runs_hist <- renderPlot(height = 300, {
        df <- run_summary()
        
        if(nrow(df) == 0){
            h <- ggplot() + theme_void()
        } else {
            df <- run_summary()
            
            h <- df %>%
                ggplot(aes(x = final_play))
            if(input$group_outcome == TRUE) {
                h <- h + geom_histogram(aes(fill = outcome),
                                        col = "black",
                                        bins = 30) +
                    facet_grid(cols = vars(outcome)) +
                    scale_fill_manual(values = c(Failure = "red4",
                                                 Success = "gray30")) +
                    guides(fill = "none")
            } else {
                h <- h + geom_histogram(col = "black",
                                        fill = "royalblue4",
                                        bins = 30)
            }
            h <- h +
                labs(x = "Simulation length",
                     y = "Count") +
                scale_x_continuous(labels = comma) +
                theme(strip.text.x = element_text(size = 16),
                      axis.text = element_text(size = 14),
                      axis.title = element_text(size = 16))
        }
        h
    })
    
    # compute theoretical probabilities and expected number of runs
    theory <- reactive({
        a <- input$initial    # starting fortune
        b <- input$target     # target fortune
        p <- input$w_prob   # prob of winning
        q <- 1 - p
        odds <- p/q
        if(p == 0.5) {
            pbroke <- (b - a) / b
            n_plays <- a*(b - a)
        } else if(p != 0){
            pbroke <- (1 - exp((b-a)*log(odds))) / (1 - exp(b*log(odds)))
            n_plays <- (b *(1 - pbroke) - a) / (p - q)
        } else {
            pbroke <- 1
            n_plays <- a
        }
        tibble(prob_broke = pbroke,
               exp_n_plays = n_plays)
    })
    
    # output probability of going broke
    output$th_w_prob <- renderTable(theory() %>%
                                        transmute(`Probability of going broke` = prob_broke),
                                    digits = 3, align = "c")
    # output expected number of plpays
    output$th_n_plays <- renderTable(theory() %>%
                                         transmute(`Expected number of plays` = scales::comma(exp_n_plays)),
                                     digits = 0, align = "c")
    # output summary run table
    output$run_summary <- renderTable(run_table(), digits = 3, align = "c")
    
    # download data
    output$download_sims <- downloadHandler(
        filename = function() {
            paste("gamblers_ruin_sim_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write_csv(vals$simul_df %>%
                          filter(outcome != "none") %>%
                          select(-outcome),
                      file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)


# Packages ----
library(pacman)
p_load(shiny, tidyverse, plotly, 
       DT, shinythemes, shinyFiles, fs)


##### this function take the selected directory and return a formatted dataframe with the cell counting data 
make_df <- function(dirs){
    map_dfr(dirs, function(my_dir){
        
        dir_list <- list.dirs(path = dirs, full.names = T, recursive = F)
        
        map(dir_list, function(my_dir){
            
            file_list <- list.files(my_dir, 
                                    pattern = "[Total|Death].\\.txt$", 
                                    full.names = T, 
                                    recursive = F)
            
            map_dfr(file_list, function(file){
                
                my_dataset <- read_table(file = file)
                my_dataset$Image_no <- seq_along(my_dataset$Count)
                my_dataset$Cell_condition <- str_sub(file, start = -9, end = -5)
                my_dataset$Treatment <- path_split(file) %>%
                    pluck(1) %>% 
                    nth(-2)
                # my_dataset$Date <- str_sub(my_dir, start = 42, end = 47)
                my_dataset$file_Path <- file
                
                my_dataset
            })
            # print(file_list)
            
        })
        
    })
    
}

make_bar_plot <- function(my_df){
    my_df %>% 
        select(-file_Path, Image_no) %>% 
        pivot_wider(names_from = Cell_condition, values_from = Count) %>% 
        mutate(Live = (1 - Death / Total) * 100,
               Death = (Death / Total) * 100) %>% 
        # head
        select(-Total) %>% 
        pivot_longer(cols = Death:Live, values_to = "Porcentage") %>% 
        # mutate(Treatment = parse_number(Treatment)) %>% 
        # glimpse()
        ggplot(mapping = aes(x = Image_no,
                             y = Porcentage, 
                             fill = name)) +
        geom_bar(stat = "identity") +
        geom_hline(yintercept = 50, 
                   linetype = "dotdash",
                   color="purple",
                   alpha = 0.5) + 
        # facet_grid(. ~Treatment,
        #            labeller = label_bquote(cols = "[Ca2+]" == .(Treatment))) +
        facet_grid(. ~Treatment) +
        coord_flip()
}

make_boxplot <- function(my_df){
    # Transform the data
    transformed_df <- my_df %>% 
        filter(Count < 50) %>% 
        select(-file_Path, Image_no) %>% 
        pivot_wider(names_from = Cell_condition, values_from = Count) %>% 
        mutate(Live_cells = (1 - Death / Total) * 100,
               Dead_cells = (Death / Total) * 100) %>% 
        select(-Death, -Total) %>% 
        pivot_longer(cols = Dead_cells:Live_cells, values_to = "Percentage") #%>% 
        # mutate(Treatment = parse_number(Treatment))
    # make the boxplot
    my_plot <- transformed_df %>% 
        ggplot(aes(x = name,
                   y = Percentage)) + 
        geom_boxplot(aes(color = name),
                     outlier.shape = NA,
                     lwd = 1,
                     show.legend = F)  +
        geom_point(shape = 21,
                   size = 1,
                   aes(fill = name),
                   color = "black",
                   alpha = .6,
                   show.legend = F,
                   position = position_jitterdodge(jitter.width = 0,
                                                   seed = 999)) +
        geom_line(aes(group = Image_no),
                  # color = as.factor(Image_no)),
                  alpha = 0.3) + 
        facet_grid(. ~ Treatment)
    
    # ggplotly(my_plot)
    my_plot
}
    
###################################################################################################################
###################################################################################################################
###################################################################################################################

ui <- fluidPage(theme = shinythemes::shinytheme("simplex"),
                titlePanel(h1("Cell Quantification"), 
                           windowTitle = "Cell Quantification App"),
                br(),
                
                sidebarLayout(
                    sidebarPanel(
                        shinyDirButton("dir", "Chose directory", "Upload"),
                        tags$hr(),
                        textInput("note", "Add note or comment"),
                        tags$hr(),
                        radioButtons('format', 'Download report', c('PDF', 'HTML', 'Word'),
                                     inline = TRUE),
                        downloadButton('downloadReport')
                    ),
                    mainPanel(
                        h3("This is your current directory"),
                        verbatimTextOutput("dir"),
                        tags$hr(),
                        # 
                        # h3("This is your data (first 6 rows)"),
                        # tableOutput('table'),
                        # tags$hr(),
                        
                        h3("This is your summary quantification plot"),
                        plotOutput("plot1", click = "plot_click"),
                        verbatimTextOutput("info"),
                        tags$hr(),
                        
                        h3("This is your boxploplot"),
                        plotlyOutput("my_boxplot"),
                        tags$hr()
                    )
                )
)
# 
# 
server <- function(input, output) {
    volumes <- c(Home = fs::path_home(), getVolumes()())
    
    shinyDirChoose(input, 'dir',
                   root=volumes)
    
    dir <- reactive(input$dir)
    output$dir <- renderText({  # use renderText instead of renderPrint
        parseDirPath(volumes, dir())
    })
    
    #
    dataframe <- reactive({parseDirPath(volumes, dir()) %>% 
            make_df})
    
    # output$table <- renderTable({
    #     dataframe() %>% 
    #         head()
    # })
    
    output$plot1 <- renderPlot({
        dataframe() %>%
            make_bar_plot()
    })
    
    output$info <- renderText({
        paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })
    
    output$my_boxplot <- renderPlotly({
        dataframe() %>%
            make_boxplot()
    })
    
    # generate report
    
    
    
    output$downloadReport <- downloadHandler(
        filename = function() {
            paste(paste('CountingCells_report', Sys.Date(), sep = "_"), sep = '.', switch(
                input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
        },
        
        
        
        content = function(file) {
            src <- normalizePath('report.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(df = dataframe(),
                           notes = input$note)
            
            library(rmarkdown)
            out <- render('report.Rmd', switch(
                input$format,
                PDF = pdf_document(), HTML = html_document(), Word = word_document()
            ),
            params = params)
            file.rename(out, file)
        }
    )
    
}

shinyApp(ui, server)
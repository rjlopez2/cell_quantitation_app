# this iscript is used to run the application in file defined in app.r in the background
options(shiny.autoreload = T,
        shiny.trace=T,
        # shiny.port = 6133,
        shiny.launch.browser = T)


shiny::runApp()
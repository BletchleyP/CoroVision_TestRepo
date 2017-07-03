library(shiny)

# Modifikation des Beispieldatensatzes mit deutschen Headern
# ----------------------------------------------------------
viewportDF <- iris
colnames(viewportDF) <- c("Sepal-L\xe4nge", "Sepal-Breite", "Petal-L\xe4nge", "Petal-Breite", "Spezies")

# Modifizierter fileInput
# -----------------------
fileInput2 <- function(inputId, label = NULL, labelIcon = NULL, multiple = FALSE, 
                       accept = NULL, width = NULL, progress = TRUE, ...) {
  # add class fileinput_2 defined in UI to hide the inputTag
  inputTag <- tags$input(id = inputId, name = inputId, type = "file", class = "fileinput_2")
  if (multiple) 
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0) 
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  
  div(..., style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";", "display: inline;"), 
      inputTag,
      # fileInput Widget als action button
      tags$label(`for` = inputId, div(icon(labelIcon), label, class = "btn btn-default action-button")),
      # optionally display a progress bar
      if(progress)
        tags$div(id = paste0(inputId, "_progress"), class = "progress shiny-file-input-progress", tags$div(class = "progress-bar"))
  )
}


# User-Interface-Teil
# -------------------
ui <- fluidPage(
  
  # CSS-Trick um fileInput in fileInput2 zu verstecken
  tags$head(tags$style(HTML(
    ".fileinput_2 {
      width: 0.1px;
      height: 0.1px;
      opacity: 0;
      overflow: hidden;
      position: absolute;
      z-index: -1;
    }
    #fileCounter {
      display: inline;
      color: red;
      margin: 10px;
    }"
  ))),
  
  titlePanel("snippet_dataTables"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Zeilenanzahl:",
                  min = 1,
                  max = 500,
                  value = 30),
      fileInput(inputId = "myFiles", label = "Laden Sie Dateien hoch:", multiple = TRUE, buttonLabel = "Durchsuchen ...",
                placeholder = "Keine Dateien hochgeladen"),
      fileInput2(inputId = "myFiles2", label = "Trainingsdaten laden", labelIcon = "folder-open-o", width= "250px",
                 multiple = TRUE, progress = FALSE),
      textOutput("fileCounter")
    ),
    
    mainPanel(
      dataTableOutput("myTable"),
      verbatimTextOutput("content1"),
      verbatimTextOutput("content2")
    )
  )
)


# Server-Teil
# -----------
server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$myTable <- renderDataTable({viewportDF[1:input$bins,]}, options = list(
    lengthMenu = c(10, 30, 50, 100),
    pageLength = 10,
    language = list("paginate" = list("next" = "Weiter", "previous" = "Zur&uuml;ck"),
                    "search" = "Suche:",
                    "lengthMenu" = "Zeige _MENU_ Eintr&auml;ge",
                    "loadingRecords" = "Daten werden geladen ...",
                    "processing" = "Bitte warten ...",
                    "info" = "Zeige _START_ bis _END_ von _TOTAL_ Eintr&auml;gen",
                    "infoEmpty" = "Zeige 0 bis 0 von 0 Eintr&auml;gen",
                    "infoFiltered" = "(gefiltert aus _MAX_ Eintr&auml;gen insgesamt)")
  ))
  
  output$content1 <- renderPrint({
    inFile <- input$myFiles
    if (is.null(inFile))
      return(NULL)
    paste("File name 1:", inFile$name)
  })
  
  output$content2 <- renderPrint({
    inFile <- input$myFiles2
    if (is.null(inFile))
      return(NULL)
    paste("File name 2:", inFile$name)
    #Test-Kommentar
    # 2. Test-Kommentar
  })
  
  output$fileCounter <- renderText({
    if (is.null(input$myFiles2)) {
      return ("Keine Dateien geladen.")
    } else if (nrow(input$myFiles2)>1) {
      return(paste(" Es wurden ", nrow(input$myFiles2), " Dateien geladen."))
    } else {
      return (" Es wurde 1 Datei geladen.")
    }
  })
}


# Energie!
# --------
shinyApp(ui = ui, server = server)

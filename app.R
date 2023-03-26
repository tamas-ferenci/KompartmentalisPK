library(shiny)
library(ggplot2)
theme_set(theme_bw())
library(data.table)

options(shiny.useragg = TRUE)

sympy <- reticulate::import("sympy")
numpy <- reticulate::import("numpy")
s <- sympy$Symbol("s")
u <- sympy$Symbol("u", positive = TRUE) # t
`%s+%` <- function(x, y) sympy$Add(x, y)
`%s*%` <- function(x, y) sympy$Mul(x, y)
`%s/%` <- function(x, y) x%s*%sympy$Pow(y, -1)
lambdify <- reticulate::py_run_string(
  "import sympy
def lambdify(*args, **kwargs):
  return sympy.lambdify(*args, **kwargs)
")$lambdify

ui <- fluidPage(
  
  tags$head(
    tags$script(type = "text/x-mathjax-config", 
                "MathJax.Hub.Config({
                  'CommonHTML': { linebreaks: { automatic: true } },
                  'HTML-CSS': { linebreaks: { automatic: true } },
                         SVG: { linebreaks: { automatic: true } }
                });"),
    tags$meta(name = "description",
              content = paste0("A kompartmentális farmakokinetikai modellek működését ",
                               "szemléltető alkalmazás. Írta: Ferenci Tamás.")),
    tags$meta(property = "og:title", content = "Kompartmentális farmakokinetika demonstráció"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:locale", content = "hu_HU"),
    tags$meta(property = "og:url", content = "https://research.physcon.uni-obuda.hu/KompartmentalisPK/"),
    tags$meta(property = "og:image",
              content = "https://research.physcon.uni-obuda.hu/KompartmentalisPK_Pelda.png"),
    tags$meta(property = "og:description",
              content = paste0("A kompartmentális farmakokinetikai modellek működését szemléltető ",
                               "alkalmazás. Írta: Ferenci Tamás.")),
    tags$meta(name = "DC.Title", content = "Kompartmentális farmakokinetika demonstráció"),
    tags$meta(name = "DC.Creator", content = "Ferenci Tamás"),
    tags$meta(name = "DC.Subject", content = "farmakokinetika"),
    tags$meta(name = "DC.Description",
              content = paste0("A kompartmentális farmakokinetikai modellek működését szemléltető ",
                               "alkalmazás. Írta: Ferenci Tamás.")),
    tags$meta(name = "DC.Publisher", content = "https://research.physcon.uni-obuda.hu/KompartmentalisPK/"),
    tags$meta(name = "DC.Contributor", content = "Ferenci Tamás"),
    tags$meta(name = "DC.Language", content = "hu_HU"),
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(name = "twitter:title", content = "Kompartmentális farmakokinetika demonstráció"),
    tags$meta(name = "twitter:description",
              content = paste0("A kompartmentális farmakokinetikai modellek működését ",
                               "szemléltető alkalmazás. Írta: Ferenci Tamás.")),
    tags$meta(name = "twitter:image",
              content = "https://research.physcon.uni-obuda.hu/KompartmentalisPK_Pelda.png"),
    tags$style(".btn {margin-bottom:10px}")
  ),
  
  tags$div(id = "fb-root"),
  tags$script(async = NA, defer = NA, crossorigin = "anonymous",
              src = "https://connect.facebook.net/hu_HU/sdk.js#xfbml=1&version=v16.0", nonce = "ZG8gLcyn"),
  
  tags$style(".shiny-file-input-progress {display: none}"),
  
  titlePanel("Kompartmentális farmakokinetika demonstráció"),
  
  p("A weboldal és az elemzések teljes forráskódja ",
    a("itt", href = "https://github.com/tamas-ferenci/KompartmentalisPK", target = "_blank"),
    "érhető el. Írta: ", a("Ferenci Tamás", href = "http://www.medstat.hu/", target = "_blank",
                           .noWS = "outside"), "."),
  
  div(style = "line-height: 13px;",
      div(class = "fb-share-button",
          "data-href" = "https://research.physcon.uni-obuda.hu/KompartmentalisPK/",
          "data-layout" = "button_count", "data-size" = "small",
          a("Megosztás", target = "_blank",
            href = paste0("https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2Fresearch.physcon.",
                          "uni-obuda.hu%2FKompartmentalisPK%2F&amp;src=sdkpreparse"),
            class = "fb-xfbml-parse-ignore")),
      
      a("Tweet", href = "https://twitter.com/share?ref_src=twsrc%5Etfw", class = "twitter-share-button",
        "data-show-count" = "true"),
      includeScript("http://platform.twitter.com/widgets.js", async = NA, charset = "utf-8")),
  
  p(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("modell", "Modell", paste0(1:3, " kompartmentes kinetika")),
      sliderInput("a10", HTML(paste0("a", tags$sub("10"))), 0, 10, 1, 0.1),
      conditionalPanel("input.modell=='2 kompartmentes kinetika'|input.modell=='3 kompartmentes kinetika'",
                       sliderInput("a12", HTML(paste0("a", tags$sub("12"))), 0, 10, 1, 0.1),
                       sliderInput("a21", HTML(paste0("a", tags$sub("21"))), 0, 10, 1, 0.1)),
      conditionalPanel("input.modell=='3 kompartmentes kinetika'",
                       sliderInput("a13", HTML(paste0("a", tags$sub("13"))), 0, 10, 1, 0.1),
                       sliderInput("a31", HTML(paste0("a", tags$sub("31"))), 0, 10, 1, 0.1)),
      sliderInput("d0","Kezdődózis", 1, 100, 10, 1),
      selectInput("dosage", "Dozírozás", c("Nincs", "Állandó", "Szinuszoidális")),
      conditionalPanel("input.dosage=='Állandó'",
                       sliderInput("rate", "Ráta", 0, 10, 1, 0.1)),
      conditionalPanel("input.dosage=='Szinuszoidális'",
                       sliderInput("amplitude", "Amplitúdó", 0, 10, 1, 0.1),
                       sliderInput("period", "Periódusidő", 0, 10, 1, 0.1)),
      checkboxInput("logy", "Függőleges tengely logaritmikus"),
      width = 2
    ),
    
    mainPanel(
      plotOutput("timePlot"),
      hr(),
      fluidRow(
        column(5, DiagrammeR::grVizOutput("compPlot")),
        column(3,
               p("A rendszert leíró mátrix:"), uiOutput("matrixPlot"),
               p("A mátrix sajátértékei:"), uiOutput("lambdaPlot"),
               p("Analitikus megoldás az első kompartmentre:"), uiOutput("solPlot")
        ),
        column(3, plotOutput("eigenPlot"))
      )
    )
  ),
  hr(),
  h4("Írta: Ferenci Tamás (Óbudai Egyetem, Élettani Szabályozások Kutatóközpont), v1.00"),
  
  tags$script(HTML("var sc_project=11601191; 
                      var sc_invisible=1; 
                      var sc_security=\"5a06c22d\";
                      var scJsHost = ((\"https:\" == document.location.protocol) ?
                      \"https://secure.\" : \"http://www.\");
                      document.write(\"<sc\"+\"ript type='text/javascript' src='\" +
                      scJsHost+
                      \"statcounter.com/counter/counter.js'></\"+\"script>\");" ),
              type = "text/javascript")
)

server <- function(input, output) {
  
  Amat <- reactive({
    switch(input$modell,
           "1 kompartmentes kinetika" = matrix(-input$a10, ncol = 1),
           "2 kompartmentes kinetika" = matrix(c(-input$a10-input$a12, input$a12, input$a21, -input$a21),
                                               ncol = 2),
           "3 kompartmentes kinetika" = matrix(c(-input$a10-input$a12-input$a13, input$a12, input$a13,
                                                 input$a21, -input$a21, 0,
                                                 input$a31, 0, -input$a31), ncol = 3))
  })
  
  solution <- reactive({
    A <- sympy$Matrix(Amat())
    compnumber <- A$shape[[1]][1]
    
    if(sympy$Matrix(A)$is_diagonalizable()) {
      matrixexp <- function(s) sympy$exp(A%s*%s)
    } else {
      temp <- sympy$Matrix(A)$jordan_form()
      S <- temp[[1]]
      J <- temp[[2]]
      matrixexp <- function(s) S%s*%sympy$exp(J%s*%s)%s*%S$inv()
    }
    
    resHom <- matrixexp(u)%s*%sympy$Matrix(c(input$d0, rep(list(0), compnumber-1)))
    
    if(input$dosage=="Állandó") {
      resInhom <- sympy$integrate(
        sympy$nfloat(sympy$expand(matrixexp(u%s+%(-1L%s*%s))%s*%sympy$Matrix(
          c(input$rate, rep(list(0), compnumber-1))))), reticulate::tuple(s, 0, u))$doit()
    } else if(input$dosage=="Szinuszoidális") {
      resInhom <- sympy$integrate(sympy$nfloat(sympy$expand(
        matrixexp(u%s+%(-1L%s*%s))%s*%
          sympy$Matrix(c(input$amplitude%s*%(1L%s+%sympy$cos(2L%s*%sympy$pi%s/%input$period%s*%s)),
                         rep(list(0), compnumber-1))))), reticulate::tuple(s, 0, u))$doit()
    } else resInhom <- sympy$Matrix(rep(list(0), compnumber))
    
    resHom <- sympy$expand(resHom)
    resInhom <- sympy$expand(resInhom)
    
    list(resHom = resHom, resInhom = resInhom, res = sympy$expand(resHom%s+%resInhom))
  })
  
  output$timePlot <- renderPlot({
    tvals <- seq(0, 10, 0.01)
    
    compnumber <- ncol(Amat())
    sol <- solution()$res
    
    f <- lambdify(u, sol, "numpy")
    tempgrid <- data.table(t = tvals, matrix(f(array(tvals)), ncol = compnumber, byrow = TRUE))
    colnames(tempgrid)[-1] <- paste0(1:compnumber, ". kompartment")
    tempgrid <- melt(tempgrid, id.vars = "t")
    
    p <- ggplot(tempgrid, aes(x = t, y = value, group = variable, color = variable)) + geom_line() +
      labs(x = "Idő", y = "Dózis", color = "Kompartment") 
    p <- if(input$logy) p + scale_y_log10() + annotation_logticks(sides = "l") else
      p + coord_cartesian(ylim = c(0, NA))
    p
  })
  
  output$eigenPlot <- renderPlot({
    A <- Amat()
    eigenvals <- eigen(A)$values
    limits <- c(-1, 1)*max(abs(eigenvals))
    
    ggplot(data.frame(x = Re(eigenvals), y = Im(eigenvals)), aes(x = x, y = y)) +
      geom_point(shape = 4, color = "red", size = 5, stroke = 2) +
      geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
      coord_cartesian(xlim = limits, ylim = limits) +
      labs(x = "Valós rész", y = "Képzetes rész", title = "Sajátértékek")
  })
  
  output$matrixPlot <- renderUI({
    A <- Amat()
    withMathJax(paste0("$$", sympy$latex(sympy$Matrix(A)), "$$"))
  })
  
  output$lambdaPlot <- renderUI({
    A <- Amat()
    compnumber <- ncol(A)
    lambdas <- eigen(A)$values
    
    withMathJax(paste0("$$", paste0(sapply(1:length(lambdas), function(i)
      paste0("\\lambda_", i, "=", round(lambdas[i], 2))), collapse = ","), "$$"))
  })
  
  output$compPlot <- DiagrammeR::renderGrViz({
    compnumber <- ncol(Amat())
    
    switch(compnumber,
           "1" = DiagrammeR::grViz("digraph {
                    graph [layout = neato]
                    node [shape = rectangle]
                    
                    Comp1 [pos = '0,0!', label = '1. kompartment']
                    Out [pos = '0,-2!', style = invis]
                    
                    Comp1 -> Out [label = <a<SUB>10</SUB> >]
                  }"),
           "2" = DiagrammeR::grViz("digraph {
                    graph [layout = neato]
                    node [shape = rectangle]
                    
                    Comp1 [pos = '0,0!', label = '1. kompartment']
                    Comp2 [pos = '3,0!', label = '2. kompartment']
                    Out [pos = '0,-2!', style = invis]
                    
                    Comp1 -> Comp2 [headlabel = <a<SUB>12</SUB>>, labeldistance = 3]
                    Comp2 -> Comp1 [headlabel = <a<SUB>21</SUB>>, labeldistance = 2]
                    Comp1 -> Out [label = <a<SUB>10</SUB> >]
                  }"),
           "3" =  DiagrammeR::grViz("digraph {
                    graph [layout = neato]
                    node [shape = rectangle]
                    
                    Comp1 [pos = '0,0!', label = '1. kompartment']
                    Comp2 [pos = '2,0!', label = '2. kompartment']
                    Comp3 [pos = '-2,0!', label = '3. kompartment']
                    Out [pos = '0,-2!', style = invis]
                    
                    Comp1 -> Comp2 [headlabel = <a<SUB>12</SUB>>, labeldistance = 2]
                    Comp2 -> Comp1 [headlabel = <a<SUB>21</SUB>>, labeldistance = 3]
                    Comp1 -> Comp3 [headlabel = <a<SUB>13</SUB>>, labeldistance = 3]
                    Comp3 -> Comp1 [headlabel = <a<SUB>31</SUB>>, labeldistance = 2]
                    Comp1 -> Out [label = <a<SUB>10</SUB> >]
                  }"))
  })
  
  output$solPlot <- renderUI({
    sol <- solution()
    withMathJax(paste0("$$\\underbrace{", sympy$latex(sympy$nfloat(sol$resHom[0]$subs(u, "t"), 3)),
                       "}_{\\text{Homogén általános mo. (tranziens)}} + \\underbrace{",
                       sympy$latex(sympy$nfloat(sol$resInhom[0]$subs(u, "t"), 3)),
                       "}_{\\text{Inhomogén partikuláris mo. (steady-state)}} = ",
                       sympy$latex(sympy$nfloat(sol$res[0]$subs(u, "t"), 3)), "$$"))
  })
  
}

shinyApp(ui = ui, server = server)

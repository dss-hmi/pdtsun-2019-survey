library(shiny)
library(psych)
library(corrgram)
library(plotrix)
library(sem)
library(GPArotation)

{shinyUI( # shinyUI - defines/describes User Interface
  {pageWithSidebar(
    {headerPanel("Shiny Exploratory Factor Analysis")},# HEADER
      {sidebarPanel(
        selectInput("dataset", "Choose a dataset:", # selectInput - "dataset" - choose dataset
                      choices = c(

                        "New_reversed",
                        "New_raw",
                        "Old_reversed",
                        "Old_raw"
                       # "Cognitive Abilities"
                        # "Emotional Traits",
                        # "Physical Measures",
                        # "Thurstone"
#                                 ,"Uploaded"
                        )) # selectInput
          , numericInput("k", label="Retain k factors:", value=7) # numericInput - "k" - number of retained factors
          , radioButtons("rotation", "Choose the rotation Method", # radioButtons - "rotation" - select rotation method
                       list(#"SVD eigenvectors"="svd",
                         "Unrotated"="none",
                         # "Target (T)"="targetT",
                         # "Target (Q)"="targetQ",
                         "Varimax (T)"="Varimax",
                         "Quartimax (T)" = "quartimax",
                         "Quartimin (Q)" = "quartimin",
                         "Geomin (T)" = "geominT",
                         "Geomin (Q)" = "geominQ",
                         "Oblimin (Q)"   ="oblimin",
                         "Bifactor (T)"="bifactorT",
                         "Bifactor (Q)"="bifactorQ",
                         "Crawford-Ferguson (T)"="cfT",
                         "Crawford-Ferguson (Q)"="cfQ")) #Close radioButtons
          , sliderInput("kappa","Value of kappa for Crawford-Ferguson:",min=0, max=1, value=0, step=.05) # sliderInput - "kappa" - value for Crawford-Ferguson
          , br(), br()
          , imageOutput("PyramidImage", width="50%", height="50%")
       )},# Close sidebarPanel

      {mainPanel(
        {tabsetPanel(id="tabcur",
          tabPanel("Data", id="tabData", imageOutput("datavars", width="100%", height="700px")),
          tabPanel("Correlations", id="tabCorrelations", plotOutput("corrgramX", width="90%", height="800px")),
          tabPanel("Eigens", id="tabEigens",
                   plotOutput("eigens", width="50%",height="400px"),
                   plotOutput("RMSEA", width="50%", height="400px")),
          tabPanel("Components", id="tabComponents",
                 plotOutput("patternPlotPCA", width="70%", height="750px")),
          tabPanel("Factors", id="tabFactors",
                 plotOutput("patternPlotFA", width="70%", height="1200px"),
#                  br(),
                 plotOutput("corrgramF", width="50%", height="180px")),
          tabPanel("Documentation", id="tabDocumentation", htmlOutput(outputId="documentation")),
          selected="Data"
        )} #Close tabsetPanel
      )} #Close mainPanel
  )} #Close pageWithSidebar
)} #Close shinyUI

# app.R
library(shiny)
source("canis_lupus.R")  # Load your analysis code

ui <- fluidPage(
  # Custom CSS for styling with wolf-inspired color scheme
  tags$head(
    tags$style(HTML("
      /* Wolf-inspired color scheme */
      :root {
        --color-wolf-dark: #2D2926;
        --color-wolf-light: #E5E4E2;
        --color-wolf-gold: #D4AF37;
        --color-wolf-warm: #8B5A2B;
        --color-wolf-cool: #6D7B8D;
      }
      body {
        font-family: 'Arial', sans-serif;
        background-color: #F5F5F5;
       # background-image: url('canis_lupus.png');
        background-size: 300px;
        background-position: right bottom;
        background-repeat: no-repeat;
        background-attachment: fixed;
        opacity: 0.95;
      }
      .main-container {
        background-color: rgba(255, 255, 255, 0.9);
        border-radius: 8px;
        padding: 20px;
        margin-top: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .navbar {
        background-color: var(--color-wolf-dark) !important;
      }
      .footer {
        background-color: var(--color-wolf-dark);
        color: var(--color-wolf-light);
        padding: 20px;
        margin-top: 30px;
        text-align: center;
        font-size: 14px;
        border-top: 3px solid var(--color-wolf-gold);
      }
      .contact-links a {
        margin: 0 15px;
        color: var(--color-wolf-gold);
        text-decoration: none;
        font-weight: bold;
      }
      .contact-links a:hover {
        color: var(--color-wolf-light);
      }
      .main-title {
        margin-bottom: 5px;
        color: var(--color-wolf-dark);
      }
      .subtitle {
        color: var(--color-wolf-cool);
        font-style: italic;
        margin-bottom: 20px;
      }
      .sidebar-panel {
        background-color: white;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        padding: 15px;
        border-left: 4px solid var(--color-wolf-warm);
      }
      .main-panel {
        background-color: white;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        padding: 20px;
      }
      .btn-primary {
        background-color: var(--color-wolf-dark);
        border-color: var(--color-wolf-dark);
        color: white;
      }
      .btn-primary:hover {
        background-color: var(--color-wolf-warm);
        border-color: var(--color-wolf-warm);
        color: white;
      }
      .tabbable > .nav > li > a {
        color: var(--color-wolf-cool);
      }
      .tabbable > .nav > li.active > a {
        color: var(--color-wolf-dark);
        font-weight: bold;
        border-bottom: 2px solid var(--color-wolf-gold);
      }
      h4 {
        color: var(--color-wolf-dark);
        border-bottom: 2px solid var(--color-wolf-warm);
        padding-bottom: 5px;
      }
      hr {
        border-top: 1px solid var(--color-wolf-warm);
      }
    "))
  ),
  # Main title with logo and subtitle
  div(class = "main-container",
      div(
        style = "display: flex; align-items: center; margin-bottom: 20px;",
        img(src = "canis_lupus.png", height = "80px", style = "margin-right: 20px; border: 3px solid var(--color-wolf-gold); border-radius: 50%;", alt = "Canis lupus logo"),
        div(
          h2("CanisLupus2.0", class = "main-title", style = "margin-bottom: 2;"),
          h4("Microbiome Data Analysis Tool", class = "subtitle", style = "margin-top: 10px;")
        )
      ),
      # Main content
      navbarPage(
        id = "mainTabs",
        title = "",
        tabPanel("Home"),
        tabPanel("Data Upload",
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     class = "sidebar-panel",
                     h4("Data Input", style = "color: var(--color-wolf-dark);"),
                     fileInput("asvFile", "Upload ASV Table (CSV)", accept = ".csv", width = "100%"),
                     fileInput("taxFile", "Upload Taxonomy Table (CSV)", accept = ".csv", width = "100%"),
                     fileInput("metaFile", "Upload Metadata Table (CSV)", accept = ".csv", width = "100%"),
                     fileInput("treeFile", "Upload Phylogenetic Tree", accept = c(".tree", ".tre", ".nwk", ".txt"), width = "100%"),
                     actionButton("update", "Load Data", class = "btn-primary", style = "width: 100%;")
                   ),
                   mainPanel(
                     width = 9,
                     class = "main-panel",
                     h4("Data Summary", style = "color: var(--color-wolf-dark);"),
                     verbatimTextOutput("dataSummary"),
                     hr(),
                     h4("Sample Table", style = "color: var(--color-wolf-dark);"),
                     DTOutput("sampleTable")
                   )
                 )
        ),
        tabPanel("Visual Exploration",
                 tabsetPanel(
                   tabPanel("Stacked Bar Plots",
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                class = "sidebar-panel",
                                selectInput("taxLevelBar", "Taxonomic Level:",
                                            choices = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                            selected = "Phylum"),
                                selectInput("regionBar", "Region/Sample Group:",
                                            choices = NULL),
                                sliderInput("topNBar", "Number of Top Taxa to Show:",
                                            min = 5, max = 20, value = 10),
                                actionButton("updateBar", "Update Plot", class = "btn-primary")
                              ),
                              mainPanel(
                                width = 9,
                                plotlyOutput("taxaBarplot", height = "600px"),
                                plotlyOutput("relativeAbundancePlot", height = "600px")
                              )
                            )
                   ),
                   tabPanel("Interactive Pie Chart",
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                class = "sidebar-panel",
                                selectInput("taxLevelPie", "Taxonomic Level:",
                                            choices = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                            selected = "Phylum"),
                                selectInput("regionPie", "Region/Sample Group:",
                                            choices = NULL),
                                actionButton("updatePie", "Update Plot", class = "btn-primary")
                              ),
                              mainPanel(
                                width = 9,
                                plotlyOutput("pieChart", height = "800px")
                              )
                            )
                   ),
                   tabPanel("Rarefaction Curve",
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                class = "sidebar-panel",
                                selectInput("regionRare", "Region/Sample Group:",
                                            choices = NULL),
                                actionButton("updateRare", "Update Plot", class = "btn-primary")
                              ),
                              mainPanel(
                                width = 9,
                                plotlyOutput("rarefactionCurve", height = "600px")
                              )
                            )
                   ),
                   tabPanel("Phylogenetic Tree",
                            plotOutput("phylogeneticTree", height = "800px")
                   ),
                   tabPanel("Heat Tree",
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                class = "sidebar-panel",
                                selectInput("taxLevelHeatTree", "Taxonomic Level:",
                                            choices = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                            selected = "Phylum"),
                                actionButton("updateHeatTree", "Update Plot", class = "btn-primary")
                              ),
                              mainPanel(
                                width = 9,
                                plotOutput("heatTree", height = "800px")
                              )
                            )
                   )
                 )
        ),
        tabPanel("Community Profile",
                 tabsetPanel(
                   tabPanel("Alpha Diversity",
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                class = "sidebar-panel",
                                selectInput("alphaMeasure", "Diversity Measure:",
                                            choices = c("Observed", "Shannon", "Simpson", "InvSimpson", "Fisher"),
                                            selected = "Shannon"),
                                selectInput("regionAlpha", "Group by:",
                                            choices = NULL),
                                actionButton("updateAlpha", "Update Plot", class = "btn-primary")
                              ),
                              mainPanel(
                                width = 9,
                                plotlyOutput("alphaDiv", height = "600px"),
                                plotlyOutput("alphaDivBoxplot", height = "600px")
                              )
                            )
                   ),
                   tabPanel("Beta Diversity",
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                class = "sidebar-panel",
                                selectInput("betaMethod", "Method:",
                                            choices = c("PCoA", "NMDS", "CCA", "RDA"),
                                            selected = "PCoA"),
                                selectInput("betaDistance", "Distance Metric:",
                                            choices = c("bray", "jaccard", "unifrac", "wunifrac", "euclidean"),
                                            selected = "bray"),
                                selectInput("regionBeta", "Color by:",
                                            choices = NULL),
                                actionButton("updateBeta", "Update Plot", class = "btn-primary")
                              ),
                              mainPanel(
                                width = 9,
                                plotlyOutput("betaPlot", height = "600px"),
                                plotlyOutput("nmdsPlot", height = "600px")
                              )
                            )
                   ),
                   tabPanel("Core Microbiome",
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                class = "sidebar-panel",
                                selectInput("taxLevelCore", "Taxonomic Level:",
                                            choices = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                            selected = "Genus"),
                                selectInput("regionCore", "Region/Sample Group:",
                                            choices = NULL),
                                sliderInput("prevalenceCore", "Prevalence Threshold (%):",
                                            min = 5, max = 100, value = 50),
                                sliderInput("detectionCore", "Detection Threshold (%):",
                                            min = 0.001, max = 10, value = 0.1, step = 0.01),
                                actionButton("updateCore", "Update Analysis", class = "btn-primary")
                              ),
                              mainPanel(
                                width = 9,
                                plotlyOutput("coreHeatmap", height = "600px"),
                                verbatimTextOutput("coreSummary")
                              )
                            )
                   )
                 )
        ),
        tabPanel("Network Analysis",
                 tabsetPanel(
                   tabPanel("Interactive Heatmap",
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                class = "sidebar-panel",
                                selectInput("taxLevelHeatmap", "Taxonomic Level:",
                                            choices = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                            selected = "Genus"),
                                selectInput("regionHeatmap", "Region/Sample Group:",
                                            choices = NULL),
                                actionButton("updateHeatmap", "Update Plot", class = "btn-primary")
                              ),
                              mainPanel(
                                width = 9,
                                plotlyOutput("interactiveHeatmap", height = "800px")
                              )
                            )
                   ),
                   tabPanel("Dendrogram",
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                class = "sidebar-panel",
                                selectInput("distanceMethod", "Distance Method:",
                                            choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                                            selected = "euclidean"),
                                selectInput("clusterMethod", "Clustering Method:",
                                            choices = c("complete", "ward.D", "ward.D2", "single", "average", "mcquitty", "median", "centroid"),
                                            selected = "complete"),
                                actionButton("updateDendro", "Update Plot", class = "btn-primary")
                              ),
                              mainPanel(
                                width = 9,
                                plotlyOutput("dendrogram", height = "800px")
                              )
                            )
                   ),
                   tabPanel("Correlation Network",
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                class = "sidebar-panel",
                                selectInput("taxLevelNetwork", "Taxonomic Level:",
                                            choices = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                            selected = "Genus"),
                                sliderInput("corThreshold", "Correlation Threshold:",
                                            min = 0.5, max = 0.99, value = 0.7, step = 0.05),
                                actionButton("updateNetwork", "Update Network", class = "btn-primary")
                              ),
                              mainPanel(
                                width = 9,
                                networkD3::forceNetworkOutput("correlationNetwork", height = "800px")
                              )
                            )
                   )
                 )
        )
      )
  ),
  # Footer with contact information
  div(class = "footer",
      h4("Contact Information", style = "color: var(--color-wolf-gold);"),
      div(class = "contact-links",
          a(href = "mailto:pyappiah561@gmail.com", icon("envelope", style = "margin-right: 5px;"), "Email"),
          a(href = "https://www.linkedin.com/in/philip-appiah", icon("linkedin", style = "margin-right: 5px;"), "LinkedIn"),
          a(href = "https://github.com/barah123", icon("github", style = "margin-right: 5px;"), "GitHub")
      ),
      p(style = "color: var(--color-wolf-gold); margin-top: 15px;",
        "© 2025 Canis Lupus Microbiome Project.|Designed by Philip Appiah|All rights reserved.")
  )
)

server <- function(input, output, session) {
  ps <- reactiveVal(NULL)
}

shinyApp(ui, server)

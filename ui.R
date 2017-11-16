## Medfate service UI
## Design and development by Miquel de Cáceres, Antoine Cabon and Víctor Granda

# libraries
# libraries
library(shiny)
library(shinythemes)

# Navbar page layout. Welcome page, docs, example app and info

navbarPage(
  title = 'medfate R package',
  id = 'navbar_medfate',
  theme = shinytheme('sandstone'),
  
  # Welcome page ####
  tabPanel(
    title = 'Welcome', icon = icon('pagelines'),
    
    # welcome page in html
    div(
      id = 'welcome',
      style = 'width:100%',
      
      # custom css
      includeCSS("www/custom.css"),
      
      # a little space
      br(),
      
      # logo with link to CRAN
      a(
        id = 'logo',
        href = "https://cran.r-project.org/package=medfate",
        img(style = 'display:inline-block',
            src = 'package_logo.png', alt = 'medfate link')
      ),
      
      # subtitle
      h2(id = 'subtitle',
         'Mediterranean Forest Simulation'),
      
      # a little space
      br(),
      
      # left div
      column(
        6,
        br(),
        div(
          id = 'leftdiv',
          style = 'display:inline-block;text-align:justify',
          br(),
          p(style = 'margin: 10px 30px 10px 30px; font-size:16px',
            strong('medfate'),
            'package provides functions to simulate forest dynamics ',
            '(Daily Water Balance, Photosynthesis, Growth...) ',
            'using cohort-based description of vegetation.'),
          br()
        )
      ),
      
      # right div
      div(
        id = 'rightdiv',
        style = 'display:inline-block;width:50%;text-align:justify',
        br(),
        p(style = 'margin: 10px 30px 10px 30px; font-size:16px',
          'Here you will learn how to use the ',
          strong('medfate'), 'R package to simulate forest dynamics. '),
        p(style = 'margin: 10px 30px 10px 30px; font-size:16px',
          'In addition, an interactive shiny app is provided to ',
          'illustrate the package capabilities, using an imaginary field plot ',
          'and several plausible climatic and physiological scenarios')
      ),
      
      br(),br(),br()
    ),
    
    # an space
    br(), br(),
    
    # logos row
    div(
      style = 'text-align:center',
      fluidRow(
        column(
          4,
          a(
            id = 'logo_grupo',
            href = "http://vegmod.ctfc.cat/",
            img(style = 'display:inline-block',
                src = 'LOGO_Group_scaled.png')
          )
        ),
        column(
          4,
          a(
            id = 'logo_ctfc',
            href = "http://www.ctfc.cat/",
            img(style = 'display:inline-block',
                src = 'logo_ctfc_scaled.png')
          )
        ),
        column(
          4,
          a(
            id = 'logo_creaf',
            href = "http://www.creaf.cat/",
            img(style = 'display:inline-block',
                src = 'logo_creaf_scaled.png')
          )
        )
      )
    )
  ),
  
  # Doc and Vignettes ####
  tabPanel(
    title = 'Documentation & Vignettes', icon = icon('book'),
    
    # a little space
    br(),
    
    fluidRow(
      
      column(
        2,
        downloadButton('swb_dwn', label = 'Soil Water Balance')
      ),
      
      column(
        3,
        p('Local water processes (soil water balance of forest stands). ',
          'Processes affecting soil water content include rainfall, canopy ',
          'interception, infiltration and runoff, percolation and deep ',
          'drainage, soil evaporation and plant transpiration.')
      ),
      
      
      
      column(
        2, offset = 1,
        downloadButton('growth_dwn', label = 'Forest Growth')
      ),
      
      column(
        3,
        p('Simulating plant growth of a set of cohorts (competing for light ',
          'and water) in a single forest stand or for multiple stands ',
          'distributed spatially.')
      )
    ),
    
    br(), br(), br(),
    
    fluidRow(
      
      column(
        2,
        downloadButton('hyd_dwn', label = 'Hydraulics')
      ),
      
      column(
        3,
        p('Hydraulics details of the Sperry et al. (2016) model for canopy water ',
          'supply, a cost-benefit approach where hydraulic costs of opening the ',
          'stomata are compared against photosynthetic gain.')
      ),
      
      column(
        2, offset = 1,
        downloadButton('roots_dwn', label = 'Root Systems')
      ),
      
      column(
        3,
        p('Soil description, root distribution and belowground hydraulic ',
          'conductances.')
      )
    )
  ),
  
  # Shiny example app ####
  tabPanel(
    title = 'Example_App', icon = icon('television'),
    
    fluidPage(
      
      # explanation
      fluidRow(
        h2('Play with medfate!'),
        p('With this app you can explore the capabilities and visualize ',
          'the outputs of the modelization.'),
        p('In this imaginary plot you can ',
          'select a tree and a shrub species, change the LAI for that species ',
          'and specify the soil description. Output plots are redrawed with ',
          'each change. At the bottom the input object used to modelize the ',
          'forest dynamic is showed.')
      ),
      br(),
      
      # app
      fluidRow(
        
        # inputs
        column(
          2,
          
          inputPanel(
            # Species inputs
            selectInput(
              'tree_1', 'Tree cohort species',
              choices = c(
                'Pinus sylvestris' = 27,
                'Pinus halepensis' = 22,
                'Quercus ilex' = 33,
                'Quercus pubescens' = 32,
                'Fagus sylvatica' = 13,
                'Arbutus unedo' = 5
              ),
              selected = c(27)
            ),
            
            selectInput(
              'shrub_1', 'Shrub cohort species',
              choices = c(
                'Buxus sempervirens' = 6,
                'Arbutus unedo' = 5,
                'Pistacia lentiscus' = 28,
                'Viburnum spp.' = 43,
                'Phillyrea latifolia' = 21,
                'Quercus ilex' = 33
              ),
              selected = c(6)
            ),
            
            # lai inputs
            numericInput('lai_t1', 'Select LAI for Sp 1', 1, 0.1, 5, 0.1),
            numericInput('lai_s1', 'Select LAI for Sp 1', 0.3, 0.1, 5, 0.1),
            
            # soil inputs
            numericInput('soil_depth', 'Second soil layer width [mm] (First layer fixed at 300).',
                         value = 700, min = 100, max = 2000, step = 1),
            selectInput('soil_tex', 'Soil texture (USDA classes)',
                        choices = c('Clay', 'Sand', 'Silt',
                                    'Loam', 'SaLo', 'SiLo'),
                        selected = 'Loam')
          )
        ),
        
        # outputs
        column(
          4,
          plotOutput('meteo_plot', width = '100%', height = "300px"),
          plotOutput('e_plot', width = '100%', height = "300px"),
          plotOutput('evap_plot', width = '100%', height = "300px")
        ),
        column(
          4,
          plotOutput('swc_plot', width = '100%', height = "300px"),
          plotOutput('stress_plot', width = '100%', height = "300px"),
          plotOutput('watexp_plot', width = '100%', height = "300px")
        )
      ),
      
      # row for the swbInput object
      br(),
      
      fluidRow(
        column(
          2
          #empty
        ),
        column(
          8,
          h4('swbInput object'),
          verbatimTextOutput('swb_input')
        )
      )
    )
  ),
  
  # Validation tab ####
  tabPanel(
    title = 'Model Validation', icon = icon('check')
  ),
  
  # About tab (tabwith r package description, disclaimer and so on...)
  tabPanel(
    title = 'About', icon = icon('info-circle'),
    
    # RMardown document
    includeMarkdown('Docs/Disclaimer.Rmd')
  )
)

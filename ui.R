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
            'package provides functions to simulate forest functioning and dynamics ',
            '(daily water balance, photosynthesis, growth...) ',
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
          'An interactive shiny app is provided here to ',
          'illustrate the package capabilities of',
          strong('medfate'), 'R package to simulate forest water balance. '),
        p(style = 'margin: 10px 30px 10px 30px; font-size:16px',
          'See package documentation at',
          a(href='https://vegmod.ctfc.cat/software/medfate/', 'our website')),
        br()
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
  # tabPanel("Installation & documentation", icon = icon('book'),
  #          fluidPage(
  #            h2('Package installation'),
  #            br(),
  #            fluidRow(
  #              # RMardown document
  #              column(
  #                8,
  #                includeMarkdown('Docs/Installation.Rmd')
  #              )
  #            )
  #          ),
  #          br(), br(), 
  #          fluidPage(
  #            h2('Package documentation'),
  #            br(),
  #            fluidRow(
  #              # RMardown document
  #              column(
  #                8,
  #                includeMarkdown('Docs/Documentation.Rmd')
  #              )
  #            )
  #          )
  #          
  # ),
  
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
                'Pinus sylvestris' = 59,
                'Pinus halepensis' = 54,
                'Quercus humilis' = 67,
                'Quercus ilex' = 68,
                'Fagus sylvatica' = 37,
                'Arbutus unedo' = 6
              ),
              selected = c(59)
            ),
            
            selectInput(
              'shrub_1', 'Shrub cohort species',
              choices = c(
                'Buxus sempervirens' = 12,
                'Arbutus unedo' = 6,
                'Pistacia lentiscus' = 61,
                'Viburnum spp.' = 88,
                'Phillyrea latifolia' = 53,
                'Quercus ilex' = 67
              ),
              selected = c(12)
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
          h4('spwbInput object'),
          verbatimTextOutput('spwb_input')
        )
      )
    )
  ),
  
  # Validation tab ####
  # tabPanel(
  #   title = 'Model Validation', icon = icon('check'),
  #   
  #   fluidRow(
  #     
  #     column(
  #       6,
  #       h3('SWC validation'),
  #       plotOutput('val_swc_plot', height = '250px')
  #     ),
  #     
  #     column(
  #       6,
  #       h3('Tranpiration validation'),
  #       plotOutput('val_e_plot', height = '250px')
  #     ),
  #     
  #     br(), br(),
  #     
  #     p('These are the results of the model validation in 12 plots with known ',
  #       'SWC and Transpiration up to date (', Sys.Date(), ').')
  #   )
  # ),
  # 
  
  # About tab (tabwith r package description, disclaimer and so on...) ####
  tabPanel(
    title = 'About', icon = icon('info-circle'),
    
    # RMardown document
    includeMarkdown('Docs/Disclaimer.Rmd'),
    
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
  )
)

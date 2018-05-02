## medfate service server

# libraries
library(shiny)
library(medfate)
# library(MedfateValidation)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# server logic
function(input, output, session) {
  
  # update inputs ####
  observe({
    
    data('SpParamsMED', package = 'medfate')
    
    t_1 <- SpParamsMED[as.numeric(input$tree_1) + 1, 'Name']
    s_1 <- SpParamsMED[as.numeric(input$shrub_1) + 1, 'Name']
    
    updateNumericInput(session, 'lai_t1', label = paste0('Select LAI for ', t_1),
                       1, 0.1, 5, 0.1)
    updateNumericInput(session, 'lai_s1', label = paste0('Select LAI for ', s_1),
                       0.3, 0.1, 5, 0.1)
  })
  
  # model as reactive ####
  
  get_res <- reactive({
    
    # build the input object
    
    t_1 <- SpParamsMED[as.numeric(input$tree_1) + 1, 'Name']
    s_1 <- SpParamsMED[as.numeric(input$shrub_1) + 1, 'Name']
    
    data(examplemeteo)
    
    data(exampleforest)
    exampleforest[['treeData']] <- exampleforest[['treeData']][1,]
    exampleforest[['shrubData']] <- exampleforest[['shrubData']][1,]
    exampleforest[['treeData']][['Species']] <- as.numeric(input$tree_1)
    exampleforest[['shrubData']][['Species']] <- as.numeric(input$shrub_1)
    
    soil_texture <- switch(
      input$soil_tex,
      Clay = c(70,10,20),
      Sand = c(5,90,5),
      Silt = c(10,10,90),
      Loam = c(20,40,40),
      SaLo = c(10,60,30),
      SiLo = c(10,30,60)
    )
    
    examplesoil <- soil(defaultSoilParams(2))
    examplesoil$dVec <- c(300, input$soil_depth)
    examplesoil$sand <- rep(soil_texture[2], 2)
    examplesoil$clay <- rep(soil_texture[1], 2)
    
    control <- defaultControl()
    
    input_simple <- forest2swbInput(exampleforest, examplesoil, SpParamsMED, control)
    
    input_simple$above$LAI_live <- c(input$lai_t1, input$lai_s1)
    
    # run the model
    swb(input_simple, examplesoil, examplemeteo)
  })
  
  
  
  # Plots ####
  
  output$e_plot <- renderPlot({
    
    t_1 <- SpParamsMED[as.numeric(input$tree_1) + 1, 'Name']
    s_1 <- SpParamsMED[as.numeric(input$shrub_1) + 1, 'Name']
    
    # transp plot
    transp_data <- as.data.frame(get_res()[['PlantTranspiration']])
    names(transp_data) <- c(t_1, s_1)
     
    transp_data %>%
      mutate(Date = as.Date(row.names(.))) %>%
      gather(Species, E, -Date) %>%
      ggplot(aes(x = Date, y = E, colour = Species)) +
      geom_line(size = 2) +
      scale_colour_manual(values = c('#26A65B', '#87D37C')) +
      labs(y = 'Transpiration') +
      theme_medfate()
  })
  
  output$meteo_plot <- renderPlot({
    
    # precip plot
    data(examplemeteo)
    
    examplemeteo %>%
      mutate(Date = as.Date(row.names(.)),
             Precipitation = if_else(Precipitation == 0, NA_real_, Precipitation)) %>%
      ggplot(aes(x = Date, y = Precipitation)) +
      geom_col(fill = 'black') +
      geom_line(aes(y = PET), colour = 'darkgrey', size = 2) +
      labs(y = 'Precipitation & PET') +
      theme_medfate()
  })
  
  output$swc_plot <- renderPlot({
    
    # swc plot
    soil_texture <- switch(
      input$soil_tex,
      Clay = c(70,10,20),
      Sand = c(5,90,5),
      Silt = c(10,10,90),
      Loam = c(20,40,40),
      SaLo = c(10,60,30),
      SiLo = c(10,30,60)
    )
    
    examplesoil <- soil(defaultSoilParams(2))
    examplesoil$dVec <- c(300, input$soil_depth)
    examplesoil$sand <- rep(soil_texture[2], 2)
    examplesoil$clay <- rep(soil_texture[1], 2)
    
    Theta_FC = soil.thetaFC(examplesoil)
    swc_data <- as.data.frame(get_res()[['SoilWaterBalance']][,1:2]) %>%
      mutate(Date = as.Date(row.names(.)),
             Shallow = W.1 * Theta_FC[1],
             Deep = W.2 * Theta_FC[2]) %>%
      select(Date, Shallow, Deep)
    
   swc_data %>%
     gather(Layers, SWC, -Date) %>%
     ggplot(aes(x = Date, y = SWC, colour = Layers)) +
     geom_line(size = 2) +
     scale_colour_manual(values = c('#1F3A93', '#3498DB')) +
     labs(y = 'Soil Water Content') +
     theme_medfate()
  })
  
  output$stress_plot <- renderPlot({
    
    # stress plot
    t_1 <- SpParamsMED[as.numeric(input$tree_1) + 1, 'Name']
    s_1 <- SpParamsMED[as.numeric(input$shrub_1) + 1, 'Name']
    
    stress_data <- as.data.frame(get_res()[['PlantStress']])
    names(stress_data) <- c(t_1, s_1)
    
    stress_data %>%
      mutate(Date = as.Date(row.names(.))) %>%
      gather(Species, Stress, -Date) %>%
      ggplot(aes(x = Date, y = Stress, colour = Species)) +
      geom_line(size = 2) +
      scale_colour_manual(values = c('#26A65B', '#87D37C')) +
      labs(y = 'Plant Stress [0-1]') +
      theme_medfate()
  })
  
  output$evap_plot <- renderPlot({
    
    # evap plot
    as.data.frame(get_res()[['DailyBalance']]) %>%
      mutate(Date = as.Date(row.names(.))) %>%
      select(Date, Etot, Esoil, Eplanttot) %>%
      gather(Origin, Evaporation, -Date) %>%
      ggplot(aes(x = Date, y = Evaporation, colour = Origin)) +
      geom_line(size = 1.5, alpha = 0.5) +
      scale_colour_manual(values = c('#26A65B', '#F27935', 'black'),
                          labels = c('Plants', 'Soil', 'Total')) +
      labs(y = 'Evaporation') +
      theme_medfate()
  })
  
  output$watexp_plot <- renderPlot({
    
    # water export plot
    as.data.frame(get_res()[['DailyBalance']]) %>%
      mutate(Date = as.Date(row.names(.))) %>%
      select(Date, Runoff, DeepDrainage) %>%
      # mutate(Total = Runoff + DeepDrainage) %>%
      gather(Export, Value, -Date) %>%
      ggplot(aes(x = Date, y = Value, colour = Export)) +
      geom_line(size = 1.5, alpha = 0.5) +
      scale_colour_manual(values = c('#1F3A93', '#3498DB')) +
      labs(y = 'Water export') +
      theme_medfate()
  })
  
  output$swb_input <- renderPrint({
    
    # build the input object
    t_1 <- SpParamsMED[as.numeric(input$tree_1) + 1, 'Name']
    s_1 <- SpParamsMED[as.numeric(input$shrub_1) + 1, 'Name']
    
    data(examplemeteo)
    
    data(exampleforest)
    exampleforest[['treeData']] <- exampleforest[['treeData']][1,]
    exampleforest[['shrubData']] <- exampleforest[['shrubData']][1,]
    exampleforest[['treeData']][['Species']] <- as.numeric(input$tree_1)
    exampleforest[['shrubData']][['Species']] <- as.numeric(input$shrub_1)
    
    soil_texture <- switch(
      input$soil_tex,
      Clay = c(70,10,20),
      Sand = c(5,90,5),
      Silt = c(10,10,90),
      Loam = c(20,40,40),
      SaLo = c(10,60,30),
      SiLo = c(10,30,60)
    )
    
    examplesoil <- soil(defaultSoilParams(2))
    examplesoil$dVec <- c(300, input$soil_depth)
    examplesoil$sand <- rep(soil_texture[2], 2)
    examplesoil$clay <- rep(soil_texture[1], 2)
    
    control <- defaultControl()
    
    input_simple <- forest2swbInput(exampleforest, examplesoil, SpParamsMED, control)
    
    input_simple$above$LAI_live <- c(input$lai_t1, input$lai_s1)
    
    input_simple
  })
  
  
  # Download helpers (Vignettes) ####
  
  output$swb_dwn1 <- downloadHandler(
    filename = 'SimpleModelSWB.pdf',
    content = function(file) {
      file.copy('Docs/SimpleModelSWB.pdf', file)
    }
  )
  
  output$swb_dwn2 <- downloadHandler(
    filename = 'ComplexModelSWB.pdf',
    content = function(file) {
      file.copy('Docs/ComplexModelSWB.pdf', file)
    }
  )
  
  output$growth_dwn <- downloadHandler(
    filename = 'ForestGrowth.pdf',
    content = function(file) {
      file.copy('Docs/ForestGrowth.pdf', file)
    }
  )
  
  output$hyd_dwn <- downloadHandler(
    filename = 'HydraulicsPhotosynthesis.pdf',
    content = function(file) {
      file.copy('Docs/HydraulicsPhotosynthesis.pdf', file)
    }
  )
  
  # # Validation ####
  # 
  # # swc_plot
  # output$val_swc_plot <- renderPlot({
  #   swc_table_raw %>%
  #     dplyr::rename(
  #       Rsq_Granier = R_sq_simple,
  #       Rsq_Sperry = R_sq_complex,
  #       Rsq_both = R_sq_both) %>%
  #     gather(Statistic, Value, -Site, -Layer) %>%
  #     tidyr::separate(Statistic, c('Statistic', 'Model')) %>%
  #     dplyr::mutate(Model = stringr::str_replace(Model, 'both', 'versus'),
  #                   Model = stringr::str_replace(Model, 'simple', 'Granier'),
  #                   Model = stringr::str_replace(Model, 'complex', 'Sperry')) %>%
  #     ggplot(aes(x = Model, y = Value)) +
  #     geom_boxplot() +
  #     facet_wrap(~Statistic, ncol = 3, scales = 'free') +
  #     labs(x = '', y = '') +
  #     theme_medfate()
  # })
  # 
  # # E plot
  # output$val_e_plot <- renderPlot({
  #   etot_table_raw %>%
  #     dplyr::rename(
  #       Eplanttot_Rsq_simple = Eplanttot_r_sq_simple,
  #       Eplanttot_Rsq_complex = Eplanttot_r_sq_complex,
  #       Eplanttot_Rsq_both = Eplanttot_r_sq_both) %>%
  #     gather(Statistic, Value, -Site) %>%
  #     tidyr::separate(Statistic, c('Var', 'Statistic', 'Model')) %>%
  #     dplyr::select(Site, Statistic, Model, Value) %>%
  #     dplyr::mutate(Statistic = stringr::str_replace(Statistic, 'bias', 'Bias')) %>%
  #     dplyr::mutate(Model = stringr::str_replace(Model, 'both', 'versus'),
  #                   Model = stringr::str_replace(Model, 'simple', 'Granier'),
  #                   Model = stringr::str_replace(Model, 'complex', 'Sperry')) %>%
  #     ggplot(aes(x = Model, y = Value)) +
  #     geom_boxplot() +
  #     facet_wrap(~Statistic, ncol = 3, scales = 'free') +
  #     labs(x = '', y = '') +
  #     theme_medfate()
  # })
}

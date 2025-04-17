
# ======== packages ========

library(shiny)
library(ggplot2)
library(ggiraph)
library(remotes)
library(withr)
library(scales)
library(patchwork)
if (!require("cmi")) {
  remotes::install_github("paulbeardactuarial/cmi")
}
library(cmi)


# ======== data items ========

max_iteration <- 500 ## <-- keeping max_iteration low as Shiny server could become overloaded if keep at 10,000. Will cause APCI to give up sometimes

extract_slider_vars_rp <-
  function(list) {
    list(
      list$smoothing_params$alpha,
      list$smoothing_params$beta,
      list$smoothing_params$kappa,
      list$smoothing_params$gamma,
      list$age$min,
      list$age$max,
      list$age$cohort_low,
      list$age$cohort_high,
      list$year$min,
      list$year$max
    )
  }


plot_font <- "sans"

plot_text_theme <- list(
  theme(
    legend.position = "left",
    axis.text = element_text(
      family = plot_font,
      size = 12
    ),
    axis.title = element_text(
      family = plot_font,
      size = 14
    ),
    legend.title = element_text(
      family = plot_font,
      size = 12,
      margin = margin(0,0,15,0)
    ),
    legend.text = element_text(
      family = plot_font,
      size = 10,
      margin = margin(5,5,5,5)
    ),
    plot.margin = unit(c(0,40,0,40), "pt")
  )
)


# ======== main server function ========

function(input, output, session) {

  # run parameters

  runParametersReactive <- reactiveVal({cmi::rp})

  observe({
    rp <- runParametersReactive()
    rp$smoothing_params$alpha <- input$smoothAlpha
    rp$smoothing_params$beta <- input$smoothBeta
    rp$smoothing_params$kappa <- input$smoothKappa
    rp$smoothing_params$gamma <- input$smoothGamma
    rp$age$min <- input$ageRange[1]
    rp$age$max <- input$ageRange[2]
    rp$year$min <- input$yearRange[1]
    rp$year$max <- input$yearRange[2]
    rp$age$cohort_low <- input$cohortRange[1]
    rp$age$cohort_high <- input$cohortRange[2]
    runParametersReactive(rp)
  })

  # projection parameters

  projParametersReactive <- reactiveVal({cmi::projection_params})

  observe({
    pp <- projParametersReactive()
    pp$additional_improve <- input$additionalImprove / 100
    pp$ltr <- input$ltr / 100
    pp$age_taper_zero <- input$taperAge
    projParametersReactive(pp)
  }) |> debounce(500)

  # output messages (will be blank unless under certain conditions)

  output$alignmentMessage <- renderUI({
    slider_rp <- runParametersReactive()
    model <- cmi_proj_model()
    model_rp <- model$rp
    if (
      identical(
        extract_slider_vars_rp(slider_rp),
        extract_slider_vars_rp(model_rp)
      )
    ) {
      ""
    } else {
      tagList(
        icon("exclamation-triangle", class = "text-warning"),
        HTML(" APCI Parameter settings are not aligned to solved values. Click `Solve APCI` button to re-calculate "),
        icon("exclamation-triangle", class = "text-warning"),
      )
    }
  })

  output$taperAgeMessage <-
    renderText({
      taper_age <- input$taperAge
      max_age <- input$ageRange[2]
      if (taper_age > max_age) {
        ""
      } else {
        "Condition not met: Age Taper to Zero > Age Range Max."
      }
    })

  output$cohortAgeMessage <-
    renderText({
      min_cohort_age <- input$cohortRange[1]
      min_age <- input$ageRange[1]
      if (min_cohort_age >= min_age) {
        ""
      } else {
        "Condition not met: Cohort Constraint Range Min. >= Age Range Min."
      }
    })

  output$convergeFailMessage <-
    renderText({
      model <- cmi_proj_model()
      if (model$iteration_no < max_iteration) {
        ""
      } else {
        glue::glue("APCI Failed to solve after {max_iteration} iterations. Max. allowed iterations has been restricted due to limited server capacity.")
      }
    })

  # --- produce the solved APCI model (will update only when "click" button is pressed) ---



  cmi_proj_model <- eventReactive(
    #input$click,
    list(input$click, input$dataSetUsed),
    {
      rp <- runParametersReactive()

      data_subset <- input$dataSetUsed

      model <- cmi::CMI2022_model$new(
        name = data_subset,
        gender = data_subset,
        dth_exp = cmi::cmi_2022_dth_exp[[data_subset]],
        rp = rp
      )
      model$solve_apci(max_iteration = max_iteration)
      return(model)
    },
    ignoreNULL = FALSE
  )

  # switch to stop rendering outputs in cases where values ain't right
  shouldRenderAllOutputs <- reactiveVal(value = TRUE)
  observe({
    model <- cmi_proj_model()
    cohort_min <- input$cohortRange[1]
    age_min <- input$ageRange[1]
    taper_age <- input$taperAge
    age_max <- input$ageRange[2]
    shouldRenderAllOutputs(
      cohort_min >= age_min & taper_age > age_max & model$iteration_no < max_iteration
    )
  })

  # --- produce the mortality projections from the solved APCI model ---

  dataset <- reactive({

    # the dynamic parts of the code...
    model <- cmi_proj_model()
    pp <- projParametersReactive()

    # return nothing in cases that have gone wrong
    render_in_full <- shouldRenderAllOutputs()
    if (!render_in_full) {
      return()
    }

    # project mortality from the solved apci model
    model$projection_params <- pp
    model$project_mortality_improvements()
    projected_mi <-
      model$mortality_improvements_projected |>
      dplyr::filter(age <= 120 & year <= 2072) |>
      dplyr::mutate(cohort = year - age) |>
      dplyr::mutate(row_number = dplyr::row_number())
    return(projected_mi)
  })

  # --- produce the plots from the mortality projections ---

  output$heatmap <- renderGirafe({

    # return nothing in cases that have gone wrong
    render_in_full <- shouldRenderAllOutputs()
    if (!render_in_full) {
      return()
    }

    variable_used_for_p2 <- input$viewType

    p1 <-
      dataset() |>
      ggplot(aes(x = year, y = age, fill = mi)) +
      geom_tile() +
      geom_tile_interactive(
        aes(
          data_id = get(variable_used_for_p2),
          tooltip = paste0(variable_used_for_p2, ": ", get(variable_used_for_p2))
        )
      ) +
      theme_classic() +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_gradientn(
        name = "qImp (%)",
        colours = c(
          "#7f3b08",
          "#b35806",
          "#e08214",
          "#fdb863",
          "#fee0b6",
          "#f7f7f7",
          "#d8daeb",
          "#b2abd2",
          "#8073ac",
          "#542788",
          "#2d004b"
        ),
        values = scales::rescale(seq(from = 0.06, to = -0.06, length.out = 11)),
        limits = c(-0.06, 0.06),
        labels = scales::percent_format()
      ) +
      theme(
        legend.position = "left"
      ) +
      plot_text_theme

    p2_x_var <- ifelse(variable_used_for_p2 == "year", "age", "year")

    p2 <-
      dataset() |>
      ggplot(
        aes(
          x = get(p2_x_var),
          y = mi,
          group = get(variable_used_for_p2)
        )
      ) +
      geom_line_interactive(
        aes(
          data_id = get(variable_used_for_p2),
          tooltip = paste0(variable_used_for_p2, ": ", get(variable_used_for_p2))
        ),
        color = "grey",
        linewidth = 0.5
      ) +
      scale_y_continuous(
        name = "qImp (%)",
        labels = scales::percent_format(accuracy = 1),
        breaks = seq(from = -0.06, to = 0.06, by = 0.01)
      ) +
      scale_x_continuous(name = p2_x_var) +
      theme_classic() +
      plot_text_theme

    p <- p1 + p2

    ip <-
      girafe(
        ggobj = p,
        width_svg = 14,
        height_svg = 5,
        options =
          list(
            opts_hover(css = "stroke:black;color:black;line-width:30px"),
            opts_hover_inv(css = "opacity:0.15;"),
            opts_tooltip(css = "background-color:#008CBA; color:white; padding:5px; border-radius:4px;")
          )
      )

    return(ip)
  })


}

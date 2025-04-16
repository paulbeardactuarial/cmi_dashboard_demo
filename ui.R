library(shiny)
library(ggplot2)
library(ggiraph)
library(cmi)
library(shinyWidgets)
library(shinymanager)

ui <-
fluidPage(
  chooseSliderSkin("Shiny", color = "#008CBA"),

  # Static banner at the top with specified color and text
  tags$div(
    style = "background-color: #008CBA; width: 100%; padding: 18px; margin-bottom: 18px;",
    tags$a(
      href = "https://paulbeardactuarial.github.io/",
      style = "color: white; text-decoration: none; font-weight: none; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 2rem;",
      "paulbeardactuarial.github.io"
    )
  ),
  tags$head(
    tags$style(HTML("
      .btn-disabled {
        pointer-events: none;
        opacity: 0.5;
      }
    "))
  ),
  tags$head(
    tags$style(HTML("
      .radio-group-buttons > label {
        border: 1px #008CBA;
        color: #008CBA;
      }
      .radio-group-buttons .active {
        background-color: #008CBA !important;
        color: white !important;
      }
    "))
  ),
  h3("Introduction"),

  p("This dashboard interacts with a re-imagining of the CMI Mortality Projection Model 2022. It was built by Paul Beard (FIA) using R.
    "),

  h3("How it works"),

  p(HTML("
  The application loads an internally developed R package that is capable of performing the CMI 2022 mortality improvement methodology.
  <div style='margin-top: 0.5em;'>
  </div>
  When mortality improvements are projected, the code first solves the APCI (Age Period Cohort Improvement) fitting algorithm, using the APCI parameters defined by the user. The APCI takes a few seconds to solve, so the dashboard is not immediately reactive to these parameters, and updates when `Solve APCI` is pressed.
  When the APCI is solved the results are blended into a mortality projection, using the Projection parameters. As this is computationally simpler, the dashboard is immediately reactive to these parameters.
  <div style='margin-top: 0.5em;'>
  </div>
    The datasets `CMI_2022 Male` and `CMI_2022 Female` are the core calibration datasets used by the CMI 2022 and are sourced from the ONS. Selection of either of these <b>should</b> lead to mortality improvements identical to the CMI Mortality Projection Model 2022 (see Disclaimer).
    The `Synthetic` dataset is a simulated dataset. The output from this is a purely fictitious demonstration.
  <div style='margin-top: 0.5em;'>
  </div>
  The outputs are two plots. A heatmap and a line chart showing the mortality improvements. The line chart can be switched to view by either age, year or cohort.
    ")),

  h3("Key differences"),

  p(HTML("
  This dashboard interacts with the developed R model. The R model aims to perform the same calcuations as the VBA-coded CMI model, although due to differences in the languages, the methodology used is not direct like-for-like. The R-based model offers the following advatanges:
  <ul style='margin-top: 10px;'>
    <li>✅ 2x speed for single runs</li>
    <li>✅ 20x speed for batch runs</li>
    <li>✅ Larger capacity for batch runs</li>
    <li>✅ Easier integration with non-Microsoft Office tools</li>
    <li>✅ Enhanced visualization capabilities (such as this interactive dashboard!)</li>
    <li>✅ Improved reproducibility and version control</li>
  </ul>
")),

  h3("More Information"),

  p(HTML("
  The underlying code to produce the mortality improvement projections is not made available. However, the code to produce the dashboard can be found here:
  <a href='https://github.com/paulbeardactuarial/cmi_dashboard' target='_blank'>
    https://github.com/paulbeardactuarial/cmi_dashboard
  </a>.
")),

  h3("The Dashboard"),

  tags$style(HTML(
    "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;
      padding: 2.8px;
    }

    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .irs {
      flex-basis: 300px;          /* Target width for slider */
    }

    "
  )),

  fluidRow(
    column(
      width = 5,
      wellPanel(
        style = "padding: 15px;",
        tags$h4("Choice of Dataset", style = "margin-top: 0;"),

        div(
      radioGroupButtons(
        inputId = "dataSetUsed",
        selected = "synthetic",
        width = "95%",
        choiceNames = c("CMI_2022 Male", "CMI_2022 Female", "Synthetic"),
        choiceValues = c("male", "female", "synthetic"),
        justified = TRUE,
        checkIcon = list(yes = icon("ok", lib = "glyphicon")
        )

        # synthetic only...

        # div(
        # radioGroupButtons(
        #   inputId = "dataSetUsed",
        #   selected = "synthetic",
        #   width = "40%",
        #   choiceNames = "Synthetic",
        #   choiceValues = "synthetic",
        #   justified = TRUE,
        #   checkIcon = list(yes = icon("ok", lib = "glyphicon"))


  ))))),

  # Main content
  fluidRow(
    # Combined smoothing parameters
    column(
      width = 3,
      wellPanel(
        style = "padding: 15px;",
        tags$h4("APCI Smoothing Parameters", style = "margin-top: 0;"),

        # Age and cohort range sliders
        div(
          class = "label-left",
          sliderInput("smoothAlpha",
            "Sα",
            min = 6,
            max = 12,
            value = cmi::rp$smoothing_params$alpha,
            step = 0.1,
            ticks = FALSE
          )
        ),
        div(
          class = "label-left",
          sliderInput("smoothBeta",
            "Sβ",
            min = 6,
            max = 12,
            value = cmi::rp$smoothing_params$beta,
            step = 0.1,
            ticks = FALSE
          )
        ),
        div(
          class = "label-left",
          sliderInput("smoothGamma",
            "Sγ",
            min = 6,
            max = 12,
            value = cmi::rp$smoothing_params$gamma,
            step = 0.1,
            ticks = FALSE
          )
        ),
        div(
          class = "label-left",
          sliderInput("smoothKappa",
            "Sκ",
            min = 6,
            max = 12,
            value = cmi::rp$smoothing_params$kappa,
            step = 0.1,
            ticks = FALSE
          )
        ),
      )
    ),
    column(
      width = 4,
      wellPanel(
        style = "padding: 15px;",
        tags$h4("APCI Other Parameters", style = "margin-top: 0;"),

        # Age and cohort range sliders
        div(
          style = "transform-origin: left top;",
          sliderInput("ageRange",
            "Age Range",
            min = 20,
            max = 100,
            value = c(cmi::rp$age$min, cmi::rp$age$max),
            step = 1,
            ticks = FALSE
          )
        ),

        # Age and cohort range sliders
        div(
          style = "transform-origin: left top;",
          sliderInput("yearRange",
            "Year Range",
            min = 1961,
            max = 2022,
            value = c(cmi::rp$year$min, cmi::rp$year$max),
            step = 1,
            ticks = FALSE,
            sep = ""
          )
        ),

        # Age and cohort range sliders
        div(
          style = "transform-origin: left top;",
          sliderInput("cohortRange",
            "Cohort Constraint Range",
            min = 20,
            max = 140,
            value = c(cmi::rp$age$cohort_low, cmi::rp$age$cohort_high),
            step = 1,
            ticks = FALSE
          )
        )
      )
    ),

    # Right column: Projection parameters
    column(
      width = 4,
      wellPanel(
        style = "padding: 15px;",
        tags$h4("Projection Parameters", style = "margin-top: 0;"),

        # LTR slider
        div(
          style = "transform-origin: left top;",
          sliderInput("ltr",
            "LTR",
            min = -2,
            max = 4,
            value = cmi::projection_params$ltr * 100,
            step = 0.1,
            ticks = FALSE,
            post = "%"
          )
        ),

        # Taper Age slider (dynamic UI based on max age)
        div(
          style = "transform-origin: left top;",
          sliderInput("taperAge",
            "Age Taper to Zero",
            min = 80,
            max = 120,
            value = cmi::projection_params$age_taper_zero,
            step = 1,
            ticks = FALSE
          )
        ),

        # additionalImprove slider
        div(
          style = "transform-origin: left top;",
          sliderInput("additionalImprove",
            "Additional Improvement",
            min = -3,
            max = 3,
            value = cmi::projection_params$additional_improve * 100,
            step = 0.1,
            ticks = FALSE,
            post = "%"
          )
        ),
      )
    )
  ),

  fluidRow(
    column(
      width = 7,
      wellPanel(
        style = "padding: 15px;",
        tags$div(
          style = "text-align: center;",
          actionButton(
            "click",
            "Solve APCI",
            icon = icon("calculator"),
            style = "background-color: #008CBA; color: white; padding: 10px 30px; min-width: 200px;"
          )
        ),
        tags$div(
          style = "color: #008CBA; font-weight: bold; font-size: 16px; padding: 4px;",
          uiOutput("alignmentMessage")
        )
        )
      )),

  # Dropdown menu to pick the graph type shown (i.e. age, cohort or year)
  fluidRow(
    column(
      width = 4,
      # Dropdown selection
      prettyRadioButtons(
        inputId = "viewType",
        label = "View Type",
        thick = TRUE,
        inline = TRUE,
        choices = c("age", "cohort", "year"),
        animation = "pulse",
        status = "info"
      )
    )
  ),

  fluidRow(
    column(
      width = 6,
      offset = 3,
      tags$div(
        style = "color: red; font-weight: bold; font-size: 15px;",
        textOutput("taperAgeMessage")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      offset = 3,
      tags$div(
        style = "color: red; font-weight: bold; font-size: 15px;",
        textOutput("cohortAgeMessage")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      offset = 3,
      tags$div(
        style = "color: red; font-weight: bold; font-size: 15px;",
        textOutput("convergeFailMessage")
      )
    )
  ),

  # the money
  fluidRow(
    column(
      width = 11,
      style = "margin-top: 15px;",
      girafeOutput("heatmap", width = "100%")
    )
  ),

  div(
    style = "padding: 40px; font-size: 0.9em; color: #6c757d;",
    HTML("<strong>Disclaimer:</strong> This application is provided for demonstration purposes only.
        While every effort has been made to ensure accuracy, no guarantees are made regarding the completeness
        or reliability of the information presented. The author accepts no responsibility for any errors,
        omissions, or consequences arising from the use of this application. Use at your own risk.
        Commercial use of this application or any of its components is strictly prohibited.
        This application is not an official output of the Institute and Faculty of Actuaries (IFoA) or
        Continuous Mortality Investigation (CMI), and it has never received endorsement from these bodies.")
  ),

  # Footer
  tags$footer(
    style = "
      background-color: #EEEEEE;
      padding: 20px 0;
      text-align: center;
      position: relative;
      bottom: 0;
      width: 100%;
      font-size: 0.9em;
    ",
    div("© 2025 Paul Beard"),
    tags$a(
      href = "https://www.linkedin.com/in/paul-beard-78a420172/",
      target = "_blank",
      tags$i(class = "fab fa-linkedin", style = "margin: 0 10px;")
    ),
    tags$a(
      href = "https://github.com/paulbeardactuarial",
      target = "_blank",
      tags$i(class = "fab fa-github", style = "margin: 0 10px;")
    ),

    # Load Font Awesome for the icons
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
    )
  )
)  |>  secure_app()

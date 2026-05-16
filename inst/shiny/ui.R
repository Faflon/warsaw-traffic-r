fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700;800&family=DM+Mono:wght@300;400;500&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      * { box-sizing: border-box; }

      body { background: #edf2f9; font-family: 'Plus Jakarta Sans', sans-serif; color: #1e2a3a; margin: 0; overflow: hidden; }

      .container-fluid { padding: 0 !important; }
      .row { margin: 0 !important; }
      .col-sm-4, .col-sm-8 { padding: 0 !important; height: 100vh; }

      /* sidebar */
      .well {
        background: #fff !important;
        border: none !important;
        border-radius: 0 !important;
        box-shadow: 4px 0 20px rgba(30,60,120,0.07) !important;
        height: 100vh;
        overflow-y: auto;
        padding: 0 !important;
        margin: 0 !important;
      }
      .well::-webkit-scrollbar { width: 4px; }
      .well::-webkit-scrollbar-thumb { background: #c8d8f0; border-radius: 4px; }

      /* header */
      .app-header {
        padding: 26px 28px 22px;
        background: linear-gradient(135deg, #4b8ff5, #3a7de8);
      }
      .app-header p { margin: 0; }
      .app-title { font-size: 22px; font-weight: 800; color: #fff; letter-spacing: -0.02em; }
      .app-subtitle { font-family: 'DM Mono', monospace; font-size: 10px; color: rgba(255,255,255,0.6); letter-spacing: 0.16em; text-transform: uppercase; margin-top: 4px !important; }

      .sidebar-inner { padding: 8px 24px 28px; }

      /* section labels */
      h4 {
        font-size: 11px !important;
        font-weight: 700 !important;
        letter-spacing: 0.1em !important;
        text-transform: uppercase !important;
        color: #8fa3bf !important;
        margin: 26px 0 12px !important;
      }
      hr { border: none !important; border-top: 1px solid #edf2f9 !important; margin: 22px 0 !important; }

      /* radio button card */
      .radio-card {
        background: #f7faff;
        border: 1px solid #e4edf8;
        border-radius: 12px;
        padding: 16px;
      }
      .radio { margin: 4px 0 !important; }
      .radio label { font-size: 13.5px !important; font-weight: 500 !important; color: #4a6080 !important; padding-left: 26px !important; line-height: 1.7; }
      .radio label:hover { color: #1e2a3a !important; }
      input[type='radio'] { accent-color: #4b8ff5; }

      /* buttons */
      .btn-primary {
        background: linear-gradient(135deg, #4b8ff5, #3a7de8) !important;
        border: none !important;
        border-radius: 10px !important;
        font-family: 'Plus Jakarta Sans', sans-serif !important;
        font-size: 14px !important;
        font-weight: 700 !important;
        color: #fff !important;
        padding: 12px 18px !important;
        box-shadow: 0 4px 14px rgba(75,143,245,0.35) !important;
        transition: opacity 0.15s, box-shadow 0.15s, transform 0.1s !important;
      }
      .btn-primary:hover { opacity: 0.92 !important; transform: translateY(-1px) !important; color: #fff !important; }

      .btn-default {
        background: #fff !important;
        border: 1.5px solid #d8e6f8 !important;
        border-radius: 10px !important;
        font-family: 'Plus Jakarta Sans', sans-serif !important;
        font-size: 13.5px !important;
        font-weight: 600 !important;
        color: #6b8bb0 !important;
        padding: 11px 18px !important;
        transition: border-color 0.15s, color 0.15s !important;
      }
      .btn-default:hover { border-color: #4b8ff5 !important; color: #3a7de8 !important; background: #f5f9ff !important; }

      /* slider */
      .control-label { font-size: 11px !important; font-weight: 700 !important; letter-spacing: 0.08em !important; text-transform: uppercase !important; color: #8fa3bf !important; }
      .irs--shiny .irs-bar { background: #4b8ff5 !important; border-color: #4b8ff5 !important; }
      .irs--shiny .irs-handle { background: #fff !important; border: 2px solid #4b8ff5 !important; box-shadow: 0 2px 8px rgba(75,143,245,0.3) !important; top: 22px !important; }
      .irs--shiny .irs-single, .irs--shiny .irs-from, .irs--shiny .irs-to { background: #4b8ff5 !important; font-family: 'DM Mono', monospace !important; font-size: 10px !important; border-radius: 5px !important; }
      .irs--shiny .irs-line { background: #e4edf8 !important; border-color: #e4edf8 !important; border-radius: 3px !important; }
      .irs--shiny .irs-min, .irs--shiny .irs-max { font-family: 'DM Mono', monospace !important; font-size: 9px !important; color: #a0b4cc !important; background: transparent !important; }

      /* text input */
      .form-group { margin-bottom: 0 !important; }
      .form-control {
        border: 1.5px solid #d8e6f8 !important;
        border-radius: 10px !important;
        font-family: 'DM Mono', monospace !important;
        font-size: 13px !important;
        padding: 10px 14px !important;
        height: auto !important;
        transition: border-color 0.15s, box-shadow 0.15s !important;
      }
      .form-control:focus { border-color: #4b8ff5 !important; box-shadow: 0 0 0 3px rgba(75,143,245,0.12) !important; outline: none !important; }
      .form-control::placeholder { color: #c0d0e4 !important; }

      /* status */
      .status-card { background: #f0f6ff; border: 1px solid #d4e6fa; border-radius: 10px; padding: 12px 14px; min-height: 44px; }
      #status_text { font-family: 'DM Mono', monospace; font-size: 11.5px; color: #5a7aa0; line-height: 1.65; display: block; }

      .help-block { font-family: 'DM Mono', monospace !important; font-size: 10.5px !important; color: #a0b8d0 !important; }

      /* legend */
      .legend-row { display: flex; align-items: center; gap: 10px; font-size: 13px; font-weight: 500; color: #4a6080; margin-bottom: 9px; }
      .legend-dot { width: 11px; height: 11px; border-radius: 50%; box-shadow: 0 1px 4px rgba(0,0,0,0.15); flex-shrink: 0; }
      .legend-ring { width: 11px; height: 11px; border-radius: 50%; border: 2.5px solid; background: transparent; flex-shrink: 0; }

      /* leaflet overrides */
      .leaflet-popup-content-wrapper { border-radius: 12px !important; box-shadow: 0 8px 30px rgba(30,60,120,0.15) !important; font-family: 'DM Mono', monospace !important; font-size: 12px !important; border: 1px solid #e4edf8 !important; }
      .leaflet-popup-content { margin: 13px 16px !important; line-height: 1.9 !important; color: #4a6080 !important; }
      .leaflet-popup-content b { color: #1e2a3a; font-weight: 500; }
      .leaflet-popup-tip-container { display: none; }
      .leaflet-control-zoom a { border-radius: 8px !important; border-color: #d8e6f8 !important; color: #6b8bb0 !important; font-weight: 700 !important; }
      .leaflet-control-zoom a:hover { background: #f0f6ff !important; color: #4b8ff5 !important; }
      .leaflet-bar { border: none !important; }
      .leaflet-control-attribution { background: rgba(255,255,255,0.85) !important; color: #a0b8d0 !important; font-size: 9px !important; }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      tags$div(class = "app-header",
               tags$p(class = "app-title", "Warsaw Traffic"),
               tags$p(class = "app-subtitle", "Disruption simulator")
      ),
      
      tags$div(class = "sidebar-inner",
               
               h4("Load vehicles"),
               actionButton("fetch_btn", "Fetch vehicles", icon = icon("rotate-right"), width = "100%", class = "btn-primary"),
               
               hr(),
               
               h4("Place disruption"),
               tags$div(class = "radio-card",
                        radioButtons("disruption_type", label = NULL,
                                     choices = c(
                                       "Road traffic - buses only" = "traffic",
                                       "Track blockage - trams only" = "track_blockage",
                                       "Both - all vehicles" = "both"
                                     ),
                                     selected = "traffic"
                        )
               ),
               br(),
               sliderInput("radius_m", label = "Disruption radius (metres)", min = 5, max = 500, value = 25, step = 5, post = " m"),
               helpText(icon("circle-info"), "\u00a0Click anywhere on the map to drop a disruption pin."),
               br(),
               actionButton("clear_btn", "Clear disruptions", icon = icon("xmark"), width = "100%"),
               
               hr(),
               
               h4("Status"),
               tags$div(class = "status-card", textOutput("status_text")),
               
               hr(),
               
               h4("Legend"),
               tags$div(class = "legend-row", tags$div(class = "legend-dot", style = "background: #2ecc71;"), "Bus - on time"),
               tags$div(class = "legend-row", tags$div(class = "legend-dot", style = "background: #f39c12;"), "Bus - delayed"),
               tags$div(class = "legend-row", tags$div(class = "legend-ring", style = "border-color: #2ecc71;"), "Tram - on time"),
               tags$div(class = "legend-row", tags$div(class = "legend-ring", style = "border-color: #e74c3c;"), "Tram - blocked"),
               
               hr(),
               
               h4("Route geometry"),
               textInput("gtfs_shape", label = NULL, placeholder = "Enter line number, e.g. 15"),
               tags$div(
                 style = "display: flex; gap: 16px; font-family: 'DM Mono', monospace; font-size: 10.5px; color: #a0b8d0; margin-top: 8px;",
                 tags$span(tags$b(style = "color: #2980b9;", "-"), " Direction A"),
                 tags$span(tags$b(style = "color: #E05236;", "-"), " Direction B")
               )
      )
    ),
    
    mainPanel(
      width = 8,
      leaflet::leafletOutput("map", height = "100vh")
    )
  )
)
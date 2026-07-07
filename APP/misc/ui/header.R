# Header principal ----
header <- bs4DashNavbar(
  status         = "white",
  border         = FALSE,
  sidebarIcon    = icon("bars"),
  controlbarIcon = icon("gears"),
  title = dashboardBrand(
    title = tit_app,
    href  = "https://analitica.racafe.com/PortalAnalitica/",
    image = "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/img/logo2.png"
  ),
  leftUi = tagList(
    tags$li(
      class = "dropdown",
      style = "display:flex;align-items:center;gap:10px;padding:8px 12px;cursor:default;",
      ## Nombre del usuario autenticado
      tags$span(uiOutput("user")),
      ## Separador visual
      tags$span(style = "color:#ddd;font-size:0.85rem;", "|"),
      ## Sucursal activa derivada del grupo del usuario
      uiOutput("sucursal_label")
    )
  )
)
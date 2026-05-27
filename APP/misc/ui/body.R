body <- bs4DashBody(
  includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
  use_waiter(),
  useShinyjs(),
  bs4TabItems(
    bs4TabItem(tabName = "ResumenGeneral"),
    bs4TabItem(tabName = "OfertasTabla"),
    bs4TabItem(tabName = "FacturacionTabla"),
    bs4TabItem(tabName = "EntradasTabla"),
    bs4TabItem(tabName = "ComunicacionesTabla")
    )
  )

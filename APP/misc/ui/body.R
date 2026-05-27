body <- bs4DashBody(
  includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
  use_waiter(),
  useShinyjs(),
  bs4TabItems(
    ##### Resumen ----
    bs4TabItem(tabName = "tab_resumen", mod_resumen_ui("resumen")),
    ##### Ofertas ----
    bs4TabItem(tabName = "tab_ofertas", mod_ofertas_ui("ofertas")),
    ##### Facturación ----
    bs4TabItem(tabName = "tab_facturacion", mod_facturacion_ui("facturacion")),
    ##### Entradas ----
    bs4TabItem(tabName = "tab_entradas", mod_entradas_ui("entradas"))
    )
  )

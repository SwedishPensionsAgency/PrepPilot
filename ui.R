## UI spec
# Scalability:
# - Scales to smaller screens/devices
# 
# Layout:
# - Top navbar
# - Horizontal margins: two 2/12 columns on each side of the UI (empty)
# 

navbarPage(
  title = "Premiepensionsportalen",
  id = "prepPage",
  
  # Add header & footer here
  header = NULL,
  footer = NULL,
  
  ## MAIN CONTENT ----
  # Start page
  tabPanel(
    title = img(src="img/PM_logo.jpg", width = 35, style = "padding: 0px 0px"),
    pages[['start']]
  ),
  
  # Reports
  navbarMenu(
    title = "Reports",
    id = "reportPanels",
    tabPanel(title = "Report 1", pages[['start']])
  ),
  
  # Meta
  navbarMenu(
    title = "Meta",
    id = "metaPanels",
    tabPanel(title = "Meta 1", pages[['start']])
  )
)
# Page title
.title <- "start"

# Page content
.tags <- list(
  fluidPage(
    fluidRow(
      column(6, offset = 3, img(src="img/PM_logo.jpg"))
    ),
    fluidRow(
      column(
        10, offset = 1,
        h2("Premiepensionsportalen", style = "text-align: center;"),
        p("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
      )
    )
  )
)

# Output
.page <- do.call(tagList, .tags)
pages[[.title]] <- .page
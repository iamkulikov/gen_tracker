recipePackPanelUi <- function() {
  shiny::tags$details(
    class = "gt-recipe-pack-details",
    shiny::tags$summary("Recipe pack"),
    shiny::tags$p(
      class = "gt-recipe-pack-hint",
      "Export copies all active query lines and chart settings into one code. Import restores them above."
    ),
    shiny::textAreaInput(
      "recipe_pack_code",
      label = NULL,
      value = "",
      placeholder = "GENPACK1:metric=count;year_min=1979;year_max=2025;show_projection=TRUE||GEN2:...",
      rows = 4,
      width = "100%"
    ),
    shiny::div(
      class = "gt-recipe-pack-actions",
      shiny::actionButton(
        "export_recipe_pack",
        "Export pack",
        class = "btn-default gt-recipe-pack-btn"
      ),
      shiny::actionButton(
        "import_recipe_pack",
        "Import pack",
        class = "btn-primary gt-recipe-pack-btn"
      )
    ),
    shiny::textOutput("recipe_pack_status")
  )
}

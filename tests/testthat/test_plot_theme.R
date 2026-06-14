test_that("shareAxisLabelFormatter uses more decimals for narrow ranges", {
  wide <- shareAxisLabelFormatter(c(0.10, 0.80))
  narrow <- shareAxisLabelFormatter(c(0.051, 0.053))

  expect_equal(wide(0.5), "50%")
  expect_match(narrow(0.052), "\\.")
})

test_that("countAxisLabelFormatter avoids decimals for integer counts", {
  fmt <- countAxisLabelFormatter(c(1000, 5000, 9000))
  expect_equal(fmt(1234.4), "1,234")
})

test_that("formatMetricLabel names count axis in thousands", {
  expect_equal(formatMetricLabel("count"), "People, thousands")
  expect_equal(formatMetricLabel("share_total_population"), "Share of total population")
  expect_equal(
    formatMetricLabel("share_working_age_population"),
    "Share of working-age population"
  )
})

test_that("isShareMetric recognizes share metrics", {
  expect_true(isShareMetric("share_total_population"))
  expect_true(isShareMetric("share_working_age_population"))
  expect_false(isShareMetric("count"))
})

test_that("shareScalePinsToUnity when series reaches full share", {
  expect_false(shareScalePinsToUnity(c(0.1, 0.5, 0.8)))
  expect_true(shareScalePinsToUnity(c(0.9, 0.95, 0.99)))
  expect_true(shareScalePinsToUnity(1))
})

test_that("shareAxisLabelFormatter shows 100% at unity", {
  fmt <- shareAxisLabelFormatter(c(0.9, 1))
  expect_equal(fmt(1), "100%")
})

test_that("buildTrackerPlot pins share axis to 100% when data reaches unity", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe(metric = "share_total_population")

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )
  plot_data$value <- seq(0.85, 1, length.out = nrow(plot_data))
  expect_true(shareScalePinsToUnity(plot_data$value))

  plot_obj <- buildTrackerPlot(
    plot_data = plot_data,
    events = events,
    metric = "share_total_population"
  )
  built <- ggplot2::ggplot_build(plot_obj)
  y_view <- built$layout$panel_params[[1]]$y
  expect_equal(y_view$limits, c(0, 1))
  expect_equal(max(y_view$breaks), 1)
  y_labels <- y_view$get_labels(y_view$breaks)
  expect_true("100%" %in% y_labels)
  expect_equal(built$plot$labels$y, "Share of total population")
})

test_that("buildTrackerPlot uses percent scale for working-age share metric", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe(metric = "share_working_age_population")

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  plot_obj <- buildTrackerPlot(
    plot_data = plot_data,
    events = events,
    metric = "share_working_age_population"
  )
  built <- ggplot2::ggplot_build(plot_obj)
  y_view <- built$layout$panel_params[[1]]$y
  y_labels <- y_view$get_labels(y_view$breaks)
  expect_true(any(grepl("%", y_labels)))
  expect_equal(built$plot$labels$y, "Share of working-age population")
})

test_that("buildTrackerPlot uses thousands label for count metric", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe(metric = "count")

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  plot_obj <- buildTrackerPlot(plot_data = plot_data, events = events, metric = "count")
  built <- ggplot2::ggplot_build(plot_obj)
  expect_equal(built$plot$labels$y, "People, thousands")
})

test_that("queryLineColors follow query slot index not display order", {
  palette <- trackerPlotPalette(4L)
  expect_equal(queryLineColors(c("q1", "q3")), palette[c(1L, 3L)])
  expect_equal(queryLineColors("q2"), palette[2L])
})

test_that("trackerPlotPalette uses colorblind-friendly gold for query slot 4", {
  palette <- trackerPlotPalette(4L)
  expect_equal(palette[[4]], "#9a6700")
})

test_that("assignMarkerLabelLanes keeps one level when zones and short labels do not overlap", {
  markers <- tibble::tibble(
    query_id = c("q1", "q2"),
    event_name = c("War A", "Crisis B"),
    start_year = c(1979L, 2001L),
    end_year = c(1989L, 2001L),
    is_single_year = c(FALSE, TRUE),
    label_x = c(1984, 2001),
    label_vjust = c(1.15, 1.15)
  )

  positioned <- assignMarkerLabelLanes(markers, year_span = 110)

  expect_equal(unique(positioned$label_vjust), 1.15)
})

test_that("assignMarkerLabelLanes separates long labels when text extends into another zone", {
  markers <- tibble::tibble(
    query_id = c("q1", "q2"),
    event_name = c(
      "Soviet-Afghan War",
      "High inflation (>20%) in Russian Federation (8 episodes)"
    ),
    start_year = c(1979L, 1995L),
    end_year = c(1989L, 2005L),
    is_single_year = c(FALSE, FALSE),
    label_x = c(1984, 2000),
    label_vjust = c(1.15, 1.15)
  )

  positioned <- assignMarkerLabelLanes(markers, year_span = 110) |>
    dplyr::arrange(.data$query_id)

  expect_equal(positioned$label_vjust[[1]], 1.15)
  expect_equal(positioned$label_vjust[[2]], 1.15 + 1.35)
})

test_that("markerZoneCenter matches shaded geom_rect span", {
  expect_equal(markerZoneCenter(1979L, 1989L, FALSE), 1984.5)
  expect_equal(markerZoneCenter(2001L, 2001L, TRUE), 2001.5)
})

test_that("assignMarkerLabelLanes separates labels when shading zones overlap", {
  markers <- tibble::tibble(
    query_id = c("q1", "q2", "q3"),
    event_name = c("Event A", "Event B", "Event C"),
    start_year = c(1979L, 1984L, 1990L),
    end_year = c(1989L, 1994L, 2000L),
    is_single_year = c(FALSE, FALSE, FALSE),
    label_x = c(1984, 1989, 1995),
    label_vjust = rep(1.15, 3)
  )

  positioned <- assignMarkerLabelLanes(markers)

  expect_equal(positioned$label_vjust[[1]], 1.15)
  expect_true(positioned$label_vjust[[2]] > positioned$label_vjust[[1]])
  expect_true(positioned$label_vjust[[3]] > positioned$label_vjust[[2]])
  expect_equal(positioned$label_vjust[[2]] - positioned$label_vjust[[1]], 1.35)
})

test_that("buildPlotEventMarkers distinguishes single- and multi-year events", {
  events <- dplyr::bind_rows(buildTestEvents(), buildTestGlobalEvent())
  plot_data <- tibble::tibble(
    query_id = c("q1", "q2"),
    event_id = c("AFG_WAR", "USA_NATIONAL_EVENT"),
    event_name = c("War in Afghanistan", "USA national event"),
    event_mode = c("start", "start")
  )

  markers <- buildPlotEventMarkers(plot_data, events)

  expect_equal(nrow(markers), 2)
  expect_false(markers$is_single_year[markers$event_id == "AFG_WAR"])
  expect_true(markers$is_single_year[markers$event_id == "USA_NATIONAL_EVENT"])
  expect_equal(markers$event_name, c("War in Afghanistan", "USA national event"))
  expect_true(all(!grepl("\\d{4}", markers$event_name)))
})

test_that("buildTrackerPlot adds rect layer for multi-year events", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe()

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  plot_obj <- buildTrackerPlot(plot_data = plot_data, events = events, metric = "count")
  built <- ggplot2::ggplot_build(plot_obj)
  layer_classes <- vapply(built$plot$layers, function(layer) class(layer$geom)[1], character(1))

  expect_true("GeomRect" %in% layer_classes)
  expect_true("GeomLine" %in% layer_classes)
  expect_true("GeomText" %in% layer_classes)
})

test_that("buildTrackerPlot adds rect layer for single-year events", {
  population <- dplyr::bind_rows(
    buildTestPopulation(),
    buildTestPopulation() |>
      dplyr::mutate(country_id = "USA", country_name = "United States")
  )
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe(event_id = "USA_NATIONAL_EVENT", country_id = "USA")

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  plot_obj <- buildTrackerPlot(plot_data = plot_data, events = events, metric = "count")
  built <- ggplot2::ggplot_build(plot_obj)
  layer_classes <- vapply(built$plot$layers, function(layer) class(layer$geom)[1], character(1))

  expect_true("GeomRect" %in% layer_classes)
  expect_false("GeomVline" %in% layer_classes)
})

test_that("buildTrackerPlot handles mixed single- and multi-year markers", {
  population <- dplyr::bind_rows(
    buildTestPopulation(),
    buildTestPopulation() |>
      dplyr::mutate(country_id = "USA", country_name = "United States")
  )
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()

  recipes <- tibble::tribble(
    ~query_id, ~country_id, ~sex, ~age_status_id, ~is_complement, ~custom_age_min, ~custom_age_max,
    ~event_id, ~event_mode, ~metric,
    "q1", "RUS", "male", "school_age", FALSE, NA_integer_, NA_integer_, "AFG_WAR", "start", "count",
    "q2", "USA", "all", "adults", FALSE, NA_integer_, NA_integer_, "USA_NATIONAL_EVENT", "start", "count"
  )

  plot_data <- buildPlotData(
    recipes = recipes,
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  plot_obj <- buildTrackerPlot(plot_data = plot_data, events = events, metric = "count")
  built <- ggplot2::ggplot_build(plot_obj)
  layer_classes <- vapply(built$plot$layers, function(layer) class(layer$geom)[1], character(1))

  expect_true(sum(layer_classes == "GeomRect") >= 2)
  expect_false("GeomVline" %in% layer_classes)
})

test_that("buildTrackerPlot omits duplicate title and chart caption", {
  population <- buildTestPopulation()
  events <- buildTestEvents()
  countries <- buildTestCountries()
  age_groups <- defaultAgeGroups()
  recipe <- buildTestRecipe()

  plot_data <- buildPlotData(
    recipes = tibble::as_tibble(recipe),
    population = population,
    events = events,
    age_groups = age_groups,
    countries = countries,
    event_countries = buildTestEventCountries()
  )

  plot_obj <- buildTrackerPlot(
    plot_data = plot_data,
    events = events,
    metric = "count",
    title = NULL,
    subtitle = "includes projection · solid = observed · dashed = projected"
  )

  built <- ggplot2::ggplot_build(plot_obj)
  expect_null(built$plot$labels$title)
  expect_null(built$plot$labels$caption)
  expect_null(built$plot$labels$x)
})

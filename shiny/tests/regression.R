if (.Platform$OS.type == 'windows') Sys.setlocale('LC_CTYPE', 'English_United States.utf8')
# Run from the application directory: Rscript tests/regression.R
source('global.R', encoding='UTF-8')
for (f in list.files('R', full.names=TRUE, pattern='[.]R$')) source(f, encoding='UTF-8')
stopifnot(!'geometry' %in% names(df_master), any(!is.na(df_master$iso_a2)))
stopifnot(nrow(count(df_master, ISOCountry, AGEGR, metric) |> filter(n != 1)) == 0)
cat('PASS: country joins and unique data keys\n')
render_gt <- function(expr) { captured_render <<- substitute(expr); NULL }
downloadHandler <- function(filename, content, ...) { captured_download <<- content; NULL }
shiny::testServer(init_table2_server, {
  ages <- c('0-<6m','6-<12m','0-<12m','12-<60m','0-<60m')
  for (metric in unique(df_master$metric)) {
    session$setInputs(table2_metric = metric)
    raw_df <- table2_matrix_data(); rendered <- eval(captured_render, envir = new.env(parent = environment()))
    for (i in seq_along(ages)) {
      stopifnot(identical(rendered[['_transforms']][[2*i-1]]$fn(NULL), raw_df[[paste0('R_ci_', ages[i])]]))
      stopifnot(identical(rendered[['_transforms']][[2*i]]$fn(NULL), raw_df[[paste0('N_ci_', ages[i])]]))
      expected <- df_master |> filter(metric == .env$metric, AGEGR == ages[i])
      stopifnot(isTRUE(all.equal(raw_df[[paste0('N_', ages[i])]],
        expected$N[match(raw_df$ISOCountry, expected$ISOCountry)])))
    }
    stopifnot(nchar(gt::as_raw_html(rendered)) > 0)
    csv <- tempfile(fileext='.csv'); captured_download(csv)
    exported <- readr::read_csv(csv, show_col_types=FALSE)
    stopifnot(identical(exported$ISOCountry, raw_df$ISOCountry))
    for (column in names(raw_df)[grepl('^[RN]_', names(raw_df))]) {
      stopifnot(isTRUE(all.equal(exported[[column]], unname(raw_df[[column]]))))
    }
    stopifnot(!'Country' %in% names(exported)); unlink(csv)
    cat('PASS: metric table and CSV:', metric, '\n')
  }
})
shiny::testServer(init_table1_server, {
  for (age in unique(df_master$AGEGR)) {
    session$setInputs(table_age = age, select_age = 'different-map-age')
    raw_df <- table_matrix_data(); rendered <- eval(captured_render, envir = new.env(parent = environment()))
    for (i in 1:4) {
      stopifnot(identical(rendered[['_transforms']][[i]]$fn(NULL), raw_df[[paste0('txt',i,'_R')]]))
      stopifnot(identical(rendered[['_transforms']][[i+4]]$fn(NULL), raw_df[[paste0('txt',i,'_N')]]))
    }
    stopifnot(nchar(gt::as_raw_html(rendered)) > 0)
    csv <- tempfile(fileext='.csv'); captured_download(csv)
    exported <- readr::read_csv(csv, show_col_types=FALSE)
    expected <- all_metrics_broad_data()
    stopifnot(identical(exported$ISOCountry, expected$ISOCountry))
    for (column in setdiff(names(expected), 'ISOCountry')) {
      stopifnot(isTRUE(all.equal(exported[[column]], unname(expected[[column]]))))
    }
    incidence <- df_master |> filter(metric == 'RSV-associated ALRI incidence', AGEGR == age)
    stopifnot(isTRUE(all.equal(exported$inc_R,
      unname(round(incidence$R[match(exported$ISOCountry, incidence$ISOCountry)], 1)))))
    unlink(csv)
    cat('PASS: age table and CSV:', age, '\n')
  }
})
updateSelectizeInput <- function(session, inputId, ..., selected=NULL) { last_selection <<- selected }
shiny::testServer(init_composition_server, {
  session$setInputs(comp_metrics=unique(df_master$metric), comp_ages='0-<6m', comp_countries=unname(country_choices), comp_preset='none', comp_top_n_slider=10, comp_is_percent=TRUE)
  for (preset in c('Afr','Amr','Emr','Eur','Sear','Wpr','L','LM','UM','all')) {
    session$setInputs(comp_preset=preset)
    expected <- df_master |> filter(AGEGR=='0-<6m', !is.na(N))
    if (preset %in% c('L','LM','UM')) expected <- filter(expected, Income2019==preset)
    else if (preset != 'all') expected <- filter(expected, WHORegion==preset)
    stopifnot(length(last_selection)>0, setequal(last_selection, expected$ISOCountry))
  }
  totals <- composition_filtered_data() |> group_by(burden_segment) |> summarise(pct=sum(pct))
  stopifnot(all(abs(totals$pct-100)<1e-8))
  cat('PASS: region/income presets and composition percentages\n')
})
shiny::testServer(init_map_server, {
  for (metric in unique(df_master$metric)) for (age in unique(df_master$AGEGR)) {
    session$setInputs(select_metric=metric, select_age=age, select_type='R',show_floating_card=FALSE)
    mapped <- filtered_map_data()
    stopifnot(sum(!is.na(mapped$value_to_plot)) > 0, nrow(mapped)==nrow(world_sf))
  }
  cat('PASS: all 20 map metric/age combinations\n')
})
source('ui.R', encoding='UTF-8'); source('server.R', encoding='UTF-8')
stopifnot(nchar(htmltools::renderTags(ui)$html)>0)
for (f in c('global.R','ui.R','server.R',list.files('R', full.names=TRUE, pattern='[.]R$'))) invisible(parse(f))
cat('PASS: UI builds and all active R files parse\n')








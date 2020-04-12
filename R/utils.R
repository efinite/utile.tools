utils::globalVariables(c(
  ':=', 'variable', 'level', 'statistic',
  'subjects', 'events', 'estimate', 'test',
  'std.error', 'conf.low', 'conf.high', 'p'
))

# Tidy model assignments
.tidy_assigns <- function (x, y) purrr::set_names(purrr::imap(x, ~ which(.y - 1 == y)), x)

# Tidy model coefficient table
.tidy_table <- function (table, cols, level, func = function(x) x, test = NA) {

  # Convert to tibble
  table <- tibble::as_tibble(table, rownames = 'variable')[, c(1, cols + 1)]

  # Set column names
  colnames(table) <- c('variable', 'estimate', 'std.error', 'statistic', 'p')

  # Calculate dist values
  level <- (1 - level)/2
  levels <- stats::qnorm(c(level, 1 - level))

  # Create estimates
  dplyr::mutate(
    table,
    test = test,
    estimate = func(estimate),
    conf.low = func(estimate + std.error*levels[1]),
    conf.high = func(estimate + std.error*levels[2])
  )

}

# Tidy model test results
.tidy_tests <- function (x, y, z = NA) {

  # Convert to tibble
  x <- tibble::as_tibble(x, rownames = 'variable')[, c(1, stats::na.omit(y) + 1)]

  # Set column names
  colnames(x) <- c('variable', c('statistic', 'p')[!is.na(y)])

  # Remove intercept and add test column
  dplyr::mutate(
    dplyr::filter(x, !(variable %in% c('<none>', '(Intercept)'))),
    test = z
  )

}

# Format prepared coefficient table
.format_table <- function(table, estimate, level, digits, p.digits) {

  # Identify reference rows
  has_levels <- 'level' %in% names(table)
  if (has_levels) ref_rows <- !is.na(table$level) & is.na(table$estimate)

  # Transmute table
  res <-
    dplyr::bind_cols(

      # Combine & format variables/levels
      Variable = purrr::map2_chr(
        table$variable, if (has_levels) table$level else NA,
        ~ if (!is.na(.y)) {
          if (nrow(table[table$variable == .x,]) == 1) paste(.x, .y, sep = ', ')
          else paste0('   ', .y)
        } else .x
      ),

      if (length(intersect(c('subjects', 'events'), names(table))) == 2) {
        list(
          n = as.character(table$subjects),
          Events = as.character(table$events)
        )
      },

      # Round & rename estimates
      !!estimate := as.character(
        dplyr::if_else(
          !is.na(table$estimate),
          round(table$estimate, digits = digits),
          as.double(NA)
        )
      ),

      # Round and consolidate CI
      !!paste0('[', level*100,'%CI]') := dplyr::if_else(
        !is.na(table$conf.low) & !is.na(table$conf.high),
        paste0('[', round(table$conf.low, digits = digits), '-', round(table$conf.high, digits = digits), ']'),
        as.character(NA)
      ),

      # Copy tests
      Test = table$test,

      # Round statistics
      Statistic = as.character(round(table$statistic, digits = digits)),

      # Format p-value
      p = format.pval(pv = table$p, digits = digits, eps = 0.0001, nsmall = p.digits, scientific = F, na.form = '')
    )

  # Set reference row placeholders
  if (has_levels) res[ref_rows, estimate] <- 'Reference'

  # Replace NA's and return
  replace(res, is.na(res), '')

}


utils::globalVariables(c(':=', 'variable', 'level', 'subjects', 'events', 'estimate', 'conf.lower', 'conf.upper', 'p'))

.format_table <- function(table, estimate, percent.sign, digits, p.digits) {

  # Identify reference rows
  ref_rows <- !is.na(table$level) & is.na(table$estimate)
  estimate <- paste(estimate, '[95%CI]')

  # Transmute table
  table <-
    dplyr::mutate_all(
      dplyr::bind_cols(
        Variable = purrr::map2_chr(
          table$variable, table$level,
          function(x, y)
            if (!is.na(y)) {
              if (nrow(table[table$variable == x,]) == 1) paste(x, y, sep = ', ')
              else paste0('   ', y)
            } else x
        ),
        if (length(intersect(c('subjects', 'events'), names(table))) == 2) {
          tibble::tibble(
            Subjects = table$subjects,
            Events = paste_freq(
              count = table$events,
              total = table$subjects,
              percent.sign = percent.sign,
              digits = digits
            )
          )
        },
        !!estimate := ifelse(
          !is.na(table$estimate),
          paste(
            round(table$estimate, digits = digits),
            ifelse(
              !is.na(table$conf.lower) & !is.na(table$conf.upper),
              paste0('[', round(table$conf.lower, digits = digits), '-', round(table$conf.upper, digits = digits), ']'),
              ''
            )
          ),
          NA
        ),
        p = ifelse(
          !is.na(table$p),
          format.pval(pv = table$p, digits = digits, eps = 0.0001, nsmall = p.digits, scientific = F),
          NA
        )
      ),
      as.character
    )

  # Set reference row placeholders
  table[ref_rows, estimate] <- 'Reference'

  # Replace NA's and return
  replace(table, is.na(table), '')
}

#' Build labels with significances represented in bold or as raised 'a', and, if wanted, standard errors in brackets. Main usage for plotting tables and brace labels.
#'
#' @inheritParams plot_lineplot
#' @param dat Dataframe with the columns that should be merged into labels.
#' @param new_name Character string for the new column that is added to `dat`. Defaults to `'label'`.
#' @param round_est Rounding of label_est.
#' @param round_se Rounding of label_se.
#'
#' @return The data.frame with an added column for the constructed label.
#' @export
#'
#' @examples #tbd
construct_label <- function(dat,
                            new_name = "label",
                            label_est = NULL,
                            label_se = NULL,
                            label_sig_bold = NULL,
                            label_sig_high = NULL,
                            round_est = 0,
                            round_se = 1,
                            plot_settings = plotsettings_tablebarplot()) {

  dat <- fill_column(dat, column_name = label_est, filling = "")
  dat <- fill_column(dat, column_name = label_se, filling = NA)
  dat <- fill_column(dat, column_name = label_sig_high, filling = FALSE)
  dat <- fill_column(dat, column_name = label_sig_bold, filling = FALSE)


if(any(is.na(dat[, c("label_sig_high", "label_sig_bold")]))){
  for (i in c("label_sig_high", "label_sig_bold")) {
    dat[is.na(dat[, i]), i] <- FALSE
  }
}

  if (is.numeric(dat$label_est) & !is.null(round_est)) {
    label_est_num <- TRUE
    dat$label_est <- format(round(dat$label_est, round_est), trim = TRUE)
  }else{
    label_est_num <- FALSE
  }
  if (is.numeric(dat$label_se) & !is.null(round_se)) {
    dat$label_se <- format(round(dat$label_se, round_se), trim = TRUE)
  }

  dat$label_est <- ifelse(dat$label_sig_bold == TRUE,
    paste0("**", dat$label_est, "**"),
    dat$label_est
  )

  ## Later on, the number of letters in a string has to be counted for aligning the decimal points.
  # Therefore, use a letter not used in other expression, which can be later subbed with the correct letter and the html expression for raising it.
  dat$label_sig <- ifelse(dat$label_sig_high == TRUE, "#", "")

  dat$label_se <- ifelse(!is.na(dat$label_se),
    paste0(" (", dat$label_se, ")"),
    ""
  )

  dat[, c("label_est", "label_sig", "label_se")][is.na(dat[, c("label_est", "label_sig", "label_se")])] <- ""

  dat[, new_name] <- paste0(
    dat$label_est,
    dat$label_sig,
    dat$label_se
  )
#if(label_est == "est_Trend_noComp_20112016_percent"){browser()}

  ## Numeric values should be aligned by the decimal point.
  if(label_est_num == TRUE){
  dat[,  new_name] <- align_by_dec(dat[, new_name], plot_settings = plot_settings)
  dat[, new_name] <- gsub("#", paste0("<sup>", plot_settings$columns_table_sig_high_letter, "</sup>"), dat[, new_name])
  }

  dat <- remove_columns(dat, cols = c("label_est", "label_sig", "label_se", "label_sig_bold", "label_sig_high"))

  return(dat)
}


# left_align: vor dec gleiche Länge
# right align: after dec gleiche Länge
# mid align: vor und nach dec gleiche Länge

## sup" "

align_by_dec <- function(vec, dec = "\\.", plot_settings = plotestting_tablebarplot()){

  vec <- gsub(dec, "\\.", vec)

before_dec <- sub("\\..*", "", vec)
after_dec <- sub(".*\\.", "", vec)

before_dec_subbed <- gsub("\\*\\*", "", before_dec)
after_dec_subbed <- gsub("\\*\\*", "", after_dec)


length_before_dec <- sapply(before_dec_subbed, function(x){
  nchar(x)
  })


length_after_dec <- sapply(after_dec_subbed, function(x){
  nchar(x)
})

max_before <- max(length_before_dec, na.rm = TRUE)
max_after <- max(length_after_dec, na.rm = TRUE)


filled_before <- sapply(seq_along(before_dec), function(x){
  background_colour <- plot_settings$background_stripes_colour[x]
  background_colour <- "red"
  n_fill <- max_before - nchar(before_dec_subbed[x])
  filler <- paste0(" <span style='color:", background_colour, "'>", rep("..", n_fill), "</span>")

  out <- paste0(filler, before_dec[x])
})

filled_after <- sapply(seq_along(after_dec), function(x){
  background_colour <- plot_settings$background_stripes_colour[x]
  background_colour <- "red"
  n_fill <- max_after - nchar(after_dec_subbed[x])
  filler <- paste0(" <span style='color:", background_colour, "'>", rep(".", n_fill), "</span>")
  out <- paste0(".", after_dec[x], filler)
})

#res_vec <-  paste0(" <span style='color:red'>..</span>", "2.2 <span style='color:red'>..</span>") #paste0(filled_before, filled_after) # sep = "."
res_vec <-  paste0(filled_before, filled_after) #paste0(filled_before, filled_after) # sep = "."

return(res_vec)
}

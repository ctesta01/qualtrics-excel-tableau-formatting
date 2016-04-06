question_to_panel_columns <- function(questionname, questiontext, df) {
  paneldf <- df[df$DataExportTag == questionname,
  c("ResponseID", "OriginalResponse", "CodedResponse")]

  colnames(paneldf) <- c("ResponseID", paste("Orig", questiontext, sep=": "),
  paste("Coded", questiontext, sep=": "))
  return(paneldf)
}

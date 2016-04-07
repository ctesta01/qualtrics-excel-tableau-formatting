### question_to_panel_columns
# given a question's dataexporttag (as the variable questionname),
# and a lean_response table, (given as variable df),
# the question_to_panel_columns function returns a dataframe
# with three columns, the "ResponseID", "Orig: [question text here]",
# and "Coded: [question text here]"
question_to_panel_columns <- function(df, questionname) {
  # set the data frame to be the subset of the original data frame
  # that matches the questionname
  df <- df[df$DataExportTag == questionname, ]

  # check that the question name appears in the dataframe,
  # or in other words check that the dataframe restricted to the
  # specific question has a longer length than 0.
  if (nrow(df) == 0) {
    error <- "DataExportTag XX for does not appear in the lean response data.
    XX was going to be included in the panel data."
    error <- gsub("XX", questionname, error)
    stop(error)
  }

  # questiontext is retrieved by taking the QuestionText column from the
  # data frame and selecting the first element
  questiontext = toString(df[, "QuestionText"][[1]])

  # subset the data frame to only inclue the response ID and the
  # response columns
  df <- df[, c("ResponseID", "OriginalResponse", "CodedResponse")]

  # rename the columns so that the question text is included in the
  # panel data column headers
  orig_questiontext <- paste0("Orig: ", questiontext)
  coded_questiontext <- paste0("Coded: ", questiontext)
  colnames(df) <- c("ResponseID", orig_questiontext, coded_questiontext)

  return(df)
}

### base_panel_data_from_csv is a function that
# creates the initial data frame that panel data will be added to later
base_panel_data_from_csv <- function(df, columnlist) {
  # if the user doesn't define a columnlist,
  # set the default columnlist to include all the variable data columns
  # given by qualtrics that start with a "V"
  if (missing(columnlist)) {
    columnlist <- likely_panel_columns
  }
  df <- df[,columnlist]
  return(df)
}

### all_panel_questions takes a dataframe and a questionlist and creates
# a dataframe with the responses to the questions from the questionlist
# formatted as panel data
all_panel_questions <- function(df, questionlist) {
  panel_questions <- question_to_panel_columns(df, questionlist[[1]])
  for (i in 2:length(questionlist)) {
    panel_questions <- merge(x = panel_questions, y = question_to_panel_columns(df, questionlist[[i]]), by = "ResponseID", all = TRUE)
  }
  return(panel_questions)
}

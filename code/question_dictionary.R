Blocks <- Blocks[[1]]$Payload

entries = list()
e = 0

for (i in 1:length(Blocks)) {
  if (length(Blocks[[i]]$BlockElements) != 0) {
    for (j in 1:length(Blocks[[i]]$BlockElements)) {
      if (length(Blocks[[i]]$BlockElements[[j]]$Responses) != 0) {
        for (k in 1:length(Blocks[[i]]$BlockElements[[j]]$Responses)) {
          e = e + 1
          qtype = ""
          if (is_multiple_choice(Blocks[[i]]$BlockElements[[j]])) {
            qtype = "Check All"
          } else if (is_single_answer(Blocks[[i]]$BlockElements[[j]])) {
            qtype = "Single Answer"
          } else if (is_rank_order(Blocks[[i]]$BlockElements[[j]])) {
            qtype = "Rank Order"
          } else {
            qtype = ""
          }

          if (is.null(Blocks[[i]]$BlockElements[[j]]$Payload$SubSelector)) {
            subselector = ""
          } else {
            subselector = Blocks[[i]]$BlockElements[[j]]$Payload$SubSelector
          }

          entry <- c(
          # response column name
          colnames(Blocks[[i]]$BlockElements[[j]]$Responses)[[k]],
          # data export tag
          Blocks[[i]]$BlockElements[[j]]$Payload$DataExportTag,
          # question text
          Blocks[[i]]$BlockElements[[j]]$Payload$QuestionText,
          # block name
          Blocks[[i]]$Description,
          # qualtrics question type
          Blocks[[i]]$BlockElements[[j]]$Payload$QuestionType,
          # qualtrics question selector
          Blocks[[i]]$BlockElements[[j]]$Payload$Selector,
          # qualtrics question subselector
          subselector,
          # question column type
          qtype
          )
          entries[[e]] <- entry
        }
      }
    }
  }
}

list_of_rows_to_df <- function(data) {
  nCol <- max(vapply(data, length, 0))
  data <- lapply(data, function(row) c(row, rep(NA, nCol-length(row))))
  data <- matrix(unlist(data), nrow=length(data), ncol=nCol, byrow=TRUE)
  data.frame(data)
}

data_dictionary <- list_of_rows_to_df(entries)
colnames(data_dictionary) <- c("ResponseColumnName", "DataExportTag",
"QuestionText", "Block", "QualtricsQuestionType", "QuestionSelector",
"QuestionSubSelector", "QuestionType")

write.csv(data_dictionary, 'output/data_dictionary.csv', row.names=FALSE)

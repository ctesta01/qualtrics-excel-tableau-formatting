
entries = list()
e = 0

for (i in 1:length(Blocks)) {
  if (length(Blocks[[i]]$BlockElements) != 0) {
    for (j in 1:length(Blocks[[i]]$BlockElements)) {
      if (length(Blocks[[i]]$BlockElements[[j]]$Responses) != 0) {
        for (k in 1:length(Blocks[[i]]$BlockElements[[j]]$Responses)) {
          e = e + 1
          qtype = ""

          # make sure that subselector is defined
          if (is.null(Blocks[[i]]$BlockElements[[j]]$Payload$SubSelector)) {
            Blocks[[i]]$BlockElements[[j]]$Payload$SubSelector <- ""
          }

          # setting qtype
          if (is_multiple_choice(Blocks[[i]]$BlockElements[[j]])) {
            qtype = "Check All"
          } else if (is_single_answer(Blocks[[i]]$BlockElements[[j]])) {
            qtype = "Single Answer"
          } else if (is_rank_order(Blocks[[i]]$BlockElements[[j]])) {
            qtype = "Rank Order"
          } else if (is_text_entry(Blocks[[i]]$BlockElements[[j]])) {
            qtype = "Text Entry"
          } else {
            qtype = ""
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
          Blocks[[i]]$BlockElements[[j]]$Payload$SubSelector,
          # question column type
          qtype
          )
          entries[[e]] <- entry
        }
      }
    }
  }
}


data_dictionary <- list_of_rows_to_df(entries)
colnames(data_dictionary) <- c("ResponseColumnName", "DataExportTag",
"QuestionText", "Block", "QualtricsQuestionType", "QuestionSelector",
"QuestionSubSelector", "QuestionType")

write.csv(data_dictionary, 'output/data_dictionary.csv', row.names=FALSE)

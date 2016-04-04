
entries = list()
e = 0

for (i in 1:length(Blocks)) {
  if (length(Blocks[[i]]$BlockElements) != 0) {
    for (j in 1:length(Blocks[[i]]$BlockElements)) {
      if (length(Blocks[[i]]$BlockElements[[j]]$Responses) != 0) {
          for (l in 1:ncol(Blocks[[i]]$BlockElements[[j]]$Responses)) {
            for (m in 1:length(Blocks[[i]]$BlockElements[[j]]$Responses[[l]])) {

          e = e + 1
          r = Blocks[[i]]$BlockElements[[j]]$Responses[[l]][[m]]
          qtype = ""
          rvalue = ""
          mitem = ""

          # make sure that subselector is defined
          if (is.null(Blocks[[i]]$BlockElements[[j]]$Payload$SubSelector)) {
            Blocks[[i]]$BlockElements[[j]]$Payload$SubSelector <- ""
          }

          # setting the question type in human readable terms as
          if (is_multiple_choice(Blocks[[i]]$BlockElements[[j]])) {
            qtype = "Check All"
            if (is_matrix_question(Blocks[[i]]$BlockElements[[j]])) {
              manswers = sapply(Blocks[[i]]$BlockElements[[j]]$Payload$Answers,
                function (x) x$Display)
              mchoices = sapply(Blocks[[i]]$BlockElements[[j]]$Payload$Choices,
                function(x) x$Display)
              scalemod = length(manswers)
              if (l %% scalemod != 0) {
                scalepoint = l %% scalemod
              } else {
                scalepoint = scalemod
              }
              choicemod = length(mchoices)
              choice = ceiling(l/choicemod)
              rvalue = manswers[[scalepoint]]
              mitem = mchoices[[choice]]
            } else {
            rvalue = sapply(Blocks[[i]]$BlockElements[[j]]$Payload$Choices,
              function(x) x$Display)[[l]]
            }
          } else if (is_single_answer(Blocks[[i]]$BlockElements[[j]])) {
            qtype = "Single Answer"
            if (is_matrix_question(Blocks[[i]]$BlockElements[[j]])) {
              manswers = sapply(Blocks[[i]]$BlockElements[[j]]$Payload$Answers,
                function (x) x$Display)
              mchoices = sapply(Blocks[[i]]$BlockElements[[j]]$Payload$Choices,
                function(x) x$Display)
              scalemod = length(manswers)
              if (l %% scalemod != 0) {
                scalepoint = l %% scalemod
              } else {
                scalepoint = scalemod
              }
              choicemod = length(mchoices)
              choice = ceiling(l/choicemod)
              rvalue = manswers[[scalepoint]]
              mitem = mchoices[[choice]]
            } else {
            if (r == -99) {
              rvalue = -99
            } else {
              rvalue = sapply(Blocks[[i]]$BlockElements[[j]]$Payload$Choices,
              function (x) x$Display)[[r]]
            }
          }
          } else if (is_rank_order(Blocks[[i]]$BlockElements[[j]])) {
            qtype = "Rank Order"
            if (r == -99) {
              rvalue = -99
            } else {
              rvalue = sapply(Blocks[[i]]$BlockElements[[j]]$Payload$Choices,
              function (x) x$Display)[[r]]
            }
          } else if (is_text_entry(Blocks[[i]]$BlockElements[[j]])) {
            qtype = "Text Entry"
            rvalue = ""
          } else {
            qtype = ""
            rvalue = ""
          }


          entry <- c(
          # response id
          toString(responses['V1'][m,1]),
          # response column name
          colnames(Blocks[[i]]$BlockElements[[j]]$Responses)[[l]],
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
          qtype,
          # response symbolic value
          Blocks[[i]]$BlockElements[[j]]$Responses[[l]][[m]],
          # response human readable value
          rvalue,
          # matrix line item
          mitem
          )
          entries[[e]] <- entry
        }
        }
      }
    }
  }
}

lean_responses <- list_of_rows_to_df(entries)
colnames(lean_responses) <- c("ResponseID", "ResponseColumnName", "DataExportTag",
"QuestionText", "Block", "QualtricsQuestionType", "QuestionSelector",
"QuestionSubSelector", "QuestionType", "CodedResponse", "ResponseText", "MatrixLineItem")

write.csv(lean_responses, 'output/lean_responses.csv', row.names=FALSE)

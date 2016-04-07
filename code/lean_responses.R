### code_response function definition:
# code_response takes a question, the response column index,
# and a response value, and returns a human readable question type
# "qtype", a human readable version of the individual response "rvalue",
# and the sub question item from a matrix question "mitem"
code_response <- function (Question, l, r) {
  qtype = ""
  rvalue = ""
  mitem = ""

  if (is_multiple_choice(Question)) {
    qtype = "Check All"
    if (is_matrix_question(Question)) {
      manswers = sapply(Question$Payload$Answers,
        function (x) x$Display)
      mchoices = sapply(Question$Payload$Choices,
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
    rvalue = sapply(Question$Payload$Choices,
      function(x) x$Display)[[l]]
    }

  } else if (is_single_answer(Question)) {
    qtype = "Single Answer"
    if (is_matrix_question(Question)) {
      manswers = sapply(Question$Payload$Answers,
        function (x) x$Display)
      mchoices = sapply(Question$Payload$Choices,
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
      mitem = mchoices[[l]]
    } else {
    if (r == -99) {
      rvalue = -99
    } else {
      rvalue = sapply(Question$Payload$Choices,
      function (x) x$Display)[[r]]
    }
  }

  } else if (is_rank_order(Question)) {
    qtype = "Rank Order"
    if (r == -99) {
      rvalue = -99
    } else {
      rvalue = sapply(Question$Payload$Choices,
      function (x) x$Display)[[r]]
    }

  } else if (is_text_entry(Question)) {
    qtype = "Text Entry"
    rvalue = ""

  } else {
    qtype = ""
    rvalue = ""
  }

  return(c(qtype, rvalue, mitem))
}

### create_entry creates the row for any individual
# response with the following elements in it:
# "ResponseID",
# "ResponseColumnName",
# "DataExportTag",
# "QuestionText",
# "Block",
# "QuestionType",
# "QuestionType2",
# "QuestionType3",
# "QuestionType4",
# "QuestionType5",
# "OriginalResponse",
# "CodedResponse"
create_entry <- function(i, j, k, l, m) {
  return(c(
  # response id
  toString(responses['ResponseID'][m,1]),
  # response column name
  colnames(Blocks[[i]]$BlockElements[[j]]$Responses)[[l]],
  # data export tag
  Blocks[[i]]$BlockElements[[j]]$Payload$DataExportTag,
  # question text
  Blocks[[i]]$BlockElements[[j]]$Payload$QuestionText,
  # block name
  Blocks[[i]]$Description,
  # matrix secondary question component
  response_coded[[1]],
  # qualtrics question type
  Blocks[[i]]$BlockElements[[j]]$Payload$QuestionType,
  # qualtrics question selector
  Blocks[[i]]$BlockElements[[j]]$Payload$Selector,
  # qualtrics question subselector
  Blocks[[i]]$BlockElements[[j]]$Payload$SubSelector,
  # question column type
  response_coded[[3]],
  # response symbolic value
  Blocks[[i]]$BlockElements[[j]]$Responses[[l]][[m]],
  # response human readable value
  response_coded[[2]]
  ))
}



### loop through each block, then each question,
# then of the columns of the responses,
# then each of the entries in each of the response columns,
# and create an entry using "create_entry"
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
            # make sure that subselector is defined
            if (is.null(Blocks[[i]]$BlockElements[[j]]$Payload$SubSelector)) {
              Blocks[[i]]$BlockElements[[j]]$Payload$SubSelector <- ""
            }
            response_coded <- code_response(Blocks[[i]]$BlockElements[[j]], l, r)
            entries[[e]] <- create_entry(i, j, k, l, m)
          }
        }
      }
    }
  }
}

# entries are turned into a data frame with the specified headers
lean_responses <- list_of_rows_to_df(entries)
colnames(lean_responses) <- c("ResponseID", "ResponseColumnName", "DataExportTag",
"QuestionText", "Block", "QuestionType", "QuestionType2",
"QuestionType3", "QuestionType4", "QuestionType5", "OriginalResponse", "CodedResponse")

# row.names=FALSE is so we don't get that stupid 1,2,3,... in the first column.
write.csv(lean_responses, 'output/lean_responses.csv', row.names=FALSE)

displayStimulus <- function(.text = NULL, .img = NULL, .type_stimulus = c("text", "image")) {
  rlang::arg_match(.type_stimulus)
  if (.type_stimulus == "image") {
    assertthat::assert_that(is_image(.img), msg = "x is not an image!")
    out <- tagList(
      #HTML(.text),
      tags$p(.text, style = "text-align: justify"),
      rep_br(2),
      HTML(paste0("<center>",tags$img(
        src = .img,
        width = "600px",
        #width = "40%",
        #height = "30%",
        class = "center"
      ), "</center>")
    ))
  } else if (.type_stimulus == "text") {
    assertthat::assert_that(!is_image(.img), msg = "x is an image, but is tagged as normal text!")
    out <- shiny::HTML(markdown::markdownToHTML(text = .text, fragment.only = TRUE))
  }
  return(out)
}

radioButtonsDynamic <- function(inputId, choices, type_answer = c("text", "image"), correct_id) {
  rlang::arg_match(type_answer)
  radio_items <- lapply(seq_along(choices), function(i) {
    id <- paste0(inputId, i)
    is_correct <- i == correct_id
    if (type_answer == "image") {
      assertthat::assert_that(is_image(choices[i]), msg = "x is not an image!")
      tagList(
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 20px;",
          tags$input(
            type = "radio",
            name = inputId,
            id = id,
            value = as.character(choices[i]),
            style = "margin-right: 10px;"
          ),
          tags$label(
            `for` = id,
            #id = paste0("label_", id), # Add a unique ID for the label
            class = "default_answer",
            tags$img(
              src = choices[i],
              #width = "30%", # Adjust the width as needed
              width = "500px",
              #height = "30%", # Adjust the height as needed
              id = paste0("label_", id)
            )
          )
        )
      )
    } else {
      assertthat::assert_that(
        !is_image(choices[i]),
        msg = "x is an image, but is tagged as normal text!"
      )
      
      tagList(
        tags$div(
          style = "display: flex; align-items: flex-start; margin-bottom: 20px;",
          tags$div(
            style = "display: flex; align-items: center; margin-right: 10px; margin-top: 3px;",
            tags$input(
              type = "radio",
              name = inputId,
              id = id,
              value = as.character(choices[i]),
              style = "width: 18px; height: 18px; cursor: pointer;"
            )
          ),
          tags$div(
            style = "flex: 1; line-height: 1.4; cursor: pointer;",
            tags$label(
              `for` = id,
              id = paste0("label_", id),
              class = "default_answer",
              style = "display: inline-block; vertical-align: middle;",
              shiny::HTML(markdown::markdownToHTML(text = choices[i], fragment.only = TRUE))
            )
          )
        )
      )
    }
  })

  do.call(
    function(...) div(..., class = "shiny-input-radiogroup", id = inputId),
    radio_items
  )
}

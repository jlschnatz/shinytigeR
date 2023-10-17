displayStimulus <- function(.text = NULL, .img = NULL, .type_stimulus = c("text", "image")) {
  rlang::arg_match(.type_stimulus)
  if (.type_stimulus == "image") {
    assertthat::assert_that(is_image(.img), msg = "x is not an image!")
    out <- tagList(
      HTML(.text),
      br(),
      tags$img(
        src = .img,
        width = "50%",
        #height = "30%",
       # class = "center"
      )
    )
  } else if (.type_stimulus == "text") {
    assertthat::assert_that(!is_image(.img), msg = "x is an image, but is tagged as normal text!")
    out <- HTML(.text)
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
            id = paste0("label_", id), # Add a unique ID for the label
            class = "default_answer",
            tags$img(
              src = choices[i],
              width = "30%", # Adjust the width as needed
              height = "30%", # Adjust the height as needed
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
          style = "display: flex; align-items: center; margin-bottom: 20px;",
          tags$div(
            style = "flex: 0 0 auto; margin-right: 10px;",
            tags$input(
              type = "radio",
              name = inputId,
              id = id,
              value = as.character(choices[i])
            )
          ),
          tags$span(
            style = "flex: 1 1 auto;",
            tags$label(
              `for` = id,
              id = paste0("label_", id), # Add a unique ID for the label
              class = "default_answer",
              choices[i]
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

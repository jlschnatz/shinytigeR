#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

check_na_feedback <- function(.feedback_col, .answer_correct) {
  fb_enq <- rlang::enquo(.feedback_col)
  fb_name <- rlang::quo_name(fb_enq)
  assertthat::assert_that(
    grepl("^if_answeroption_[0-9]{2}$", fb_name),
    msg = "Column does not match the naming convention if_answeroption_0<int>"
  )
  assertthat::assert_that(dplyr::between(.answer_correct, 1, 6))
  fb_number <- substr_nth(fb_name, n = 1, side = "right")

  if (is.na(.feedback_col)) {
    return(dplyr::if_else(
      fb_number == .answer_correct,
      paste0("Richtige Antwort. Gut gemacht! ", icon("smile")),
      "Falsche Antwort."
    ))
  } else {
    return(.feedback_col)
  }
}

# image validation function
is_image <- function(x) {
  pattern <- "(([\\/a-zA-Z0-9äöüÄÖÜ_-]*?)\\.(svg|pdf|png|jpeg|jpg|tiff))$"
  return(grepl(pattern = pattern, x))
}


# Useful helpers
substr_nth <- function(x, n = 1, side = "right") {
  rlang::arg_match(side, c("left", "right"))
  assertthat::assert_that(is.character(x))
  assertthat::assert_that(n < nchar(x))
  out <- switch(side,
    "right" = substr(x, nchar(x) - n + 1, nchar(x)),
    "left"  = substr(x, 1, n)
  )

  return(out)
}

get_answeroptions <- function(.df, .row) {
  na.omit(as.character(.df[.row, paste0("answeroption_0", seq_len(6))]))
}

get_feedbackoptions <- function(.df, .row) {
  na.omit(as.character(.df[.row, paste0("if_answeroption_0", seq_len(6))]))
}

## IRT functions
# 2PL model function
prob_2pl <- function(theta, discrimination, difficulty) {
  return(1 / (1 + exp(-discrimination * (theta - difficulty))))
}

# Negative log-likelihood function for the 2PL model
negative_log_likelihood <- function(theta, responses, discrimination, difficulty) {
  p <- prob_2pl(theta, discrimination, difficulty)
  return(-sum(responses * log(p) + (1 - responses) * log(1 - p)))
}

# Function to estimate theta for a person using Maximum Likelihood
estimate_theta <- function(responses, discrimination, difficulty) {
  result <- optim(0, negative_log_likelihood,
    method = "L-BFGS-B",
    lower = -3, upper = 3, responses = responses,
    discrimination = discrimination, difficulty = difficulty
  )
  return(result$par)
}


#### Goethe Color Palette ----
# thanks @ https://drsimonj.svbtle.com/
# define colors
goethe_colors <- c(
  `blue` = "#00618f",
  `yellow` = "#e3ba0f",
  `magenta` = "#ad3b76",
  `green` = "#737c45",
  `orange` = "#c96215"
)

# define extractor
goethe_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols)) {
    return(goethe_colors)
  } else {
    goethe_colors[cols]
  }
}

# define palette
goethe_palettes <- list(
  `main` = goethe_cols("blue", "yellow", "magenta"),
  `long` = goethe_cols(),
  `reds` = goethe_cols("yellow", "orange", "magenta"),
  `blues` = goethe_cols("yellow", "green", "blue")
)

# palette grabber
goethe_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- goethe_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}

# define scales for ggplot
scale_color_goethe <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- goethe_pal(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("colour", paste0("goethe_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_goethe <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- goethe_pal(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("fill", paste0("goethe_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

pill <- function(...) {
  shiny::tabPanel(..., class = "p-3 border")
}

tab <- function(...) {
  #bslib::nav_panel(..., class = "p-3 border border-top-0 rounded-bottom")
  bslib::nav_panel(..., class = "p-3")

}

bs_table <- function(x, class = NULL, ...) {
  class <- paste(c("table", class), collapse = " ")
  class <- sprintf('class="%s"', class)
  HTML(knitr::kable(x, format = "html", table.attr = class))
}

loginUI <- function(id, title = "Please log in", user_title = "User Name",
                    pass_title = "Password", login_title = "Log in", error_message = "Invalid username or password!",
                    additional_ui = NULL, cookie_expiry = 7) {
  ns <- shiny::NS(id)
  shinyjs::hidden(
    shiny::div(
      id = ns("panel"),
      style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      bslib::card(
        bslib::card_header(
          class = "text-center bg-primary text-light",
          shiny::tags$b(title)
        ),
        bslib::card_body(
          shiny::textInput(
            ns("user_name"),
            shiny::tagList(
              bsicons::bs_icon("person-fill"),
              user_title
            ),
            width = "100%",
            placeholder = "Benutzername"
          ),
          shiny::passwordInput(
            ns("password"),
            shiny::tagList(
              bsicons::bs_icon("lock-fill"),
              pass_title
            ),
            placeholder = "Passwort",
            width = "100%"
          ),
          shiny::div(
            style = "text-align: center;",
            shiny::actionButton(
              ns("button"),
              login_title,
              class = "btn-primary",
              style = "color: white;"
            )
          ),
          additional_ui,
          shinyjs::useShinyjs(),
          shinyauthr:::jscookie_script(),
          shinyjs::extendShinyjs(
            text = shinyauthr:::js_cookie_to_r_code(ns("jscookie"),
              expire_days = cookie_expiry
            ),
            functions = c(
              "getcookie",
              "setcookie",
              "rmcookie"
            )
          ),
          shinyjs::extendShinyjs(
            text = shinyauthr:::js_return_click(
              ns("password"),
              ns("button")
            ),
            functions = c()
          ),
          fillable = TRUE,
          fill = TRUE,
          padding = 25,
          style="text-align: justify;",
          gap = "10pt"
        )
      )
    )
  )
}

loginServer <- function(id, data, user_col, pwd_col, sodium_hashed = FALSE,
                        log_out = shiny::reactiveVal(), reload_on_logout = FALSE,
                        cookie_logins = FALSE, sessionid_col, cookie_getter, cookie_setter) {
  try_class_uc <- try(class(user_col), silent = TRUE)
  if (try_class_uc == "character") {
    user_col <- rlang::sym(user_col)
  }
  try_class_pc <- try(class(pwd_col), silent = TRUE)
  if (try_class_pc == "character") {
    pwd_col <- rlang::sym(pwd_col)
  }
  if (cookie_logins && (missing(cookie_getter) | missing(cookie_setter) |
    missing(sessionid_col))) {
    stop("if cookie_logins = TRUE, cookie_getter, cookie_setter and sessionid_col must be provided")
  } else {
    try_class_sc <- try(class(sessionid_col), silent = TRUE)
    if (try_class_sc == "character") {
      sessionid_col <- rlang::sym(sessionid_col)
    }
  }
  data <- dplyr::mutate_if(data, is.factor, as.character)
  shiny::moduleServer(id, function(input, output, session) {
    credentials <- shiny::reactiveValues(
      user_auth = FALSE,
      info = NULL, cookie_already_checked = FALSE
    )
    shiny::observeEvent(log_out(), {
      if (cookie_logins) {
        shinyjs::js$rmcookie()
      }
      if (reload_on_logout) {
        session$reload()
      } else {
        shiny::updateTextInput(session, "password", value = "")
        credentials$user_auth <- FALSE
        credentials$info <- NULL
      }
    })
    shiny::observe({
      if (cookie_logins) {
        if (credentials$user_auth) {
          shinyjs::hide(id = "panel")
        } else if (credentials$cookie_already_checked) {
          shinyjs::show(id = "panel")
        }
      } else {
        shinyjs::toggle(id = "panel", condition = !credentials$user_auth)
      }
    })
    if (cookie_logins) {
      shiny::observeEvent(shiny::isTruthy(shinyjs::js$getcookie()), {
        shinyjs::js$getcookie()
      })
      shiny::observeEvent(input$jscookie, {
        credentials$cookie_already_checked <- TRUE
        shiny::req(credentials$user_auth == FALSE, is.null(input$jscookie) ==
          FALSE, nchar(input$jscookie) > 0)
        cookie_data <- dplyr::filter(
          cookie_getter(),
          {{ sessionid_col }} == input$jscookie
        )
        if (nrow(cookie_data) != 1) {
          shinyjs::js$rmcookie()
        } else {
          .userid <- dplyr::pull(cookie_data, {{ user_col }})
          .sessionid <- randomString()
          shinyjs::js$setcookie(.sessionid)
          cookie_setter(.userid, .sessionid)
          cookie_data <- dplyr::slice_head(dplyr::filter(
            cookie_getter(),
            {{ sessionid_col }} == .sessionid, {{ user_col }} == .userid
          ))
          credentials$user_auth <- TRUE
          credentials$info <- dplyr::bind_cols(dplyr::filter(
            data,
            {{ user_col }} == .userid
          ), dplyr::select(
            cookie_data,
            -{{ user_col }}
          ))
        }
      })
    }
    shiny::observeEvent(input$button, {
      row_username <- which(dplyr::pull(data, {{ user_col }}) == input$user_name)
      if (length(row_username)) {
        row_password <- dplyr::filter(data, dplyr::row_number() ==
          row_username)
        row_password <- dplyr::pull(row_password, {{ pwd_col }})
        if (sodium_hashed) {
          password_match <- sodium::password_verify(
            row_password,
            input$password
          )
        } else {
          password_match <- identical(row_password, input$password)
        }
      } else {
        password_match <- FALSE
      }
      if (length(row_username) == 1 && password_match) {
        credentials$user_auth <- TRUE
        credentials$info <- dplyr::filter(data, {{ user_col }} == input$user_name)
        if (cookie_logins) {
          .sessionid <- randomString()
          shinyjs::js$setcookie(.sessionid)
          cookie_setter(input$user_name, .sessionid)
          cookie_data <- dplyr::filter(dplyr::select(
            cookie_getter(),
            -{{ user_col }}
          ), {{ sessionid_col }} == .sessionid)
          if (nrow(cookie_data) == 1) {
            credentials$info <- dplyr::bind_cols(
              credentials$info,
              cookie_data
            )
          }
        }
      } else {
        shinyalert::shinyalert(
          title = "Achtung",
          text = "Invalider Benutzername oder Passwort!",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "error",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      }
    })
    shiny::reactive({
      shiny::reactiveValuesToList(credentials)
    })
  })
}

parse_clock <- function(.seconds) {
  clock_sec <- .seconds %% 60
  clock_min <- .seconds %/% 60
  clock_hour <- .seconds %/% 3600

  clock_parsed <- dplyr::case_when(
    .seconds < 60 ~ paste0(clock_sec, " sek."),
    .seconds < 3600 ~ paste0(clock_min, "min, ", clock_sec, " sek."),
    .seconds < 86400 ~ paste0(clock_hour, "h, ", clock_min - clock_hour*60, "min, ", clock_sec, " sek."),
    .seconds >= 86400 ~ "Du solltest definitiv aufhören...1 Tag!"
  )

  return(clock_parsed)
}

parse_difftime <- function(.cur_time_reactive, .start_time) {
  diff_time <- difftime(.cur_time_reactive, .start_time, units = "secs")
  return(round(as.numeric(diff_time), 0))
}

safe_sample <- function(x, ...) x[sample.int(length(x), ...)]
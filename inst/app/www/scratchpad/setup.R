# Runs once in the student's webR session when the R console starts
# (www/js/scratchpad.js; R/mod_scratchpad.R explains the feature). Everything
# lives in an attached environment, so rm(list = ls()) doesn't remove it.
local({
  shims <- new.env()
  data_dir <- "/home/web_user/.pandar"
  data_url <- "https://pandar.netlify.app/daten/"

  # pandaR sends no CORS header, so the browser can't download its datasets.
  # The app's server hands them over at start-up (written to data_dir), and
  # url() reads these copies: load(url("https://pandar.netlify.app/daten/x.rda"))
  # works as written in the items. Any other URL behaves as before.
  base_url <- base::url
  shims$url <- function(description, open = "", ...) {
    if (is.character(description) && length(description) == 1L &&
      startsWith(description, data_url)) {
      f <- file.path(data_dir, basename(description))
      if (!file.exists(f)) {
        stop("Der Datensatz ", basename(description),
          " ist in der R-Konsole nicht verfügbar.",
          call. = FALSE
        )
      }
      # gzfile() reads compressed and uncompressed files alike
      return(gzfile(f, open = open))
    }
    base_url(description, open = open, ...)
  }

  # Packages come from webR's own repository (repo.r-wasm.org)
  ensure <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("Paket „", pkg, "“ wird geladen …")
      webr::install(pkg, quiet = TRUE)
    }
  }
  shims$library <- function(package, ..., character.only = FALSE) {
    if (missing(package)) {
      return(base::library())
    }
    pkg <- if (character.only) package else as.character(substitute(package))
    ensure(pkg)
    base::library(pkg, character.only = TRUE, ...)
  }
  shims$require <- function(package, ..., character.only = FALSE) {
    pkg <- if (character.only) package else as.character(substitute(package))
    try(ensure(pkg), silent = TRUE)
    base::require(pkg, character.only = TRUE, ...)
  }
  shims$install.packages <- function(pkgs, ...) {
    webr::install(pkgs, quiet = TRUE)
    invisible()
  }

  # Echo of code lines: \001 marks an expression's first line, \002 its
  # continuation lines (shown as "> " and "+ " by scratchpad.js)
  echo <- function(lines) {
    cat("\001", paste(lines, collapse = "\n\002"), "\n", sep = "")
  }

  # Runs the student's code like the R console: expression by expression,
  # each echoed before its output, visible results printed, and an error
  # stops the rest (output up to there is kept).
  shims$.tiger_run <- function(code) {
    exprs <- tryCatch(
      parse(text = code, keep.source = TRUE),
      error = function(e) {
        echo(strsplit(code, "\n", fixed = TRUE)[[1]])
        msg <- sub("^<text>:", "Zeile ", conditionMessage(e))
        cat("Fehler: ", msg, "\n", sep = "", file = stderr())
        NULL
      }
    )
    if (is.null(exprs)) {
      return(invisible())
    }
    src <- attr(exprs, "srcref")
    for (i in seq_along(exprs)) {
      echo(as.character(src[[i]]))
      ok <- tryCatch(
        withCallingHandlers(
          {
            res <- withVisible(eval(exprs[[i]], envir = globalenv()))
            if (res$visible) print(res$value)
            TRUE
          },
          warning = function(w) {
            cat("Warnung: ", conditionMessage(w), "\n", sep = "", file = stderr())
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          call <- conditionCall(e)
          # a top-level stop() reports this function's own eval() call: shown
          # like the R console, without a call
          top <- is.call(call) && identical(call[[1]], quote(eval))
          where <- if (!is.null(call) && !top) paste0(" in ", deparse(call, nlines = 1L)) else ""
          cat("Fehler", where, ": ", conditionMessage(e), "\n", sep = "", file = stderr())
          FALSE
        }
      )
      if (!isTRUE(ok)) break
    }
    invisible()
  }

  # ── Help ─────────────────────────────────────────────────────────────────
  # webR hands help pages to a pager the console doesn't have, so ?mean
  # showed nothing. Printing a help object (what ? and help() return) now
  # writes the page as text, each line marked \003: scratchpad.js shows those
  # lines in the panel's help area, not in the output.
  print_help <- function(x, ...) {
    paths <- as.character(x)
    topic <- attr(x, "topic")
    if (!length(paths)) {
      cat("Keine Hilfe zu „", topic, "“ gefunden.\n", sep = "", file = stderr())
      return(invisible(x))
    }
    rd <- utils:::.getHelpFile(paths[1])
    txt <- utils::capture.output(
      tools::Rd2txt(rd, options = list(underline_titles = FALSE))
    )
    pkg <- basename(dirname(dirname(paths[1])))
    cat(paste0("\003", c(paste0(topic, " {", pkg, "}"), txt)), sep = "\n")
    if (length(paths) > 1L) {
      others <- basename(dirname(dirname(paths[-1])))
      cat("Hinweis: „", topic, "“ gibt es auch in ",
        paste(others, collapse = ", "), "; help(", topic,
        ", package = \"…\") zeigt sie.\n",
        sep = "", file = stderr()
      )
    }
    invisible(x)
  }
  registerS3method("print", "help_files_with_topic", print_help,
    envir = asNamespace("utils")
  )
  # F1 in the editor: the help for the word at the cursor, without echo
  shims$.tiger_help <- function(topic) {
    h <- tryCatch(do.call(utils::help, list(topic)), error = function(e) NULL)
    if (is.null(h)) {
      cat("Keine Hilfe zu „", topic, "“ gefunden.\n", sep = "", file = stderr())
    } else {
      print_help(h)
    }
    invisible()
  }

  # ── Completion and signature hint (scratchpad.js, Ace) ─────────────────────
  # R's own completion engine (the plain R console's Tab): function and
  # object names, argument names inside f(...), columns after $. One
  # "kind<TAB>completion" string each, kind = arg/fun/obj/other.
  shims$.tiger_complete <- function(line) {
    utils::rc.settings(ipck = TRUE)
    utils:::.assignLinebuffer(line)
    utils:::.assignEnd(nchar(line))
    utils:::.guessTokenFromLine()
    utils:::.completeToken()
    comps <- utils::head(utils:::.retrieveCompletions(), 200L)
    kind <- vapply(comps, function(x) {
      if (endsWith(x, "=")) {
        "arg"
      } else if (grepl("[$@]", x)) {
        "obj"
      } else if (exists(x, envir = globalenv(), mode = "function")) {
        "fun"
      } else if (exists(x, envir = globalenv())) {
        "obj"
      } else {
        "other"
      }
    }, "")
    paste(kind, comps, sep = "\t")
  }

  # The call signature, e.g. "mean(x, ...)". For S3 generics the default
  # method's ("t.test(x, ...)" alone says little)
  shims$.tiger_signature <- function(name) {
    f <- if (grepl("::", name, fixed = TRUE)) {
      tryCatch(eval(str2lang(name)), error = function(e) NULL)
    } else {
      get0(name, envir = globalenv(), mode = "function")
    }
    if (!is.function(f)) {
      return("")
    }
    if (!is.primitive(f) && any(grepl("UseMethod", deparse(body(f)), fixed = TRUE))) {
      d <- utils::getS3method(sub(".*::", "", name), "default", optional = TRUE)
      if (!is.null(d)) f <- d
    }
    a <- args(f)
    if (is.null(a)) {
      return("")
    }
    s <- deparse(a, width.cutoff = 500L)
    s <- paste(trimws(s[-length(s)]), collapse = " ")
    trimws(sub("^function ", name, s))
  }

  attach(shims, name = "tigeR:konsole", warn.conflicts = FALSE)
})

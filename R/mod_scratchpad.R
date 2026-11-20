# R scratchpad ("R-Konsole") — PROTOTYPE (branch feature/webr-scratchpad)
#
# An optional R console in a side panel of the practice view, for coding
# items. R runs in the student's browser with webR (R compiled to
# WebAssembly, www/js/scratchpad.js), never on the server: students' code
# can't reach the databases, and nothing they type is stored. The answer is
# still given with the item's MC/numeric input; the console is only a
# scratchpad, so scoring, the ability estimate and the item DB are unchanged.
#
# The server's only part: pandaR's datasets. Items load them with
# load(url("https://pandar.netlify.app/daten/x.rda")), but pandaR sends no
# CORS header, so the browser may not read them. The server downloads them
# (servers aren't bound by CORS) and hands them to the console, where url()
# reads these copies (see scratchpad.js).
#
# webR and the editor (Ace) come from CDNs (SCRATCHPAD_WEBR_URL,
# SCRATCHPAD_ACE_URL) in this prototype; to be self-hosted like KaTeX if the
# feature stays.

# Base of webR's files (its CDN, versioned; sends CORS headers)
SCRATCHPAD_WEBR_URL <- "https://webr.r-wasm.org/v0.6.1/"

# The code editor: Ace (BSD), R highlighting and auto-closed brackets and
# quotes; loaded on the first open like webR, the plain textarea until then
SCRATCHPAD_ACE_URL <- "https://cdnjs.cloudflare.com/ajax/libs/ace/1.45.2/"

# pandaR's data folder
PANDAR_DATA_URL <- "https://pandar.netlify.app/daten/"

# R packages that ship with webR, never installed
SCRATCHPAD_BASE_PKGS <- c(
  "base", "compiler", "datasets", "graphics", "grDevices", "grid",
  "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools",
  "utils", "webr"
)

# All text of the coding items (stimulus, options, feedback), one string per
# item
scratchpad_item_text <- function(items) {
  items <- items[items$type_item %in% "coding", , drop = FALSE]
  cols <- intersect(
    names(items),
    c("stimulus_text", sprintf("answeroption_%02d", 1:6), sprintf("if_answeroption_%02d", 1:6))
  )
  if (!nrow(items) || !length(cols)) {
    return(character())
  }
  txt <- do.call(paste, c(lapply(items[cols], function(x) ifelse(is.na(x), "", x)), sep = "\n"))
  as.character(txt)
}

# The pandaR datasets the coding items load (full URLs, unique)
pandar_data_urls <- function(items) {
  txt <- scratchpad_item_text(items)
  pat <- "https://pandar\\.netlify\\.app/daten/[A-Za-z0-9_.-]+\\.(rda|RData|rdata|rds|csv)"
  as.character(sort(unique(unlist(regmatches(txt, gregexpr(pat, txt))))))
}

# Packages the coding items use with pkg:: (pre-installed when the console
# starts, since `::` can't install on demand; library() can, see
# scratchpad.js)
scratchpad_packages <- function(items) {
  txt <- scratchpad_item_text(items)
  m <- unlist(regmatches(txt, gregexpr("\\b[A-Za-z][A-Za-z0-9.]*(?=:::?)", txt, perl = TRUE)))
  setdiff(as.character(sort(unique(m))), SCRATCHPAD_BASE_PKGS)
}

# Downloads the datasets (once per R process, cached in tempdir()) and
# returns them base64-encoded for the browser: list(files = list of
# list(name, b64), failed = names)
scratchpad_fetch_data <- function(urls, cache_dir = file.path(tempdir(), "pandar")) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  old <- options(timeout = 10)
  on.exit(options(old))
  files <- list()
  failed <- character()
  for (u in urls) {
    name <- basename(u)
    f <- file.path(cache_dir, name)
    if (!file.exists(f)) {
      ok <- tryCatch(
        {
          utils::download.file(u, f, mode = "wb", quiet = TRUE)
          TRUE
        },
        error = function(e) FALSE,
        warning = function(w) FALSE
      )
      if (!ok) {
        unlink(f)
        failed <- c(failed, name)
        next
      }
    }
    files[[length(files) + 1L]] <- list(
      name = name,
      b64 = jsonlite::base64_enc(readBin(f, "raw", file.size(f)))
    )
  }
  list(files = files, failed = failed)
}

# The side panel (a non-modal Bootstrap offcanvas: the item stays readable
# and clickable while it's open). Moved to <body> by scratchpad.js, so no
# transformed or hidden ancestor traps it.
mod_scratchpad_ui <- function(id) {
  ns <- NS(id)
  div(
    id = ns("panel"),
    class = "offcanvas offcanvas-end scratchpad",
    tabindex = "-1",
    `data-bs-backdrop` = "false",
    `data-bs-scroll` = "true",
    `aria-labelledby` = ns("title"),
    `data-init-input` = ns("init"),
    `data-webr-url` = SCRATCHPAD_WEBR_URL,
    `data-ace-url` = SCRATCHPAD_ACE_URL,
    div(
      class = "offcanvas-header scratchpad-head",
      tags$h2(
        id = ns("title"),
        class = "scratchpad-title",
        bsicons::bs_icon("terminal"), "R-Konsole"
      ),
      tags$span(class = "scratchpad-status", role = "status", "Noch nicht gestartet"),
      tags$button(
        type = "button", class = "btn-close",
        `data-bs-dismiss` = "offcanvas", `aria-label` = "Schließen"
      )
    ),
    div(
      class = "offcanvas-body scratchpad-body",
      tags$p(
        class = "scratchpad-note",
        "R läuft hier direkt in deinem Browser. Nichts davon wird gespeichert oder bewertet; deine Antwort gibst du wie immer in der Aufgabe ein."
      ),
      # Help pages (?mean, help(), F1 in the editor), shown by scratchpad.js
      div(
        class = "scratchpad-help",
        hidden = NA,
        role = "region",
        `aria-label` = "Hilfe",
        div(
          class = "scratchpad-help-head",
          tags$span(class = "scratchpad-help-title"),
          tags$button(
            type = "button", class = "btn-close scratchpad-help-close",
            `aria-label` = "Hilfe schlie\u00dfen"
          )
        ),
        tags$pre(class = "scratchpad-help-body", tabindex = "0")
      ),
      div(
        class = "scratchpad-output",
        `aria-live` = "polite",
        `aria-label` = "Ausgabe"
      ),
      # No id on purpose: Shiny would bind a textarea with an id as an input
      # and send every keystroke to the server
      tags$textarea(
        class = "scratchpad-code",
        `aria-label` = "R-Code",
        rows = "8",
        spellcheck = "false",
        autocomplete = "off",
        autocapitalize = "off",
        placeholder = "R-Code eingeben, z. B. mean(c(2, 4, 6, 8))"
      ),
      # The signature of the call the cursor is in, e.g. mean(x, trim = 0, ...)
      div(class = "scratchpad-signature", `aria-live` = "polite"),
      div(
        class = "scratchpad-actions",
        tags$span(
          class = "scratchpad-hint",
          tags$kbd("Strg"), "/", tags$kbd("⌘"), "+", tags$kbd("Enter"), " ausf\u00fchren \u00b7 ",
          tags$kbd("Tab"), " Vorschl\u00e4ge \u00b7 ",
          tags$kbd("F1"), " Hilfe"
        ),
        tags$button(
          type = "button", class = "btn btn-outline-secondary btn-sm scratchpad-clear",
          "Ausgabe leeren"
        ),
        tags$button(
          type = "button", class = "btn btn-primary btn-sm scratchpad-run",
          disabled = NA,
          bsicons::bs_icon("play-fill"), "Alles ausführen"
        )
      )
    )
  )
}

# Answers the console's start-up request with the pandaR datasets and the
# packages to pre-install
mod_scratchpad_server <- function(id, data_item) {
  moduleServer(id, function(input, output, session) {
    urls <- pandar_data_urls(data_item)
    pkgs <- scratchpad_packages(data_item)
    observeEvent(input$init, {
      data <- scratchpad_fetch_data(urls)
      session$sendCustomMessage("tiger-scratchpad", list(
        id = session$ns("panel"),
        data_url = PANDAR_DATA_URL,
        files = data$files,
        failed = as.list(data$failed),
        packages = as.list(pkgs)
      ))
    })
  })
}

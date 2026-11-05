# The FAQ tab: static UI, no server part. The contact line on top, then the
# questions (including the links to pandaR and otter), which are data in R/faq.R.
mod_faq_ui <- function(id) {
  div(
    class = "faq-wrap",
    tags$h3(class = "page-title mb-2", "Häufig gestellte Fragen"),
    # Contact first, so it's found without scrolling past all questions
    tags$p(
      class = "text-muted mb-4",
      bsicons::bs_icon("envelope"),
      " Deine Frage ist nicht dabei, oder du hast ein Problem? Schreib uns an ",
      tags$a(href = paste0("mailto:", CONTACT_EMAIL), CONTACT_EMAIL, .noWS = "after"),
      "."
    ),
    # The questions themselves live in R/faq.R (one entry per question)
    faq_section()
  )
}

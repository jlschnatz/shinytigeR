# The FAQ tab: static UI, no server part (built once at app start). The contact line on top, then the
# questions (including the links to pandaR and otter), which are data in R/faq.R.
mod_faq_ui <- function(id) {
  div(
    class = "faq-wrap",
    # Title and contact line, like the home page's greeting (.page-intro).
    # Contact first, so it's found without scrolling past all questions
    div(
      class = "page-intro",
      tags$h3(class = "page-title mb-1", "Häufig gestellte Fragen"),
      tags$p(
        class = "text-muted mb-0",
        bsicons::bs_icon("envelope"),
        " Deine Frage ist nicht dabei, oder du hast ein Problem? Schreib uns an ",
        tags$a(href = paste0("mailto:", CONTACT_EMAIL), CONTACT_EMAIL, .noWS = "after"),
        "."
      )
    ),
    # The questions themselves live in R/faq.R (one entry per question); the
    # difficulty answer says how many items currently have a preliminary one
    faq_section(faq_entries(provisional = provisional_item_count()))
  )
}

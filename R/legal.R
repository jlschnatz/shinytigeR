# Impressum and privacy policy: one public page ("legal_panel" in ui.R), linked
# from the footer on every page (app_footer()). PROVISIONAL, TO BE ADAPTED (the
# page says so too): the contacts and authorities follow pandaR's page
# (pandar.netlify.app/privacy), the section on what tigeR stores follows the
# FAQ answer "Welche Daten speichert tigeR?" (R/faq.R; keep both in sync). The
# final text comes later. Like the FAQ, it makes no claims about the server
# (run by the university) and no anonymity promise.

# The sections as data: `title` and `body` (Markdown, rendered with render_md()).
legal_sections <- function(contact = CONTACT_EMAIL) {
  mail <- sprintf("[%s](mailto:%s)", contact, contact)
  list(
    list(
      title = "Name und Anschrift des Verantwortlichen",
      body = paste(
        "Prof. Dr. Martin Schultze  ",
        "\nJohann Wolfgang Goethe-Universität Frankfurt am Main  ",
        "\nTheodor-W.-Adorno-Platz 6, 60323 Frankfurt am Main  ",
        "\nPostanschrift: 60629 Frankfurt am Main",
        "\n\nTelefon: +49 69 798-35355  ",
        sprintf("\nE-Mail: %s  ", mail),
        "\nWeb: [www.uni-frankfurt.de](https://www.uni-frankfurt.de)"
      )
    ),
    list(
      title = "Kontaktdaten der Datenschutzbeauftragten",
      body = paste(
        "Die behördlichen Datenschutzbeauftragten der Goethe-Universität  ",
        "\nTheodor-W.-Adorno-Platz 1, 60323 Frankfurt am Main  ",
        "\n[www.uni-frankfurt.de/datenschutzbeauftragte](https://www.uni-frankfurt.de/datenschutzbeauftragte)"
      )
    ),
    list(
      title = "Welche Daten tigeR speichert",
      body = paste(
        "tigeR speichert keine Namen, E-Mail-Adressen oder Matrikelnummern. Gespeichert werden nur:",
        "\n\n- dein selbst gewählter **Benutzername** und dein Passwort (nur in unlesbarer Form, als Hash),",
        "\n- zu jeder Antwort: welche Aufgabe, wann, welche Antwort du gewählt oder eingegeben hast, ob sie richtig war, und eine zufällige Kennung der Sitzung,",
        "\n- deine berechneten Einschätzungen pro Themenbereich.",
        "\n\nDiese Daten dienen dazu, dir Rückmeldungen zu deinen Antworten und deinem Lernstand zu geben.",
        "Wähl am besten einen Benutzernamen, der dich nicht erkennbar macht, also nicht deinen Namen."
      )
    ),
    list(
      title = "Deine Rechte",
      body = paste(
        "Du hast das Recht auf Auskunft über die zu dir gespeicherten Daten, auf Berichtigung oder Löschung,",
        "auf Einschränkung der Verarbeitung, auf Widerspruch gegen die Verarbeitung, auf Widerruf einer Einwilligung",
        sprintf("und auf Datenübertragbarkeit. Wende dich dafür an %s.", mail),
        "\n\nAußerdem kannst du dich bei der Aufsichtsbehörde beschweren:  ",
        "\nDer Hessische Beauftragte für Datenschutz und Informationsfreiheit (HBDI)  ",
        "\nPostfach 3163, 65021 Wiesbaden  ",
        "\nTelefon: +49 611 1408-0  ",
        "\n[datenschutz.hessen.de](https://datenschutz.hessen.de)"
      )
    )
  )
}

# The page: title, then one card per section.
legal_page <- function(sections = legal_sections()) {
  tagList(
    div(
      class = "page-intro",
      tags$h3(class = "page-title mb-1", "Impressum & Datenschutz"),
      # Visible until the final text replaces legal_sections()
      tags$p(
        class = "text-muted mb-0",
        "Vorläufige Fassung: übernommen von ",
        tags$a(href = "https://pandar.netlify.app/privacy/", target = "_blank", rel = "noopener", "pandaR"),
        ", wird noch an tigeR angepasst."
      )
    ),
    lapply(sections, function(s) {
      bslib::card(
        class = "legal-card",
        bslib::card_header(tags$b(s$title)),
        bslib::card_body(div(class = "legal-text", shiny::HTML(faq_answer_html(s$body))))
      )
    })
  )
}

# Footer on every page (page_navbar(footer = ...) in ui.R), before and after
# login, fixed to the window's bottom (app.css, --footer-h). The link is an
# actionLink switching to legal_panel (server.R, wired before the auth gate);
# it's static UI, never re-rendered, so the resend-0 quirk of recreated
# actionButtons doesn't apply.
app_footer <- function() {
  tags$footer(
    class = "app-footer",
    div(
      class = "app-footer-inner",
      tags$span(class = "app-footer-name", "tigeR · Goethe-Universität Frankfurt"),
      actionLink("show_legal", "Impressum & Datenschutz")
    )
  )
}

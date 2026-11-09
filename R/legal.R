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
        "\nTheodor-W.-Adorno-Platz 6 ",
        "\n60323 Frankfurt am Main  ",
        "\nPostanschrift: ",
        "\nJohann Wolfgang Goethe-Universität Frankfurt am Main  ",
        "\nTheodor-W.-Adorno-Platz 6 ",
        "\n60629 Frankfurt am Main ",
        "\n\nTelefon: +49 69 798-35355  ",
        sprintf("\nE-Mail: %s  ", mail),
        "\nWeb: [www.uni-frankfurt.de](https://www.uni-frankfurt.de)"
      )
    ),
    list(
      title = "Datenschutzbeauftragte",
      body = paste(
        "Johann Wolfgang Goethe-Universität Frankfurt am Main ",
        "\nDie behördlichen Datenschutzbeauftragten ",
        "\nTheodor-W.-Adorno-Platz 1, 60323 Frankfurt am Main  ",
        "\n\nTelefon: +49 69 798-12238 ",
        "\nE-Mail-Adresse: [dsb@uni-frankfurt.de](mailto:dsb@uni-frankfurt.de) ",
        "\nWeb: [www.uni-frankfurt.de/datenschutzbeauftragte](https://www.uni-frankfurt.de/datenschutzbeauftragte)"
      )
    ),
    list(
      title = "Welche Daten tigeR verarbeitet",
      body = paste(
        "tigeR erhebt, verarbeitet und speichert keine Namen, E-Mail-Adressen oder Matrikelnummern. tigeR verwendet keine Tracking- oder Marketing-Cookies.",
        "\nGespeichert werden aussschließlich: ",
        "\n\n- dein selbst gewählter Benutzername und dein Passwort (nur in unlesbarer Form, als Hashwert),",
        "\n- zu jeder von dir eingegebenen Antwort: Die bearbeitete Aufgabe, Zeitpunkt der Beantwortung, welche Antwort du gewählt oder eingegeben hast, ob sie richtig war, und eine zufällige Kennung der Sitzung,",
        "\n- deine berechneten Einschätzungen pro Themenbereich.",
        "\nDie Daten werden in einer Form gespeichert, die den Betreibern von tigeR keine Zuordnung der Benutzerkonten zu einer bestimmten Person ermöglicht. ",
        "\nDie Daten werden dauerhaft gespeichert, um dir auch bei späteren Besuchen Zugriff auf deinen bisherigen Lernstand und Lernverlauf zu ermöglichen sowie die Daten für wissenschaftliche Forschungszwecke zu nutzen. Eine automatische Löschung nach einem festen Zeitraum erfolgt derzeit nicht. "
      )
    ),
    list(
      title = "Rechte und Beschwerdemöglichkeiten",
      body = paste(
        "Du hast gegenüber der Goethe-Universität folgende Rechte hinsichtlich deiner gespeicherten personenbezogenen Daten: ",
        "\n- Recht auf Auskunft (Art. 15 DSGVO),",
        "\n- Recht auf Berichtigung oder Löschung (Art. 16 und 17 DSGVO),",
        "\n- Recht auf Einschränkung der Verarbeitung (Art. 18 DSGVO),",
        "\n- Recht auf Widerruf Ihrer Einwilligung, sofern die Verarbeitung auf einer Einwilligung beruht (Art. 7 Abs. 3 DSGVO),",
        "\n- Recht auf Widerspruch gegen die Verarbeitung (Art. 21 DSGVO),",
        "\n- Recht auf Datenübertragbarkeit in einem strukturierten, gängigen und maschinenlesbaren Format (Art. 20 DSGVO).",
        "\n\nDu hast das Recht, sich bei datenschutzrechtlichen Problemen bei der zuständigen Fachaufsichtsbehörde zu beschweren. Wende dich dafür an die Kontaktadresse der Fachaufsichtsbehörde der Goethe-Universität Frankfurt am Main:",
        "\nHessischer Beauftragter für Datenschutz und Informationsfreiheit (HBDI) ",
        "\nPostfach 3163, 65021 Wiesbaden",
        "\n\nE-Mail an den HBDI (Link zum Kontaktformular des Hessischen Datenschutzbeauftragten: [https://datenschutz.hessen.de/kontakt](https://datenschutz.hessen.de/kontakt)) ",
        "\nTelefon: +49 611 1408 - 0"
      )
    ),
    list(
      title = "Änderung dieser Datenschutzerklärung",
      body = paste(
        "Wir behalten uns vor, diese Datenschutzerklärung anzupassen, um sie an geänderte rechtliche Anforderungen oder technische Entwicklungen anzupassen. Die jeweils aktuelle Version ist auf dieser Webseite abrufbar. ",
        "\n\nStand: **9. November 2026**"
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
      tags$span(
        class = "app-footer-name",
        "tigeR · Goethe-Universität Frankfurt",
        # only from 992 px (app.css): too long for narrower windows
        tags$span(
          class = "app-footer-lab",
          "· Abteilung für Psychologische Methoden mit interdisziplinärer Ausrichtung"
        )
      ),
      actionLink("show_legal", "Impressum & Datenschutz")
    )
  )
}

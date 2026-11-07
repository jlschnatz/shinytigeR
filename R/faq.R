FAQ_TOPICS <- c("Üben", "Fortschritt", "Konto und Daten", "Hilfe")

# `provisional`: the pool's count of items with a preliminary difficulty,
# c(n = , total = ) from provisional_item_count(); NULL leaves that sentence out.
faq_entries <- function(contact = CONTACT_EMAIL, provisional = NULL) {
  mail <- sprintf("[%s](mailto:%s)", contact, contact)
  provisional_txt <- if (is.null(provisional)) {
    NULL
  } else if (provisional[["n"]] == 0L) {
    "Zurzeit ist die Schwierigkeit aller Aufgaben aus Antworten berechnet."
  } else {
    sprintf(
      "Zurzeit ist das bei %d von %d Aufgaben der Fall.",
      as.integer(provisional[["n"]]), as.integer(provisional[["total"]])
    )
  }
  list(
    # ── Üben ──────────────────────────────────────────────────────────────────
    list(
      topic = "Üben",
      q = "Wie wähle ich Aufgaben aus?",
      a = paste(
        "Unter **Üben** wählst du in Schritt 1 pro Themenbereich aus, ob du inhaltliche Aufgaben, R-Code-Aufgaben oder beides üben möchtest.",
        "In Schritt 2 legst du fest, wie viele Aufgaben du bearbeiten möchtest, und klickst auf **Starten**.",
        "Die Aufgaben werden **zufällig** aus deiner Auswahl gezogen.",
        "Mit **Nur neue Aufgaben** bekommst du nur Aufgaben, die du noch nicht bearbeitet hast."
      )
    ),
    list(
      topic = "Üben",
      q = "Was ist der Unterschied zwischen „Inhaltlich“ und „R-Code“?",
      a = paste(
        "**Inhaltliche** Aufgaben prüfen dein Verständnis der Statistik. Es werden z.B. Begriffe, Interpretationen oder konkrete Rechnungen abgefragt.",
        "**R-Code**-Aufgaben drehen sich um die Umsetzung in R, zum Beispiel was ein Befehl ausgibt oder welcher Code eine Analyse richtig umsetzt."
      )
    ),
    list(
      topic = "Üben",
      q = "Wie gebe ich bei Zahlenaufgaben meine Antwort ein?",
      a = paste(
        "Tippe die Zahl in das Feld **Deine Antwort**. Als Dezimaltrennzeichen geht ein Punkt oder ein Komma (3.5 oder 3,5).",
        "Runde so, wie es in der Aufgabe angegeben ist."
      )
    ),
    list(
      topic = "Üben",
      q = "Was bedeutet „Antwort nicht erkannt“?",
      a = paste(
        "Deine Zahl passt zu keiner der Antworten, die für diese Aufgabe hinterlegt sind, weder zur richtigen noch zu einem typischen Fehler.",
        "Eine nicht erkannte Antwort zählt als falsch, auch für deine Einschätzung.",
        "Prüfe am besten deinen Rechenweg und die Rundung. Wenn du gar nicht weiter weißt, kannst du im Tab „Üben“ auf „Spezifische Aufgabe auswählen“, um dir die korrekte Lösung anzeigen zu lassen."
      )
    ),
    list(
      topic = "Üben",
      q = "Was passiert, wenn ich eine Aufgabe überspringe?",
      a = paste(
        "Mit **Aufgabe überspringen** überspringst du deine Antwort.",
        "Übersprungene Aufgaben zählen nicht als bearbeitet und fließen nicht in deine Fähigkeitseinschätzung ein."
      )
    ),
    list(
      topic = "Üben",
      q = "Kann ich eine Aufgabe mehrmals bearbeiten?",
      a = paste(
        "Ja. Für deine Einschätzung zählt pro Aufgabe immer deine **letzte** Antwort:",
        "Löst du eine Aufgabe nach einem Fehler richtig, zählt die richtige Lösung."
      )
    ),
    list(
      topic = "Üben",
      q = "Wie finde ich eine bestimmte Aufgabe wieder?",
      a = paste(
        "Über jeder Aufgabe steht ihre **ID** (ein Klick darauf kopiert sie).",
        "Unter **Üben** kannst du bei **Spezifische Aufgabe auswählen** eine ID eingeben und die Aufgabe ansehen.",
        "Die Lösung erscheint dort erst auf Klick, und das Ansehen zählt nicht als Bearbeitung."
      )
    ),
    # ── Fortschritt ───────────────────────────────────────────────────────────
    list(
      topic = "Fortschritt",
      q = "Wie wird mein Lernstand berechnet?",
      a = paste(
        "Dein Lernstand wird mit einem **psychometrischen Modell** geschätzt, also einem statistischen Modell dafür,",
        "wie eine Fähigkeit mit den Antworten auf Aufgaben zusammenhängt.",
        "tigeR nutzt dafür das **Rasch-Modell**.",
        "Das Modell geht davon aus, dass die Lösung einer Aufgabe nur von deiner Fähigkeit und der Schwierigkeit der Aufgabe abhängt",
        "(siehe die nächste Frage).",
        "\n\nGeschätzt wird dann der Lernstand, der am besten zu deinen Antworten passt:",
        "der Lernstand, bei dem man für die Aufgaben, die du bearbeitet hast, genau so viele richtige Antworten erwarten würde, wie du tatsächlich hast",
        "(in der Statistik heißt das **Maximum-Likelihood-Schätzung**).",
        "Es zählt also, wie viele Aufgaben du richtig gelöst hast, gemessen an ihrer Schwierigkeit:",
        "7 von 10 richtig bei schweren Aufgaben ergibt eine höhere Einschätzung als 7 von 10 bei leichten.",
        "\n\nDas geschieht für jeden Themenbereich getrennt, und pro Aufgabe zählt deine letzte Antwort.",
        "Mehr dazu findest du unter **Fortschritt** bei **Dein Lernstand im Detail** (**Wie wird das berechnet?**)."
      )
    ),
    # Same rules as compute_and_save_ability()/estimate_competency()
    list(
      topic = "Fortschritt",
      q = "Welche Aufgaben zählen für meinen Lernstand?",
      a = paste(
        "Für deinen Lernstand in einem Themenbereich zählen die Aufgaben aus diesem Bereich, und zwar so:",
        "\n\n- Jede Aufgabe, die du **beantwortet** hast, ob richtig oder falsch. Nicht erkannte Zahleneingaben zählen als falsch.",
        "\n- Hast du eine Aufgabe mehrmals bearbeitet, zählt deine **letzte Antwort**.",
        "\n- **Übersprungene** Aufgaben zählen nicht.",
        "\n\nWie viele Aufgaben es in jedem Themenbereich sind, steht unter **Fortschritt** bei **Dein Lernstand im Detail**,",
        "dort auch, bei wie vielen davon die Schwierigkeit noch vorläufig ist (siehe die nächste Frage)."
      )
    ),
    # Rasch difficulty b: the ability at which P(correct) = 50 %, on the same
    # scale as theta
    list(
      topic = "Fortschritt",
      q = "Was bedeutet die „Schwierigkeit“ einer Aufgabe?",
      a = paste(
        "Jede Aufgabe hat einen Schwierigkeitswert. Er liegt auf derselben Skala wie dein Lernstand,",
        "also auf den Zahlen von −3 bis 3, die du unter **Dein Lernstand im Detail** siehst.",
        "Die Schwierigkeit gibt an, bei welchem Lernstand man die Aufgabe mit 50% Wahrscheinlichkeit löst:",
        "Liegt deine Fähigkeit deutlich über der Schwierigkeit einer Aufgabe, löst du sie wahrscheinlich, liegt sie darunter, eher nicht.",
        "\n\nBerechnet wird die Schwierigkeit aus den Antworten der Studierenden, die die Aufgabe schon bearbeitet haben.",
        "Für neue Aufgaben gibt es dafür noch nicht genug Antworten: Ihre Schwierigkeit ist zunächst eine **vorläufige Schätzung**",
        "durch ein KI-Sprachmodell (LLM) und wird durch die aus den Antworten berechnete ersetzt, sobald genug Antworten vorliegen.",
        provisional_txt,
        "Deine Einschätzung kann sich deshalb bei der nächsten Aktualisierung etwas verschieben, auch wenn sich dein Können nicht verändert hat.",
        "Frühere Punkte im **Fähigkeitsverlauf** bleiben so, wie sie damals berechnet wurden."
      )
    ),
    list(
      topic = "Fortschritt",
      q = "Warum ändert sich meine Einschätzung nicht nach jeder Aufgabe?",
      a = paste(
        "Die Einschätzung wird höchstens einmal pro Sitzung neu berechnet: beim Anmelden oder über **Fähigkeitsverlauf aktualisieren**.",
        "Dein Lernstand ändert sich nicht von Aufgabe zu Aufgabe, und so springt die Einschätzung nicht bei jeder einzelnen Antwort hin und her."
      )
    ),
    list(
      topic = "Fortschritt",
      q = "Was bedeuten die Punkte bei „Evidenz“?",
      a = paste(
        "Sie zeigen, wie sicher die Einstufung in einem Bereich ist: drei Punkte heißt wahrscheinlich zutreffend, ein Punkt heißt noch unsicher.",
        "Je mehr Aufgaben du in einem Bereich bearbeitest, desto sicherer wird die Einstufung."
      )
    ),
    list(
      topic = "Fortschritt",
      q = "Wie übe ich meine Fehler nochmal, und warum erst am nächsten Tag?",
      a = paste(
        "Unter **Fortschritt** zeigt dir **Fehler nochmal üben**, wie viele Aufgaben du zuletzt falsch beantwortet hast.",
        "Bis zu 10 davon kannst du dort nochmal üben, frühestens ab dem Tag nach deiner letzten Antwort:",
        "Mit etwas Abstand bleibt die Lösung besser hängen, als wenn du dieselbe Aufgabe gleich noch einmal machst."
      )
    ),
    # ── Konto und Daten ───────────────────────────────────────────────────────
    list(
      topic = "Konto und Daten",
      q = "Wie bekomme ich einen Zugang?",
      a = paste(
        "Unter **Login** klickst du bei „Noch kein Konto?“ auf **Registriere** und legst ein Konto an:",
        "mit einem Benutzernamen (3 bis 30 Zeichen: Buchstaben, Ziffern, Punkt, Binde- oder Unterstrich),",
        "einem Passwort (mindestens 8 Zeichen) und dem **Semester-Code**.",
        "Den Semester-Code bekommst du im Praktikum."
      )
    ),
    list(
      topic = "Konto und Daten",
      q = "Ich habe mein Passwort vergessen. Was nun?",
      a = "Da du dich nur über deinen pseudonymen Namen ohne E-Mail Adresse registrierst, gibt es für uns keine Möglichkeit dein Passwort wiederherzustellen oder zu ändern. In diesem Fall musst du dir also ein neues Konto erstellen. Merk dir dein Passwort also gut und schreibe es dir am besten auf."
    ),
    list(
      topic = "Konto und Daten",
      q = "Welche Daten speichert tigeR?",
      a = paste(
        "tigeR speichert keine Namen, E-Mail-Adressen oder Matrikelnummern. Gespeichert werden nur:",
        "\n\n- dein selbst gewählter **Benutzername** und dein Passwort (nur in unlesbarer Form, auch wir können es nicht lesen),",
        "\n- zu jeder Antwort: welche Aufgabe, wann, welche Antwort du gewählt oder eingegeben hast, ob sie richtig war, und eine zufällige Kennung der Sitzung,",
        "\n- deine berechneten Einschätzungen pro Themenbereich.",
        "\n\nWähl deshalb am besten einen Benutzernamen, der dich nicht erkennbar macht, also nicht deinen Namen."
      )
    ),
    # ── Hilfe ─────────────────────────────────────────────────────────────────
    list(
      topic = "Hilfe",
      q = "Ich habe einen Fehler in einer Aufgabe gefunden. Was kann ich machen?",
      a = sprintf(
        "Notiere dir die **ID** der Aufgabe (sie steht über der Aufgabe, ein Klick kopiert sie) und schreib uns kurz, was dir aufgefallen ist, an %s.",
        mail
      )
    ),
    list(
      topic = "Hilfe",
      q = "Wo finde ich Informationen zu den Datensätzen in den R-Aufgaben?",
      a = paste(
        "Die Dokumentation zu den Datensätzen findest du auf [pandaR](https://pandar.netlify.app/daten/datensaetze/)."
      )
    ),
    list(
      topic = "Hilfe",
      q = "Wo kann ich die Grundlagen von R üben?",
      a = paste(
        "Wenn dir für die R-Code-Aufgaben noch Grundlagen fehlen, kannst du sie z.B. mit",
        "[otter](https://meikesteinhilber.github.io/otter/) üben."
      )
    ),
    list(
      topic = "Hilfe",
      q = "Funktioniert tigeR auch auf dem Handy oder Tablet?",
      a = "Ja, die Ansicht passt sich der Bildschirmgröße an. Für ein optimales User Interface empfehlen wir dir jedoch die Nutzung eines Laptops."
    )
  )
}

# How many items of the pool only have a preliminary difficulty
# (item_provisional(), the item DB's irt_provisional column), for the FAQ.
# NULL if the pool has no such column or can't be read (the FAQ then just
# leaves the sentence out). Read when app_ui() is built, i.e. at app start: on
# ShinyProxy every session starts its own container, so it's always current.
provisional_item_count <- function(path = DB_ITEMS()) {
  # file.exists() first: opening a missing path would create an empty DB
  items <- if (file.exists(path)) tryCatch(db_get_items(path), error = function(e) NULL)
  if (is.null(items) || !"irt_provisional" %in% names(items)) {
    return(NULL)
  }
  c(n = sum(item_provisional(items)), total = nrow(items))
}

# An answer's Markdown as HTML, with every link opening in a new tab: in the
# same tab a click would leave tigeR and end the Shiny session.
faq_answer_html <- function(md) {
  gsub("<a href=", '<a target="_blank" rel="noopener" href=', render_md(md), fixed = TRUE)
}

# The FAQ as one collapsible list per topic (all closed), in FAQ_TOPICS order.
faq_section <- function(entries = faq_entries()) {
  topics <- intersect(FAQ_TOPICS, vapply(entries, `[[`, character(1), "topic"))
  lapply(topics, function(tp) {
    items <- Filter(function(e) identical(e$topic, tp), entries)
    div(
      class = "faq-topic",
      tags$h5(class = "faq-topic-title", tp),
      do.call(
        bslib::accordion,
        c(
          lapply(items, function(e) {
            bslib::accordion_panel(
              title = e$q,
              div(class = "faq-answer", shiny::HTML(faq_answer_html(e$a)))
            )
          }),
          list(open = FALSE, multiple = TRUE, class = "faq-accordion")
        )
      )
    )
  })
}

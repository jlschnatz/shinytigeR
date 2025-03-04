theme_tiger <- bslib::bs_theme(
  preset = "cosmo",
  "primary" = "#285f8a",
  "success" = "#285f8a",
  "danger" = "#D81B60",
 # navbar_bg = "green",
  "base_font" = bslib::font_google("Karla", wght = 300),
  "font-weight-bold" = 2000
) |>
  bslib::bs_add_rules(".accordion-button::after { width: 0mm; }") |> # make accordion icon disappear
  bslib::bs_add_rules(".accordion-button:not(.collapsed) { background-color: #285f8a;}") |>
  bslib::bs_add_rules(".accordion-button { background-color: #285f8a;}") |>
  bslib::bs_add_rules(".accordion-button:not(.collapsed) { color: #ffffff}") |>
  bslib::bs_add_rules(".accordion-button { color: #ffffff }") |>
  bslib::bs_add_rules(".navbar { background-color: #285f8a !important; }") 




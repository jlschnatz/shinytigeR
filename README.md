# shinytigeR

<img src="inst/app/www/img_app/tigeR_hex.png" alt="shinytigeR hex logo" width="150" align="right" />

**T**raining mit **i**ndividuell **ge**nerierten Erfolgs**r**ückmeldungen in R

A Shiny web application for Goethe University psychology students to practice
statistics and R. Students log in, filter and select multiple-choice practice
items, answer them, receive immediate automated feedback, and track their
progress via an IRT-based competency dashboard.

The app is structured as an R package (`shinytigeR`), with the Shiny app
itself living in `inst/app/`. It's deployed via Docker on ShinyProxy.

## Getting started

See [SETUP.md](SETUP.md) for local setup instructions (native R + `rv`, or a
Docker-based dev environment).

```r
rv run dev/run.R
```

## Documentation

[CLAUDE.md](CLAUDE.md) has the full architecture writeup: module structure,
database schemas, deployment process, and conventions for extending the app
(new item types, new learning areas, new modules).

## License

MIT — see [LICENSE](LICENSE).

## Contact

tiger@psych.uni-frankfurt.de

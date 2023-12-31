% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_display_item.R
\name{mod_display_item_server}
\alias{mod_display_item_server}
\title{display_item Server Functions}
\usage{
mod_display_item_server(
  id,
  data_item,
  index_display,
  check_button_value,
  credentials
)
}
\arguments{
\item{id}{A character string of the id of the module}

\item{data_item}{A dataframe that contains the item data from the item database}

\item{index_display}{A (reactive) vector of the indices of the filtered data (the output of mod_select_item_server)}
}
\value{
A named list with the following elements:

\itemize{
  \item A reactive vector `cur_answer_txt` of length 1L that contains the the text string of
   the current selected answeroption of the MC-item
  \item A reactive vector `cur_item_id` of length 1L that contains the current selected
   index of the answeroption
  \item A reactive vector `cur_answer_id` of length 1L that contains the current selected
   index of the selected answeroption (1-5)
}
}
\description{
The server part of the display_item module.
}

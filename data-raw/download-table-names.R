
# Save internal copy of table of TAI datasets
vars_et <- boulder::get_all_tables(local = FALSE, lang = "et")
vars_en <- boulder::get_all_tables(local = FALSE, lang = "en")
devtools::use_data(vars_et, vars_en, internal = TRUE, overwrite = TRUE)

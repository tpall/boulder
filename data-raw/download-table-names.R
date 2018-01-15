
# Save internal copy of table of TAI datasets
vars_et <- boulder::list_variables(lang = "et")
vars_en <- boulder::list_variables(lang = "en")
devtools::use_data(vars_et, vars_en, internal = TRUE, overwrite = TRUE)

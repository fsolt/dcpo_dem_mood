source_url("https://raw.githubusercontent.com/paulcbauer/flex_bib/master/replace_bib_braces.R")
source_url("https://raw.githubusercontent.com/paulcbauer/flex_bib/master/merge_bib_lines.R")
x <- stringi::stri_read_lines("dcpo_demsupport.bib")
x <- replace_bib_braces(x = x) # Replace (non-existent) quotes by braces
x  <- merge_bib_lines(x = x) # merge line breaks
stringi::stri_write_lines(x, "references_test.bib")

devtools::source_url("https://raw.githubusercontent.com/paulcbauer/flex_bib/master/flex_bib.R")
flex_bib(
    rmarkdown_file = "dcpo_demsupport.Rmd",
    bib_input = "references_test.bib",
    bib_output = "dcpo_demsupport.bib",
    by_sections = c("<!--- flex_bib appendix split -->"),
    repair = TRUE,
    removeISSN = TRUE,
    removeISBN = TRUE,
    removeDOI = TRUE,
    removeURL = TRUE
)

# some hand-editing still left to do *sigh*
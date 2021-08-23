# convert OSPAR COMPEAT json review file into an HTML table

# read in table info:
review <- jsonlite::read_json("Review/Review-Core-Data.json")

# create the rows of the table to create
table_df <-
  do.call(
    rbind,
    lapply(review$acceptances, function(acceptance) {
      data.frame(
        Title = acceptance$title,
        Checked = acceptance$checked,
        Accepted = acceptance$accepted,
        Exceptions = acceptance$exceptions,
        Actions = acceptance$actions,
        check.names = FALSE
      )
    })
  )

# render the document
rmarkdown::render(
  "Review/Review.Rmd",
  params = list(
    title = review$title,
    description = review$description,
    table_df = table_df
  )
)

devtools::load_all()

dataraw <- "data-raw"

lgadata <- 
  dataraw |>
  here::here("list_of_local_government_areas_of_nigeria-1729j.csv") |> 
  read.csv() |> 
  subset(select = -1) |> 
  transform(State = sub(" State", "", State)) |>     # note whitespace removed
  setNames(c("LGA", "State")) |> 
  tidyr::nest(LGA = LGA, .by = State) |> 
  transform(LGA = lapply(LGA, function(item) unname(unlist(item))))

with(lgadata, setNames(LGA, State)) |>
  jsonlite:::toJSON(pretty = TRUE) |>
  cat(file = here::here(dataraw, "lgas.json"))

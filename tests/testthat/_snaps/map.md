# Tables with > 2 columns are appropriately handled

    Code
      map_ng(data = dsl, x = value, plot = FALSE)
    Condition
      Warning:
      Multiple columns have regions, so the first was used
      Warning:
      Multiple columns have regions, so the first was used

# choropleth colour management

    Code
      eval_tidy(func)
    Condition
      Error in `.get_color_range()`:
      ! "Baba" is not a supported sequential palette

---

    Code
      eval_tidy(func)
    Condition
      Warning:
      Only the first element of `col` was used


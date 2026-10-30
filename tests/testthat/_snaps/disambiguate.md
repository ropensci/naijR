# inputs are validated

    Code
      disambiguate_lga("Bauchi")
    Condition
      Error in `disambiguate_lga()`:
      ! Expected an object of class `lgas`

---

    Code
      disambiguate_lga(lgas(c("Bauchi", "Kogi")))
    Condition
      Error in `disambiguate_lga()`:
      ! Disambiguation is only done for objects with one element

---

    Code
      disambiguate_lga(obilga, state = "Abia")
    Condition
      Error in `disambiguate_lga()`:
      ! Obi LGA is not in Abia State


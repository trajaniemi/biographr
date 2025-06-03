get_metadata_table <- function(data_set = "heart") {

  if (data_set == "heart") {
    meta <- data.frame(
      variable = c("sex", "age", "education", "smoking", "diabetes", "bp.meds", "high.bp", "cholesterol", "bp.sys", "bp.diast", "cigs.per.day", "bmi", "heart.rate", "glucose", "death", "heart.disease", "stroke"),
      type = c("nominal", "discrete", "nominal", "nominal", "nominal", "nominal", "nominal", "continuous", "continuous", "continuous", "discrete", "continuous", "continuous", "continuous", "nominal", "nominal", "nominal"),
      units = c(".", "years", ".", ".", ".", ".", ".", "mg/dL", "mmHg", "mmHg", ".", "kg/m^2", "beats per min", "mg/dL", ".", ".", "."),
      description = c("participant sex", "age at exam", "attained education", "current smoker at exam", "diabetic", "use of antihypertensive medication", "hypertensive (yes = systolic >=140 mmHg or diastolic >=90 mmHg)", "serum total cholesterol", "systolic blood pressure (mean of last 2 of 3 measurements)", "diastolic blood pressure (mean of last 2 of 3 measurements)", "number of cigarettes smoked each day", "body mass index", "heart rate", "casual serum glucose", "status at study followup", "heart disease during study follow up", "stroke during study follow up"),
      values = c("female, male", ".", "no high school diploma, high school, some college, college degree", "non-smoker, smoker", "not diabetic, diabetic", "no meds, bp meds", "normal, hypertensive",".",".",".",".",".",".",".","alive, dead","no heart disease, heart disease","no stroke, stroke")
    )
  } else if (data_set == "dunes") {
    meta <- data.frame(
      variable = c("transect", "zone", "veg", "distance", "windMax", "windAvg", "sunPercent", "tempSurf", "temp30", "moisture", "coverTotal", "coverDom", "coverShrub", "plfa", "saltSpray", "soilSalin", "availN", "totalN", "totalC", "CNratio"),
      type = c("discrete", "nominal", "nominal", "continuous", "continuous", "continuous", "continuous", "continuous", "continuous", "continuous", "continuous", "continuous", "continuous", "continuous", "continuous", "continuous", "continuous", "continuous", "continuous", "continuous"),
      units = c(".", ".", ".", "m", "mph", "mph", ".", "degrees C", "degrees C", ".", ".", ".", ".", "nmol", ".", ".", "ug/g", ".", ".", "."),
      description = c("transect #", "vegetation zone", "vegetation in plot", "distance to dune crest", "maximum wind speed measured over 1 minute", "average wind speed measured over 1 minute", "percent full sun at soil surface", "soil surface temperature", "soil temperature at 30 cm depth", "percent soil moisture", "percent cover of plants total", "percent cover of the dominant species (see zone)", "percent cover of shrubs", "mass of phospholid fatty acids; indicator of microbial biomass", "aboveground salt spray, standardized by day", "soil salinity, standardized by day", "soil nitrogen as nitrate + ammonia", "soil percent nitrogen by weight", "soil percent carbon by weight", "carbon:nitrogen ratio")
    )
  }

  meta_gt <- meta |>
    gt::gt(rowname_col = "variable") |>
    gt::cols_align("left") |>
    gt::opt_table_font(size = 12)


}

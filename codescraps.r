
#####Old Clean Table from Global
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$unemployment <- allzips$Unemp..Rate * 100
allzips$pubcov <- as.numeric(allzips$Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone)
allzips$medicare <- as.numeric(allzips$Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone)
allzips$va <- as.numeric(allzips$Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone)
allzips$medicaidexpansion <- as.numeric(allzips$Percent.Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold)
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
#row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
select(
City = city.x,
State = state.x,
Zipcode = zipcode,
Rank = rank,
Score = centile,
Superzip = superzip,
Population = adultpop,
College = college,
unemployment = Unemp..Rate,
Income = income,
Lat = latitude,
Long = longitude
)





#####Old Data Exlorer from Superzip
observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
      selected = stillSelected)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })

superzip <- read.csv(file="~/GitHub/congressImpact/data/superzip.csv")

superzip.dt <- data.table(superzip)



superCollapse <- superzip.dt[,
list(zipcode=rank[1],
number=rank[1],
X=rank[1],
centile=mean(na.rm=TRUE, as.numeric(as.vector(centile))),
superzip=mean(na.rm=TRUE, as.numeric(as.vector(superzip))),
rank=mean(na.rm=TRUE, as.numeric(as.vector(rank))),
city=rank[1],
adultpop=sum(na.rm=TRUE, as.numeric(as.vector(adultpop))),
households=sum(na.rm=TRUE, as.numeric(as.vector(households))),
college=mean(na.rm=TRUE, as.numeric(as.vector(college))),
income=mean(na.rm=TRUE, as.numeric(as.vector(income))),
State=rank[1],
state.x=rank[1],
Congressional.District=rank[1],
Unemp..Rate=mean(na.rm=TRUE, as.numeric(as.vector(Unemp..Rate))),
X..in.sample=sum(na.rm=TRUE, as.numeric(as.vector(X..in.sample))),
Total..Estimate..Civilian.noninstitutionalized.population=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..Civilian.noninstitutionalized.population))),
Total..Margin.of.Error..Civilian.noninstitutionalized.population=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..Civilian.noninstitutionalized.population))),
Public.Coverage..Estimate..Civilian.noninstitutionalized.population=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..Civilian.noninstitutionalized.population))),
Public.Coverage..Margin.of.Error..Civilian.noninstitutionalized.population=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..Civilian.noninstitutionalized.population))),
Percent.Public.Coverage..Estimate..Civilian.noninstitutionalized.population=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..Civilian.noninstitutionalized.population))),
Percent.Public.Coverage..Margin.of.Error..Civilian.noninstitutionalized.population=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..Civilian.noninstitutionalized.population))),
Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone))),
Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone))),
Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone=sum(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone))),
Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone=sum(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone))),
Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone))),
Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone))),
Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone))),
Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone))),
Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone))),
Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone))),
Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone))),
Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone))),
Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone))),
Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone))),
Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone))),
Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone))),
Total..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))),
Total..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))),
Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))),
Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))),
Percent.Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))),
Percent.Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))),
Total..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold))),
Total..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold))),
Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold))),
Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold))),
Percent.Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold))),
Percent.Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold))),
Total..Estimate..Worked.full.time..year.round..18.years.and.over.=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..Worked.full.time..year.round..18.years.and.over.))),
Total..Margin.of.Error..Worked.full.time..year.round..18.years.and.over.=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..Worked.full.time..year.round..18.years.and.over.))),
Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over.=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over.))),
Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over.=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over.))),
Percent.Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over.=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over.))),
Percent.Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over.=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over.))),
Total..Estimate..Worked.full.time..year.round..18.years.and.over....18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..Worked.full.time..year.round..18.years.and.over....18.to.64.years))),
Total..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....18.to.64.years))),
Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....18.to.64.years))),
Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....18.to.64.years))),
Percent.Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....18.to.64.years))),
Percent.Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....18.to.64.years))),
Total..Estimate..Worked.full.time..year.round..18.years.and.over....65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..Worked.full.time..year.round..18.years.and.over....65.years.and.over))),
Total..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....65.years.and.over))),
Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....65.years.and.over))),
Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....65.years.and.over))),
Percent.Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....65.years.and.over))),
Percent.Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....65.years.and.over))),
Total..Estimate..Under.6=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..Under.6))),
Total..Margin.of.Error..Under.6=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..Under.6))),
Public.Coverage..Estimate..Under.6=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..Under.6))),
Public.Coverage..Margin.of.Error..Under.6=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..Under.6))),
Percent.Public.Coverage..Estimate..Under.6=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..Under.6))),
Percent.Public.Coverage..Margin.of.Error..Under.6=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..Under.6))),
Total..Estimate..6.to.17.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..6.to.17.years))),
Total..Margin.of.Error..6.to.17.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..6.to.17.years))),
Public.Coverage..Estimate..6.to.17.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..6.to.17.years))),
Public.Coverage..Margin.of.Error..6.to.17.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..6.to.17.years))),
Percent.Public.Coverage..Estimate..6.to.17.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..6.to.17.years))),
Percent.Public.Coverage..Margin.of.Error..6.to.17.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..6.to.17.years))),
Total..Estimate..18.to.24.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..18.to.24.years))),
Total..Margin.of.Error..18.to.24.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..18.to.24.years))),
Public.Coverage..Estimate..18.to.24.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..18.to.24.years))),
Public.Coverage..Margin.of.Error..18.to.24.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..18.to.24.years))),
Percent.Public.Coverage..Estimate..18.to.24.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..18.to.24.years))),
Percent.Public.Coverage..Margin.of.Error..18.to.24.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..18.to.24.years))),
Total..Estimate..25.to.34.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..25.to.34.years))),
Total..Margin.of.Error..25.to.34.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..25.to.34.years))),
Public.Coverage..Estimate..25.to.34.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..25.to.34.years))),
Public.Coverage..Margin.of.Error..25.to.34.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..25.to.34.years))),
Percent.Public.Coverage..Estimate..25.to.34.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..25.to.34.years))),
Percent.Public.Coverage..Margin.of.Error..25.to.34.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..25.to.34.years))),
Total..Estimate..35.to.44.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..35.to.44.years))),
Total..Margin.of.Error..35.to.44.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..35.to.44.years))),
Public.Coverage..Estimate..35.to.44.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..35.to.44.years))),
Public.Coverage..Margin.of.Error..35.to.44.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..35.to.44.years))),
Percent.Public.Coverage..Estimate..35.to.44.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..35.to.44.years))),
Percent.Public.Coverage..Margin.of.Error..35.to.44.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..35.to.44.years))),
Total..Estimate..45.to.54.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..45.to.54.years))),
Total..Margin.of.Error..45.to.54.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..45.to.54.years))),
Public.Coverage..Estimate..45.to.54.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..45.to.54.years))),
Public.Coverage..Margin.of.Error..45.to.54.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..45.to.54.years))),
Percent.Public.Coverage..Estimate..45.to.54.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..45.to.54.years))),
Percent.Public.Coverage..Margin.of.Error..45.to.54.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..45.to.54.years))),
Total..Estimate..55.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..55.to.64.years))),
Total..Margin.of.Error..55.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..55.to.64.years))),
Public.Coverage..Estimate..55.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..55.to.64.years))),
Public.Coverage..Margin.of.Error..55.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..55.to.64.years))),
Percent.Public.Coverage..Estimate..55.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..55.to.64.years))),
Percent.Public.Coverage..Margin.of.Error..55.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..55.to.64.years))),
Total..Estimate..65.to.74.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..65.to.74.years))),
Total..Margin.of.Error..65.to.74.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..65.to.74.years))),
Public.Coverage..Estimate..65.to.74.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..65.to.74.years))),
Public.Coverage..Margin.of.Error..65.to.74.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..65.to.74.years))),
Percent.Public.Coverage..Estimate..65.to.74.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..65.to.74.years))),
Percent.Public.Coverage..Margin.of.Error..65.to.74.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..65.to.74.years))),
Total..Estimate..75.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..75.years.and.over))),
Total..Margin.of.Error..75.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..75.years.and.over))),
Public.Coverage..Estimate..75.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..75.years.and.over))),
Public.Coverage..Margin.of.Error..75.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..75.years.and.over))),
Percent.Public.Coverage..Estimate..75.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..75.years.and.over))),
Percent.Public.Coverage..Margin.of.Error..75.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..75.years.and.over))),
Total..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Total..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Percent.Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Percent.Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Total..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Total..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Percent.Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Percent.Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Total..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Total..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Percent.Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Percent.Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Total..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Total..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Percent.Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Percent.Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Total..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Total..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Percent.Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Percent.Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Total..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Total..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Percent.Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Percent.Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Total..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Total..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Percent.Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Percent.Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
Total..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Total..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Percent.Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Percent.Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
Total..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Total..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Percent.Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
Percent.Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over)))), by= list(districtcode, representative, firstname, middlename, lastname, party, phone, website, congress_office, bioguide_id, votesmart_id, fec_id, govtrack_id, crp_id, twitter_id, congresspedia_url, facebook_id, oc_email)]

superzip.simp <- as.data.frame(superCollapse)

states <- read.csv(file="/Users/lee/GitHub/congressImpact/data/States.csv")
superstates <- merge(states, superCollapse, by.x="districtcode", by.y="districtcode")

districtsforshp <- read.csv("/Users/lee/GitHub/congressImpact/data/DistrictsforSHP.csv")

US <- get_map(location="US", zoom=3, maptype="terrain")

setwd("/Users/lee/GitHub/congressImpact/")
district.map <- readOGR(dsn=path.expand("/Users/lee/GitHub/congressImpact/districts114"), layer="districts114")
district.map@data$districtcode <- districtsforshp$district.code
district.map@data = data.frame(district.map@data, superzip.simp[match(district.map@data[,"districtcode"], superzip.simp[,"districtcode"]),])
district.fortify <- fortify(district.map, by="districtcode")




test.plot <- ggmap(US) +
geom_polygon(data=district.map,  aes(long, lat, group=id), alpha=0.5) +
geom_path(colour="white") +
scale_x_continuous("Longitude", limits=c(-125, -60)) +
scale_y_continuous("Latitude", limits=c(20, 50)) +
coord_equal() +
coord_map() +
scale_fill_discrete("Generalized Districts") +
theme_classic()





district.map@data$districtcode <- districtsforshp$district.code
district.shp = fortify(district.map, Region="districtcode")
district.shp <- join(district.map@data, as.data.frame(superCollapse), by="districtcode")



iberia.map3@data$id = rownames(iberia.map3@data)
iberia.poly = fortify(iberia.map3, Region="id")
iberia.df2 = join(iberia.poly, iberia.map3@data, by="id")

iberia.bio.shp <- readOGR(dsn=".", layer="Sites")
iberia.bio.shp@data$id = rownames(iberia.bio.shp@data)
iberia.shp = fortify(iberia.bio.shp, Region="id")
iberia.bio.shp.df = join(iberia.shp, iberia.bio.shp@data, by="id")





iberia.plot.regions <- ggmap(spain) +
geom_polygon(data=iberia.df2,  aes(long, lat, group=group, fill=Region, map_id=Region), alpha=0.5) +
geom_point(aes(x=Dec_Long, y=Dec_Lat), data=iberia.bio.shp.frame) +
geom_path(colour="white") +
scale_x_continuous("Longitude", limits=c(-10, 4)) +
scale_y_continuous("Latitude", limits=c(35, 44)) +
coord_equal() +
coord_map() +
scale_fill_discrete("Generalized Regions") +
theme_classic()




#####Testing Ideas

republicans <- subset(allzips, party=="R")
democrats <- subset(allzips, party=="D")

republican.economy <- ggplot(republicans, aes(as.numeric(as.vector(college))~as.numeric(as.vector(Median.Income)))) +
geom_point() +
stat_smooth() +
theme_light()
republican.economy



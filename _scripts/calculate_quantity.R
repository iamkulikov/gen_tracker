library("dplyr")
library("reshape2")
library("countrycode")
library("readxl")
library("readr")
library("tidyr")
library("data.table")
library("writexl")
library("stringr")
library("gsubfn")
library("ggplot2")
library("hrbrthemes")

#### Import data
setwd("C:/Projects/generation_tracker")
fname <- "Age_quest.csv"
fullbase <- read_csv(fname)
fullbase <- fullbase %>% mutate(all = PopMale+PopFemale) %>% rename(male = PopMale, female = PopFemale, 

                                                                                                                                        year = Time, age = AgeGrpStart, country_name = Location)

#### Main function to filter the database and calculate number of people in a generation or their share in total population

countpeople_simple <- function(unfiltered, year_int_single,
                    type = c("num", "share_pop", "share_subpop", "share_gen"), 
                    who = c("all","male","female", "active"), 
                    country = "World", 
                    mode = c("became_before", "became_after", "were_when"), 
                    status = c("alive", "preschool", "school", "university", "army", "young", "adult", "active", "old", "custom_gen"), 
                    year_from = 1950, year_to = 2019, age_from = 0, age_to = 0) {
  
    # check input values
    type <- match.arg(type)
    who <- match.arg(who)
    mode <- match.arg(mode)
    status <- match.arg(status)
    if ( any(year_int_single %% 1 != 0) | year_from %% 1 != 0 | year_to %% 1 != 0 | any(year_int_single > 2019) | !(year_from %in% 1950:2019) | !(year_to %in% 1950:2019) ) {
      stop("year_int_single, year_from and year_to are meant to be integers between 1950 and 2019")
    }
    if ( age_from %% 1 != 0 | age_to %% 1 != 0 | !(age_from %in% 0:100) | !(age_to %in% 0:100) ) {
      stop("age_from and age_to are meant to be integers between 0 and 100")
    }
    if (length(year_int_single) != 1 | length(year_from) != 1 | length(year_to) != 1 | length(age_from) != 1 | length(age_to) != 1 | 
        length(type) != 1 | length(who) != 1 | length(country) != 1 | length(mode) != 1 | length(status) != 1 ) {
      stop("All inputs are expected to be length 1 in all modes except were_when - use function countpeople to vectorize -> 
           age_int might be longer in the were_when mode then")
    }
    
   # type = "share_pop"; who = "all"; country = "Germany"; mode = "became_after"; status = "custom_gen";
   # year_from = 1950; year_to = 2005; year_int = 1989; age_from = 0; age_to = 0
    
    # select needed country/year subset
    filtered <- unfiltered %>% filter(country_name == country, year <= year_to, year >= year_int_single) %>% 
      arrange(year, age) %>% mutate(shift = as.integer(year-year_int_single), year = as.integer(year))

    # calculate total population size by year (all genders)
    population <- filtered %>% group_by(year) %>% summarize(pop = sum(all)/1000)
    
    # select gender subset and calculate subpopulation size by year (chosen gender)
    if (who == "all") {filtered <- filtered %>% select(year, age, shift, all) %>% rename("num" = "all")}
    if (who == "male") {filtered <- filtered %>% select(year, age, shift, male) %>% rename("num" = "male")}
    if (who == "female") {filtered <- filtered %>% select(year, age, shift, female) %>% rename("num" = "female")}
    if (who == "active") {filtered <- filtered %>% select(year, age, shift, all) %>% filter(age>=18, age<=65) %>% rename("num" = "all")}
    subpopulation <- filtered %>% group_by(year) %>% summarize(subpop = sum(num)/1000)
    
    # filter to everyone older than the left boundary for a status
    filtered <- filtered %>%
      filter(age >= case_when(
        status == "alive" | status == "pre_school" | status == "young" ~ 0,
        status == "school" ~ 7,
        status == "university" | status == "army" | status == "adult" | status == "active" ~ 18,
        status == "old" ~ 65,
        status == "custom_gen" ~ age_from
      ))
    
    # count everyone older than the left boundary for a status
    older <- filtered %>% group_by(year) %>% summarize(older_count = sum(num, na.rm=T)/1000) 
    
    # perform a shift on ages
    filtered <- filtered %>% group_by(year) %>% mutate(numm = dplyr::lead(num, n = unique(shift))) %>% ungroup()
    
    # count those who achieved status before or precisely in year_int
    if (mode == "became_before" | mode == "became_after") {
      result <- filtered %>% group_by(year) %>% summarize(nummm = sum(numm, na.rm=T)/1000)
    }
    
    # count those who achieved status after year_int
    if (mode == "became_after") {
      result <- result %>% left_join(older, by="year") %>% mutate(nummm = older_count - nummm) %>% select(-older_count)
    }
    
    # count those who had the status in year_int
    if (mode == "were_when") {
      result <- filtered %>% filter(age <= case_when(
        status == "pre-school" ~ 6,
        status == "school" ~ 17,
        status == "university" ~ 22,
        status == "army" ~ 28,
        status == "young" ~ 20,
        status == "active" ~ 65,
        status == "alive" | status == "adult" | status == "old" ~ 100,
        status == "custom_gen" ~ age_to
      )) %>% group_by(year) %>% summarize(nummm = sum(numm, na.rm=T)/1000)
    }
    
    # calculate size of the generation at the start
    if (type == "share_gen") {generation <- result %>% filter(year == year_int) %>% select(nummm) %>% unlist() }
    
    # normalize when shares needed
    if (type == "share_pop") {result <- result %>% left_join(population, by="year") %>% mutate(nummm = 100*nummm/pop) %>% select(-pop)}
    if (type == "share_subpop") {result <- result %>% left_join(subpopulation, by="year") %>% mutate(nummm = 100*nummm/subpop) %>% select(-subpop)}
    if (type == "share_gen") {result <- result %>% mutate(nummm = 100*nummm/generation)}
    
    # finalize result
    result <- result %>% filter(year >= year_from)
    return(data.frame(result))
    
  }

adder <- function(vector, constant) {
  vector+constant
}

age_shift <- function(age_interval, years_int) {
  shifts <- cumsum(c(0, na.omit(years_int - lag(years_int))))
  res <- lapply(shifts, adder, vector = age_interval)
  for (i in seq_along(years_int)[-1]) {
    if (i==2) {res[[i]] <- list(unlist(res[[i-1]]),unlist(res[[i]]))} 
    if (i>2) {res[[i]] <- append(res[[i-1]],list(res[[i]]))} 
  }
  return(res)
}

#age_interval = c(0,4); years_int = c(1990, 1992, 1997)
#age_shift(age_interval, years_int)

simplify_intervals <- function(input) {
    
    ## list to dataframe
    df <- rbind(cbind(sapply(input,"[[",1),"1_left"),cbind(sapply(input,"[[",2),"0_right"))
    df <- data.frame(df) %>% rename(n = X1, t = X2) %>% mutate(n = as.numeric(n), counter = 1) 
    
    ## check inputs
    # input is a list of vectors
    # all vectors are length 2
    # vectors are arranged in ascending order
    
    df <- df %>% arrange(n,t)
  
    for (i in seq_along(df$n)[-1]) {
      df$counter[i] = df$counter[i-1]+ifelse(df$t[i]=="1_left",1,-1)
    }
    
    df <- df %>% mutate(counter_l = lag(counter), 
                        limits = case_when(
                          counter == 1 & counter_l == 0 ~ "start",
                          counter == 1 & is.na(counter_l) ~ "start",
                          counter == 0 & counter_l == 1 ~ "end")) %>% 
                filter(!is.na(limits)) %>%
                mutate(n_l = lag(n, default = -1000), n_f = lead(n, default = -1000)) %>%
                filter(n != n_l, n != n_f, !is.na(limits)) %>% mutate(n_f = lead(n)) %>%
                filter(limits == "start") %>% select(n, n_f)
    
    output <- as.list(as.data.frame(t(df)))
    output

  }
  
  #input2 <- list(c(1,4),c(3,6),c(6,8),c(10,11), c(9,12))
  #simplify_intervals(input2)

  
  


#### ------------------------------------- Deadends in development -----------------------------------

#### Countpeople_simple wrapper to deal with year_int of nonsingular length, but it doesn't sum up generations from different year_ints

countpeople <- function(unfiltered, year_int,
                               type = c("num", "share_pop", "share_subpop", "share_gen"), 
                               who = c("all","male","female", "active"), 
                               country = "World", 
                               mode = c("became_before", "became_after", "were_when"), 
                               status = c("alive", "preschool", "school", "university", "army", "young", "adult", "active", "old", "custom_gen"), 
                               year_from = 1950, year_to = 2019, age_from = 0, age_to = 0) {
  
  # check input values
  type <- match.arg(type)
  who <- match.arg(who)
  mode <- match.arg(mode)
  status <- match.arg(status)
  if ( any(year_int %% 1 != 0) | year_from %% 1 != 0 | year_to %% 1 != 0 | any(year_int > 2019) | !(year_from %in% 1950:2019) | !(year_to %in% 1950:2019) ) {
    stop("year_int_single, year_from and year_to are meant to be integers between 1950 and 2019")
  }
  if ( age_from %% 1 != 0 | age_to %% 1 != 0 | !(age_from %in% 0:100) | !(age_to %in% 0:100) ) {
    stop("age_from and age_to are meant to be integers between 0 and 100")
  }
  if (length(year_from) != 1 | length(year_to) != 1 | length(age_from) != 1 | length(age_to) != 1 | 
      length(type) != 1 | length(who) != 1 | length(country) != 1 | length(mode) != 1 | length(status) != 1 ) {
    stop("All inputs are expected to be length 1 in all modes except were_when - age_int might be longer then")
  }
  if (length(year_int) != 1 & mode != "were_when") {
    stop("year_int is expected to be length 1 in all modes except were_when")
  }
  
  if (length(year_int) == 1) {return(countpeople_simple(unfiltered = unfiltered, year_int_single = year_int, type = type, who = who,
                                    country = country, mode = mode, status = status, year_from = year_from, year_to = year_to, 
                                    age_from = age_from, age_to = age_to))}
  if (length(year_int) > 1) {
    for (i in seq_along(year_int)) {
      a <- countpeople_simple(unfiltered = unfiltered, year_int_single = year_int[i], type = type, who = who,
                         country = country, mode = mode, status = status, year_from = year_from, year_to = year_to, 
                         age_from = age_from, age_to = age_to)
      if (i != length(year_int)) {a <- a %>% filter(year<year_int[i+1])}
      if (i==1) {result <- a} else {result <- rbind(result, a)}
    }
    return(result)
  }
}

countpeople_simple(fullbase, type = "share_pop", who = "all", country = "Russian Federation", mode = "became_after", status = "custom_gen",
            year_from = 1950, year_to = 2015, year_int_single = 1989, age_from = 0, age_to = 10)

countpeople(fullbase, type = "share_pop", who = "all", country = "Russian Federation", mode = "became_after", status = "custom_gen",
                   year_from = 1950, year_to = 2015, year_int = 1989, age_from = 0, age_to = 10)

countpeople(fullbase, type = "share_pop", who = "all", country = "Russian Federation", mode = "were_when", status = "active",
            year_from = 1950, year_to = 2015, year_int = c(1989,2000), age_from = 0, age_to = 10)

#### Function to produce a single table from multiple queries

collect_queries <- function(unfiltered, year_int, type, who, country, mode, status, year_from, year_to, age_from, age_to) {
  for (i in seq_along(year_int)) {
    newcall <- countpeople(unfiltered, year_int = year_int[i], type = type[i], who = who[i],
                country = country[i], mode = mode[i], status = status[i], year_from = year_from[i], year_to = year_to[i],
                age_from = age_from[i], age_to = age_to[i]) %>%
                mutate(call_num = i)
    if (i==1) {output <- newcall} else { output <- rbind(output, newcall) }
  }
  output <- output %>% mutate(call_num = as.factor(call_num))
  return(output)
}

queries_collected <- collect_queries(fullbase, 
              type = c("share_pop","share_pop"), 
              who = c("all", "all"), 
              country = c("Russian Federation", "Germany"),
              mode = c("became_after", "became_after"),
              status = c("active","active"),
              year_from = c(1950, 1950), 
              year_to = c(2019, 2019), 
              year_int = c(1998, 1998),
              age_from = c(0, 0), 
              age_to = c(100, 100)
              )

ggplot(queries_collected, aes(year, nummm)) + geom_line(aes(group=call_num, color=call_num), alpha=1, size=2) + 
  theme_ipsum() + scale_y_continuous(limits = c(0,100))



genfact <- countpeople_simple(fullbase, type = "share_pop", who = "all", country = "Russian Federation", mode = "were_when", status = "custom_gen",
            year_from = 1950, year_to = 2015, year_int_single = 1989, age_from = 0, age_to = 10)

fullbase %>% filter(country_name == "Russian Federation", year %in% 2004:2005, age %in% 15:16)


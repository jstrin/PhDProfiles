#' Small Cell Suppression
#'
#' This function is designed to check for a minimum cell size
#' for reporting purposes. If the minimum cell size is not met,
#' categories will be iteratively grouped together to result in
#' reportable responses.
#'
#' @importFrom dplyr rowwise mutate filter group_by summarise case_when ungroup select
#' @import rlang
#' @importFrom tidyr left_join



##Sample Data Load




replace_basic <- function(n){
  if(is.numeric(n)){
    ifelse(n < 5, NA, n)
  }
}


dfOutPut2 <- dfOutPut %>%
  rowwise() %>%
  mutate(count = replace_basic(count))


###
#  replace basic will work on raw counts, but bigger issue is around grouping variables.
#   1. if grouping variable is ordinal, we want to collapse into simarly ordered groups (e.g., Agree+Strongly Agree)
#   2. if grouping variable is unordered,
#       a. we want to collapse smaller groups together -or-
#       b. collapse groups with similar meaning
#
#   2.a. is "easiest" to implement
#
###

replace_loop <-  function(df, collapse_var, sum_function, minCell = 5, ...) {


  cVar <- enquo(collapse_var)

  l.gVar <- quos(...)

  # Get count of grouping variables, used to trigger events

  gVar_counter <- length(l.gVar)

  # find out if there are any groups with fewer than the minimum cell size

  df_grouped <- df %>%
    filter( !( !!cVar) %in% "Other") %>%
    group_by(( !!cVar), !!!(l.gVar)) %>%
    summarise(count = n())



  if(min(df_grouped$count)<minCell){

    print(min(df_grouped$count))


    #create the key variable based on collapsing and grouping variables


      df <- mutate(df, xy = paste(( !!cVar), UQS(l.gVar), sep = "_"))




    df_prime <- df %>%
      group_by(xy, ( !!cVar), UQS(l.gVar)) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      mutate(collapse_out = ifelse(count < minCell, "Other", xy)) %>%
      select(xy, collapse_out, UQS(quo))



    l.groupNames <- sapply(l.gVar, quo_name)


    names(df_prime) <- c("xy", quo_name(cVar), l.groupNames)


    # remove the original collapse variable and replace with the modified collapse values

    df <- df %>%
      select(- ( !!cVar) ) %>%
      left_join(., df_prime, by = c("xy", l.groupNames))



      }else{

        if("Other" %in% unlist(select(df, matches(collapse_var)))){

          df_otherCheck <- df %>%
            group_by_(.dots = c(collapse_var, l.grouping_vars)) %>%
            summarise(count = n()) %>%
            ungroup()

          if(min(df_otherCheck$count)<minCell){

           #need to identify l.grouping_vars where collapse_var == Other

            df_otherCorrection <- df_otherCheck %>%
              filter_(.dots = filter_out_other) %>%
              slice(min(count))

          }

        }else{

          #specify sum_functiona and sum_variable

          #not working (lines 174 - 193)

          qFunc <- enquo(sum_function)
          qVar <- enquo(sum_var)

                    sumFunc <- if_else(quo_text(qFunc) %in% c("n()"),
                            qFunc,
                            enquo(sum_function (( !!qVar))))


                    qFunc <- enquo(sum_function)
                    qVar <- enquo(sum_var)

                    sumFunc <- if_else(quo_text(qFunc) %in% c("n()"),
                                       qFunc,
                                       FALSE)




    df_out <- df %>%
      group_by(( !!cVar), ( !!gVar1), ( !!gVar2), ( !!gVar3), ( !!gVar4) ) %>%
      summarise( outVar = ( !!qFunc(( !!qVar))))

      }

      }

  return(df_out)
}

#' @examples
#' library(nycflights13)
#' dfRaw <- flights
#'
#' dfOutPut <- dfRaw %>%
#'   group_by(month, carrier) %>%
#'     summarise(count = n())
#'
#' temp <- replace_loop(dfRaw, carrier, sum_function = "n()", minCell = 5, month, year, arr_time)
#' l.temp <- list( c("n()", "count"),  c("sum(count, na.rm=T)", "sum"))
#' dfTemp <- dfOutPut %>%
#'      ungroup() %>%
#'      group_by(month) %>%
#'      summarise_(.dots = setNames(l.temp[[1]][1], l.temp[[1]][2]))


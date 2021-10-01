Data tidy
================

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

\#\#’pivot)longer load the pulse data

``` r
pulse_data = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>%
  janitor::clean_names()
```

    ## Warning in FUN(X[[i]], ...): strings not representable in native encoding will
    ## be translated to UTF-8

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00C4>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00D6>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00E4>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00F6>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00DF>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00C6>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00E6>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00D8>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00F8>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00C5>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00E5>' to native encoding

Wide format to long format…

``` r
pulse_data_tidy = 
  pulse_data%>%
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",#combine several bdi lines
    names_prefix = "bdi_score_",#delete prefix
    values_to = "bdi"#give a column name to the value of several bdi columns.
)%>%
  relocate(id,visit)%>%
  mutate(
    visit = replace(visit, visit =="bl","00m"),#replace bl with 00m
    visit = factor(visit)
  )
```

\#\#pivot\_wider makeup a results data table

``` r
analysis_df =
  tibble(
    group = c("treatment","treatment","control","control"),
    time = c("pre","post","pre","post"),
    group_mean = c(4, 8, 3, 6)
  )

analysis_df1 = analysis_df %>%
   pivot_wider(
     names_from = "time",
     values_from = "group_mean"
   )
# knitr::kable()#create a formal table of Makrdown
```

(what abou ‘bind.rows()’) \#\#bind\_rows import LotR

``` r
library(readxl)

fellowship_df = 
  read_excel("data/LotR_Words.xlsx", range = "B3:D6")%>%
  mutate(movie = "fellowship_rings")

two_towers_df = 
  read_excel("data/LotR_Words.xlsx", range = "F3:H6")%>%
  mutate(movie = "two_towers")

return_df = read_excel("data/LotR_Words.xlsx", range = "J3:L6")%>%
  mutate(movie = "return_king")#没跟上

lotr_df =
  bind_rows(fellowship_df, two_towers_df, return_df)%>%
  janitor::clean_names()%>%
  relocate(movie)%>%
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words"
  )
```

\#\#joining dataset Import and clean the FAS datasets.

``` r
pups_df =
  read_csv("./data/FAS_pups.csv")%>%
  janitor::clean_names()%>%
  mutate(sex = recode(sex,'1' = "male", '2' = "female"))
```

    ## Rows: 313 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_df =
  read_csv("./data/FAS_litters.csv")%>%
  janitor::clean_names()%>%
  relocate(litter_number)%>%
  separate(group, into =c("dose", "day_of_tx"), sep = 3)
```

    ## Rows: 49 Columns: 8

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fas_df =
  left_join(pups_df, litters_df, by = "litter_number")%>%
  arrange(litter_number)%>%
  relocate(litter_number, dose, day_of_tx)
```

``` r
litters_df=
  read_csv("data/FAS_litters.csv")%>%
janitor::clean_names()
```

    ## Rows: 49 Columns: 8

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pups_df =
  read_csv("data/FAS_pups.csv")%>%
  janitor::clean_names()
```

    ## Rows: 313 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

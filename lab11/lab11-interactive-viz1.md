Lab 11 - Interactive Visualization
================

# Learning Goals

- Read in and process Starbucks data.
- Create interactive visualizations of different types using `plot_ly()`
  and `ggplotly()`.
- Customize the hoverinfo and other plot features.
- Create a Choropleth map using `plot_geo()`.

# Lab Description

We will work with two Starbucks datasets, one on the store locations
(global) and one for the nutritional data for their food and drink
items. We will do some text analysis of the menu items.

# Deliverables

Upload an html file to Quercus and make sure the figures remain
interactive.

# Steps

### 0. Install and load libraries

### 1. Read in the data

- There are 4 datasets to read in, Starbucks locations, Starbucks
  nutrition, US population by state, and US state abbreviations. All of
  them are on the course GitHub.

``` r
sb_locs <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/starbucks-locations.csv")
```

    ## Rows: 25600 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (10): Store Number, Store Name, Ownership Type, Street Address, City, St...
    ## dbl  (2): Longitude, Latitude
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sb_nutr <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/starbucks-menu-nutrition.csv")
```

    ## Rows: 205 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Item, Category
    ## dbl (5): Calories, Fat (g), Carb. (g), Fiber (g), Protein (g)
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
usa_pop <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/us_state_pop.csv")
```

    ## Rows: 55 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): state
    ## dbl (1): population
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
usa_states<-read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/refs/heads/main/data/starbucks/states.csv")
```

    ## Rows: 51 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): State, Abbreviation
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### 2. Look at the data

- Inspect each dataset to look at variable names and ensure it was
  imported correctly.

``` r
head(sb_locs)
```

    ## # A tibble: 6 × 12
    ##   `Store Number` `Store Name`            `Ownership Type` `Street Address` City 
    ##   <chr>          <chr>                   <chr>            <chr>            <chr>
    ## 1 47370-257954   Meritxell, 96           Licensed         Av. Meritxell, … Ando…
    ## 2 22331-212325   Ajman Drive Thru        Licensed         1 Street 69, Al… Ajman
    ## 3 47089-256771   Dana Mall               Licensed         Sheikh Khalifa … Ajman
    ## 4 22126-218024   Twofour 54              Licensed         Al Salam Street  Abu …
    ## 5 17127-178586   Al Ain Tower            Licensed         Khaldiya Area, … Abu …
    ## 6 17688-182164   Dalma Mall, Ground Flo… Licensed         Dalma Mall, Mus… Abu …
    ## # ℹ 7 more variables: `State/Province` <chr>, Country <chr>, Postcode <chr>,
    ## #   `Phone Number` <chr>, Timezone <chr>, Longitude <dbl>, Latitude <dbl>

``` r
head(sb_nutr)
```

    ## # A tibble: 6 × 7
    ##   Item         Category Calories `Fat (g)` `Carb. (g)` `Fiber (g)` `Protein (g)`
    ##   <chr>        <chr>       <dbl>     <dbl>       <dbl>       <dbl>         <dbl>
    ## 1 Chonga Bagel Food          300         5          50           3            12
    ## 2 8-Grain Roll Food          380         6          70           7            10
    ## 3 Almond Croi… Food          410        22          45           3            10
    ## 4 Apple Fritt… Food          460        23          56           2             7
    ## 5 Banana Nut … Food          420        22          52           2             6
    ## 6 Blueberry M… Food          380        16          53           1             6

``` r
head(usa_pop)
```

    ## # A tibble: 6 × 2
    ##   state      population
    ##   <chr>           <dbl>
    ## 1 Alabama       4779736
    ## 2 Alaska         710231
    ## 3 Arizona       6392017
    ## 4 Arkansas      2915918
    ## 5 California   37253956
    ## 6 Colorado      5029196

``` r
head(usa_states)
```

    ## # A tibble: 6 × 2
    ##   State      Abbreviation
    ##   <chr>      <chr>       
    ## 1 Alabama    AL          
    ## 2 Alaska     AK          
    ## 3 Arizona    AZ          
    ## 4 Arkansas   AR          
    ## 5 California CA          
    ## 6 Colorado   CO

- `sb_locs` = Starbucks locations, including their street address, the
  city, the state/province, and longitude and latitude
- `sb_nutr` = the nutrient information of the items on the Starbucks
  menus, including calories, fat, car, fiber, protein
- `usa_pop` = population in the different states in usa
- `usa_states` = abbreviations of all the us states

### 3. Format and merge the data

- Subset Starbucks data to the US.
- Create counts of Starbucks stores by state.
- Merge population in with the store count by state.
- Inspect the range values for each variable.

``` r
sb_usa <- sb_locs |> filter(Country == "US")

sb_locs_state <- sb_usa |>
  group_by("State/Province") |>
  rename(state = "State/Province") |>
  group_by(state) |>
  summarize(n_stores = n())

# need state abbreviations
usa_pop_abbr <- 
  full_join(usa_pop, usa_states,
            by = join_by(state == State)
            ) 
  
sb_locs_state <- full_join(sb_locs_state, usa_pop_abbr,
                           by = join_by(state == Abbreviation))
  

summary(sb_locs_state)
```

    ##     state              n_stores        state.y            population      
    ##  Length:55          Min.   :   8.0   Length:55          Min.   :   56882  
    ##  Class :character   1st Qu.:  56.5   Class :character   1st Qu.: 1344331  
    ##  Mode  :character   Median : 123.0   Mode  :character   Median : 3751351  
    ##                     Mean   : 266.8                      Mean   : 5677621  
    ##                     3rd Qu.: 332.0                      3rd Qu.: 6515716  
    ##                     Max.   :2821.0                      Max.   :37253956  
    ##                     NA's   :4

For the numeric values, the number of stores ranges from 8 stores per
state/province to 2821 stores per state/province. The population ranges
from 56882 to 37253956. The other variables are of type character.

### 4. Use `ggplotly` for EDA

Answer the following questions:

- Are the number of Starbucks proportional to the population of a state?
  (scatterplot)

- Is the caloric distribution of Starbucks menu items different for
  drinks and food? (histogram)

- What are the top 20 words in Starbucks menu items? (bar plot)

``` r
p1 <- ggplot(sb_locs_state, aes(x = population, y = n_stores,
                                colour = state)) +
  geom_point(alpha = 0.8) +
  theme_bw() 


p1 <- ggplotly(p1)

frameWidget(p1)
```


[View interactive plot](lab11-interactive-viz1_files/figure-gfm/widgets/plotly_libs/widget_unamed-chunk-9.html)


- 4a) Answer:

The plot does appear to show a general trend, where as the population
grows, the number of Starbucks stores also increases. However, the
correlation here is not strictly linear, as some states (like the state
with the abbreviation WA) have more stores than expected based on their
population. But overall, yes, the number of Starbucks stores is
correlated with population, but not perfectly proportional.

``` r
p2 <- ggplot(sb_nutr, aes(x = Calories, fill = Category)) +
  geom_histogram(alpha = 0.6) +
  theme_bw() 

p2<- ggplotly(p2)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

``` r
frameWidget(p2)
```

<div id="htmlwidget-8937379017f2608d0cb4" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-8937379017f2608d0cb4">{"x":{"url":"lab11-interactive-viz1_files/figure-gfm//widgets/widget_unnamed-chunk-6.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

- 4b) Answer:

Yes, the caloric distribution of Starbucks menu items is different for
drinks and food. The majority of drinks have a lower calorie count,
clustering between 0 calories and 300 calories, whereas foods mainly
cluster between 100 calories all the way to 600 calories. So, drinks
tend to have a lower caloric range, while food items generally have
higher calorie values.

``` r
p3 <- sb_nutr |> 
  unnest_tokens(word, Item, token = "words") |>
  count(word, sort = T) |>
  head(20) |>
  ggplot(aes(fct_reorder(word, n), n)) +
  geom_col() +
  coord_flip() +
  theme_bw()

p3 <- ggplotly(p3)

frameWidget(p3)
```

<div id="htmlwidget-8213550ed480f2142815" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-8213550ed480f2142815">{"x":{"url":"lab11-interactive-viz1_files/figure-gfm//widgets/widget_unnamed-chunk-7.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

- 4c) Answer:

The top word is `iced`, as it is seen 32 times in the `sb_nutr` dataset.
`tazo`, `bottled`, `sandwich`, and `chocolate` are also in the top 20
words.

### 5. Scatterplots using `plot_ly()`

- Create a scatterplot using `plot_ly()` representing the relationship
  between calories and carbs. Color the points by category (food or
  beverage). Is there a relationship, and do food or beverages tend to
  have more calories?

``` r
p5 <- sb_nutr |>
  plot_ly(x = ~Calories, y = ~`Carb. (g)`, 
          type = "scatter", mode = "markers", color = ~Category)
  

frameWidget(p5)
```

    ## Warning in RColorBrewer::brewer.pal(N, "Set2"): minimal value for n is 3, returning requested palette with 3 different levels
    ## Warning in RColorBrewer::brewer.pal(N, "Set2"): minimal value for n is 3, returning requested palette with 3 different levels

<div id="htmlwidget-dc4dddc510a5b8f40bac" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-dc4dddc510a5b8f40bac">{"x":{"url":"lab11-interactive-viz1_files/figure-gfm//widgets/widget_unnamed-chunk-8.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

- 5a) Answer:

Yes, there is a relationship between calories and carbs, as in the
scatterplot, generally higher caolories lead to higher carbs. Food tends
to have more calories than drinks.

- Repeat this scatterplot but for the items that include the top 10
  words. Color again by category, and add hoverinfo specifying the word
  in the item name. Add layout information to title the chart and the
  axes, and enable `hovermode = "compare"`.
- What are the top 10 words and is the plot much different than above?

``` r
topwords <- sb_nutr |> 
  unnest_tokens(word, Item, token = "words") |>
  group_by(word) |>
  summarise(word_frequency = n()) |>
  arrange(across(word_frequency, desc)) |>
  head(10)
  

p5_2 <- sb_nutr |> 
  unnest_tokens(word, Item, token = "words") |>
  filter(word %in% topwords$word) |>
  plot_ly(
    x = ~Calories,
    y = ~`Carb. (g)`,
    type = "scatter", 
    mode = "markers",
    color = ~Category,
    hoverinfo  = "text",
    text = ~paste0("Item: ", word)
  ) |>
  layout(
    title = "Cal vs Carbs",
    xaxis = list(title = "Calories"),
    yaxis  = list(title = "Carbs"),
    hovermode = "compare"
    
  )

frameWidget(p5_2)
```

    ## Warning in RColorBrewer::brewer.pal(N, "Set2"): minimal value for n is 3, returning requested palette with 3 different levels
    ## Warning in RColorBrewer::brewer.pal(N, "Set2"): minimal value for n is 3, returning requested palette with 3 different levels

<div id="htmlwidget-8338206355c241f5b870" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-8338206355c241f5b870">{"x":{"url":"lab11-interactive-viz1_files/figure-gfm//widgets/widget_unnamed-chunk-9.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

- 5b) Answer:

### 6. `plot_ly` Boxplots

- Create a boxplot of all of the nutritional variables in groups by the
  10 item words.
- Which top word has the most calories? Which top word has the most
  protein?

``` r
sb_nutr_long <- sb_nutr |>
  unnest_tokens(word, Item, token = "words") |>
  filter(word %in% topwords$word) |>
  pivot_longer(cols = c(Calories, `Fat (g)`, `Carb. (g)`,
                        `Fiber (g)`, `Protein (g)`), names_to = "Nutrient", values_to = "value")


p6 <- plot_ly(data = sb_nutr_long, 
        x = ~word,
        y = ~value, 
        color = ~Nutrient,
        type = "box" ) |>
  layout(
    title = "Nutrient values for the top 10 word items",
    xaxis = list(title = "Item word"),
    yaxis = list(title = "Nutritional Value")
  )
frameWidget(p6)
```

<div id="htmlwidget-5c427d4486023d67026d" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-5c427d4486023d67026d">{"x":{"url":"lab11-interactive-viz1_files/figure-gfm//widgets/widget_unnamed-chunk-10.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

- 6)  Answer:

The top word that has the most calories are sandwiches, as their median
is the highest (460), and their tails are the highest - indicating that
in general they have more calories compared to the other words. The
words that have the highest protein are sandwich and egg. Both their
medians are pretty similar (19 and 18.5 respectively). The box (and the
tails) for the word sandwich are larger, indicating more variation
compared to the protein amount in the word egg.

### 7. 3D Scatterplot

- Create a 3D scatterplot between Calories, Carbs, and Protein for the
  items containing the top 10 words
- Do you see any patterns (clusters or trends)?

``` r
sb_nutr_long <- sb_nutr |>
  unnest_tokens(word, Item, token = "words") |>
  filter(word %in% topwords$word[1:10]) |>
  plot_ly(
    x = ~Calories,
    y = ~`Carb. (g)`,
    z = ~`Protein (g)`,
    color = ~word,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 5)) |>
  
    layout(
      title = "3D scatterplot of Calories, Carbs, and Protein",
      scene = list(
        xaxis = list(title = "Calories"),
        yaxis = list(title = "Carbohydrates (g)"),
        zaxis = list(title = "Protein (g)")
      )
    )
  
sb_nutr_long
```

    ## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
    ## Returning the palette you asked for with that many colors
    ## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
    ## Returning the palette you asked for with that many colors

<div class="plotly html-widget html-fill-item" id="htmlwidget-307b6ffed2ad3d536edf" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-307b6ffed2ad3d536edf">{"x":{"visdat":{"a97440e6906":["function () ","plotlyVisDat"]},"cur_data":"a97440e6906","attrs":{"a97440e6906":{"x":{},"y":{},"z":{},"mode":"markers","marker":{"size":5},"color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter3d"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"3D scatterplot of Calories, Carbs, and Protein","scene":{"xaxis":{"title":"Calories"},"yaxis":{"title":"Carbohydrates (g)"},"zaxis":{"title":"Protein (g)"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[420,30,60,60,70,140,140,140,140,150,150],"y":[42,8,15,15,17,35,35,35,35,38,38],"z":[27,0,0,0,0,0,0,0,0,0,0],"mode":"markers","marker":{"color":"rgba(102,194,165,1)","size":5,"line":{"color":"rgba(102,194,165,1)"}},"type":"scatter3d","name":"black","textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"line":{"color":"rgba(102,194,165,1)"},"frame":null},{"x":[60,60,60,60,70,70,120,120,120,120,140,140,140,140,140,140,140,140,140,140,150,150,150,150,150,150],"y":[15,15,15,15,17,17,31,31,31,31,35,35,35,35,35,35,35,35,35,35,38,37,38,38,37,38],"z":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"mode":"markers","marker":{"color":"rgba(228,156,113,1)","size":5,"line":{"color":"rgba(228,156,113,1)"}},"type":"scatter3d","name":"bottled","textfont":{"color":"rgba(228,156,113,1)"},"error_y":{"color":"rgba(228,156,113,1)"},"error_x":{"color":"rgba(228,156,113,1)"},"line":{"color":"rgba(228,156,113,1)"},"frame":null},{"x":[170,310,440,330,390,490,410,490,440,160,180,180,210,300,320,320,360,430],"y":[30,42,60,38,43,64,46,55,54,23,23,12,33,47,47,53,53,45],"z":[2,4,7,6,7,6,6,7,5,2,2,4,20,10,14,20,14,12],"mode":"markers","marker":{"color":"rgba(201,152,157,1)","size":5,"line":{"color":"rgba(201,152,157,1)"}},"type":"scatter3d","name":"chocolate","textfont":{"color":"rgba(201,152,157,1)"},"error_y":{"color":"rgba(201,152,157,1)"},"error_x":{"color":"rgba(201,152,157,1)"},"line":{"color":"rgba(201,152,157,1)"},"frame":null},{"x":[390,370,0,5,10,10,50,60,110,110,140,280,350],"y":[57,67,0,0,2,0,11,13,24,24,28,60,64],"z":[5,5,0,0,1,1,1,1,3,3,4,4,15],"mode":"markers","marker":{"color":"rgba(175,154,200,1)","size":5,"line":{"color":"rgba(175,154,200,1)"}},"type":"scatter3d","name":"coffee","textfont":{"color":"rgba(175,154,200,1)"},"error_y":{"color":"rgba(175,154,200,1)"},"error_x":{"color":"rgba(175,154,200,1)"},"line":{"color":"rgba(175,154,200,1)"},"frame":null},{"x":[370,490,230,500,410,450,310,170,170,500,290,480],"y":[32,40,28,41,43,42,9,13,13,35,33,42],"z":[18,21,16,15,21,24,19,13,13,26,19,16],"mode":"markers","marker":{"color":"rgba(226,148,184,1)","size":5,"line":{"color":"rgba(226,148,184,1)"}},"type":"scatter3d","name":"egg","textfont":{"color":"rgba(226,148,184,1)"},"error_y":{"color":"rgba(226,148,184,1)"},"error_x":{"color":"rgba(226,148,184,1)"},"line":{"color":"rgba(226,148,184,1)"},"frame":null},{"x":[470,0,5,10,30,30,30,50,60,60,60,60,70,70,70,70,90,120,120,130,130,130,130,130,140,140,190,200,230,250,260,300],"y":[68,0,0,2,8,8,8,11,15,15,13,15,17,17,17,17,24,31,31,21,21,21,21,13,23,23,30,34,36,37,34,47],"z":[6,0,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,5,5,5,5,8,5,5,7,7,9,10,11,10],"mode":"markers","marker":{"color":"rgba(176,208,99,1)","size":5,"line":{"color":"rgba(176,208,99,1)"}},"type":"scatter3d","name":"iced","textfont":{"color":"rgba(176,208,99,1)"},"error_y":{"color":"rgba(176,208,99,1)"},"error_x":{"color":"rgba(176,208,99,1)"},"line":{"color":"rgba(176,208,99,1)"},"frame":null},{"x":[600,300,430,300,360,600,530,350,500,370,280,490,230,500,450,500,470,480,460],"y":[80,34,34,43,52,47,44,31,46,32,18,40,28,41,42,35,35,42,31],"z":[19,11,21,19,22,32,23,16,18,18,18,21,16,15,24,26,21,16,29],"mode":"markers","marker":{"color":"rgba(227,217,62,1)","size":5,"line":{"color":"rgba(227,217,62,1)"}},"type":"scatter3d","name":"sandwich","textfont":{"color":"rgba(227,217,62,1)"},"error_y":{"color":"rgba(227,217,62,1)"},"error_x":{"color":"rgba(227,217,62,1)"},"line":{"color":"rgba(227,217,62,1)"},"frame":null},{"x":[10,45,45,50,60,60,80,90,90,200,210,430],"y":[2,11,5,11,14,13,18,27,27,34,33,45],"z":[1,0,3,1,0,1,0,0,0,20,20,12],"mode":"markers","marker":{"color":"rgba(245,207,100,1)","size":5,"line":{"color":"rgba(245,207,100,1)"}},"type":"scatter3d","name":"starbucks","textfont":{"color":"rgba(245,207,100,1)"},"error_y":{"color":"rgba(245,207,100,1)"},"error_x":{"color":"rgba(245,207,100,1)"},"line":{"color":"rgba(245,207,100,1)"},"frame":null},{"x":[60,60,60,60,70,70,120,120,120,120,140,140,140,140,140,140,140,140,140,140,150,150,150,150,150,150],"y":[15,15,15,15,17,17,31,31,31,31,35,35,35,35,35,35,35,35,35,35,38,37,38,38,37,38],"z":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"mode":"markers","marker":{"color":"rgba(219,192,155,1)","size":5,"line":{"color":"rgba(219,192,155,1)"}},"type":"scatter3d","name":"tazo","textfont":{"color":"rgba(219,192,155,1)"},"error_y":{"color":"rgba(219,192,155,1)"},"error_x":{"color":"rgba(219,192,155,1)"},"line":{"color":"rgba(219,192,155,1)"},"frame":null},{"x":[30,30,30,60,60,60,70,70,80,90,120,120],"y":[8,8,8,15,15,15,17,17,19,24,31,31],"z":[0,0,0,0,0,0,0,0,0,0,0,0],"mode":"markers","marker":{"color":"rgba(179,179,179,1)","size":5,"line":{"color":"rgba(179,179,179,1)"}},"type":"scatter3d","name":"tea","textfont":{"color":"rgba(179,179,179,1)"},"error_y":{"color":"rgba(179,179,179,1)"},"error_x":{"color":"rgba(179,179,179,1)"},"line":{"color":"rgba(179,179,179,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

``` r
frameWidget(sb_nutr_long)
```

    ## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
    ## Returning the palette you asked for with that many colors
    ## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
    ## Returning the palette you asked for with that many colors

<div id="htmlwidget-0ac9a16ae4126492f7b0" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-0ac9a16ae4126492f7b0">{"x":{"url":"lab11-interactive-viz1_files/figure-gfm//widgets/widget_unnamed-chunk-11.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

- 7)  Answer:

There seems to be a very weak overall trend (as it is quite spread out).
For the most part, I can see that higher calories generally lead to
higher carbs and protein. So yes, there does seem to be an overall
trend.

### 8. `plot_ly` Map

- Create a map to visualize the number of stores per state, and another
  for the population by state. Add custom hover text. Use subplot to put
  the maps side by side.
- Describe the differences if any.

``` r
# Set up mapping details
set_map_details <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('steelblue')
)

# Make sure both maps are on the same color scale
shadeLimit <- 125

# Create hover text
sb_locs_state$hover <- with(sb_locs_state, paste("Number of Starbucks: ", n_stores, '<br>', "State: ", state.y, '<br>', "Population: ", population))

# Create the map
map1 <- plot_geo(sb_locs_state, locationmode = "USA-states") |>
  add_trace(z = ~n_stores, text = ~hover, locations = ~state, color = ~n_stores, colors = "Purples") |>
  layout(
    title = "starbucks store by state", geo = set_map_details)
map1
```

    ## Warning: Ignoring 4 observations

<div class="plotly html-widget html-fill-item" id="htmlwidget-e1730b000d6768a065a6" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-e1730b000d6768a065a6">{"x":{"visdat":{"a9747521aeee":["function () ","plotlyVisDat"]},"cur_data":"a9747521aeee","attrs":{"a9747521aeee":{"locationmode":"USA-states","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"z":{},"text":{},"locations":{},"color":{},"colors":"Purples","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"mapType":"geo","title":"starbucks store by state","geo":{"domain":{"x":[0,1],"y":[0,1]},"scope":"usa","projection":{"type":"albers usa"},"showlakes":true,"lakecolor":"rgba(70,130,180,1)"},"scene":{"zaxis":{"title":"n_stores"}},"hovermode":"closest","showlegend":false,"legend":{"yanchor":"top","y":0.5}},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"colorbar":{"title":"n_stores","ticklen":2,"len":0.5,"lenmode":"fraction","y":1,"yanchor":"top"},"colorscale":[["0","rgba(252,251,253,1)"],["0.0416666666666667","rgba(248,246,250,1)"],["0.0833333333333333","rgba(243,242,248,1)"],["0.125","rgba(239,237,245,1)"],["0.166666666666667","rgba(232,231,242,1)"],["0.208333333333333","rgba(225,224,238,1)"],["0.25","rgba(218,218,235,1)"],["0.291666666666667","rgba(208,208,230,1)"],["0.333333333333333","rgba(198,199,225,1)"],["0.375","rgba(188,189,220,1)"],["0.416666666666667","rgba(178,177,213,1)"],["0.458333333333333","rgba(168,166,207,1)"],["0.5","rgba(158,154,200,1)"],["0.541666666666667","rgba(148,144,195,1)"],["0.583333333333333","rgba(138,135,191,1)"],["0.625","rgba(128,125,186,1)"],["0.666666666666667","rgba(121,110,178,1)"],["0.708333333333333","rgba(114,96,171,1)"],["0.75","rgba(106,81,163,1)"],["0.791666666666667","rgba(99,67,156,1)"],["0.833333333333333","rgba(92,54,150,1)"],["0.875","rgba(84,39,143,1)"],["0.916666666666667","rgba(77,28,137,1)"],["0.958333333333333","rgba(70,16,131,1)"],["1","rgba(63,0,125,1)"]],"showscale":true,"locationmode":"USA-states","z":[49,85,55,488,2821,481,123,91,25,694,326,99,89,67,575,221,94,116,84,273,257,30,283,184,188,32,36,338,13,58,29,261,76,253,645,378,79,359,357,27,131,25,180,1042,101,432,8,757,145,25,23],"text":["Number of Starbucks:  49 <br> State:  Alaska <br> Population:  710231","Number of Starbucks:  85 <br> State:  Alabama <br> Population:  4779736","Number of Starbucks:  55 <br> State:  Arkansas <br> Population:  2915918","Number of Starbucks:  488 <br> State:  Arizona <br> Population:  6392017","Number of Starbucks:  2821 <br> State:  California <br> Population:  37253956","Number of Starbucks:  481 <br> State:  Colorado <br> Population:  5029196","Number of Starbucks:  123 <br> State:  Connecticut <br> Population:  3574097","Number of Starbucks:  91 <br> State:  District of Columbia <br> Population:  601723","Number of Starbucks:  25 <br> State:  Delaware <br> Population:  897934","Number of Starbucks:  694 <br> State:  Florida <br> Population:  18801310","Number of Starbucks:  326 <br> State:  Georgia <br> Population:  9687653","Number of Starbucks:  99 <br> State:  Hawaii <br> Population:  1360301","Number of Starbucks:  89 <br> State:  Iowa <br> Population:  3046355","Number of Starbucks:  67 <br> State:  Idaho <br> Population:  1567582","Number of Starbucks:  575 <br> State:  Illinois <br> Population:  12830632","Number of Starbucks:  221 <br> State:  Indiana <br> Population:  6483802","Number of Starbucks:  94 <br> State:  Kansas <br> Population:  2853118","Number of Starbucks:  116 <br> State:  Kentucky <br> Population:  4339367","Number of Starbucks:  84 <br> State:  Louisiana <br> Population:  4533372","Number of Starbucks:  273 <br> State:  Massachusetts <br> Population:  6547629","Number of Starbucks:  257 <br> State:  Maryland <br> Population:  5773552","Number of Starbucks:  30 <br> State:  Maine <br> Population:  1328361","Number of Starbucks:  283 <br> State:  Michigan <br> Population:  9883640","Number of Starbucks:  184 <br> State:  Minnesota <br> Population:  5303925","Number of Starbucks:  188 <br> State:  Missouri <br> Population:  5988927","Number of Starbucks:  32 <br> State:  Mississippi <br> Population:  2967297","Number of Starbucks:  36 <br> State:  Montana <br> Population:  989415","Number of Starbucks:  338 <br> State:  North Carolina <br> Population:  9535483","Number of Starbucks:  13 <br> State:  North Dakota <br> Population:  672591","Number of Starbucks:  58 <br> State:  Nebraska <br> Population:  1826341","Number of Starbucks:  29 <br> State:  New Hampshire <br> Population:  1316470","Number of Starbucks:  261 <br> State:  New Jersey <br> Population:  8791894","Number of Starbucks:  76 <br> State:  New Mexico <br> Population:  2059179","Number of Starbucks:  253 <br> State:  Nevada <br> Population:  2700551","Number of Starbucks:  645 <br> State:  New York <br> Population:  19378102","Number of Starbucks:  378 <br> State:  Ohio <br> Population:  11536504","Number of Starbucks:  79 <br> State:  Oklahoma <br> Population:  3751351","Number of Starbucks:  359 <br> State:  Oregon <br> Population:  3831074","Number of Starbucks:  357 <br> State:  Pennsylvania <br> Population:  12702379","Number of Starbucks:  27 <br> State:  Rhode Island <br> Population:  1052567","Number of Starbucks:  131 <br> State:  South Carolina <br> Population:  4625364","Number of Starbucks:  25 <br> State:  South Dakota <br> Population:  814180","Number of Starbucks:  180 <br> State:  Tennessee <br> Population:  6346105","Number of Starbucks:  1042 <br> State:  Texas <br> Population:  25145561","Number of Starbucks:  101 <br> State:  Utah <br> Population:  2763885","Number of Starbucks:  432 <br> State:  Virginia <br> Population:  8001024","Number of Starbucks:  8 <br> State:  Vermont <br> Population:  625741","Number of Starbucks:  757 <br> State:  Washington <br> Population:  6724540","Number of Starbucks:  145 <br> State:  Wisconsin <br> Population:  5686986","Number of Starbucks:  25 <br> State:  West Virginia <br> Population:  1852994","Number of Starbucks:  23 <br> State:  Wyoming <br> Population:  563626"],"locations":["AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY"],"type":"choropleth","marker":{"line":{"colorbar":{"title":"","ticklen":2},"cmin":8,"cmax":2821,"colorscale":[["0","rgba(252,251,253,1)"],["0.0416666666666667","rgba(248,246,250,1)"],["0.0833333333333333","rgba(243,242,248,1)"],["0.125","rgba(239,237,245,1)"],["0.166666666666667","rgba(232,231,242,1)"],["0.208333333333333","rgba(225,224,238,1)"],["0.25","rgba(218,218,235,1)"],["0.291666666666667","rgba(208,208,230,1)"],["0.333333333333333","rgba(198,199,225,1)"],["0.375","rgba(188,189,220,1)"],["0.416666666666667","rgba(178,177,213,1)"],["0.458333333333333","rgba(168,166,207,1)"],["0.5","rgba(158,154,200,1)"],["0.541666666666667","rgba(148,144,195,1)"],["0.583333333333333","rgba(138,135,191,1)"],["0.625","rgba(128,125,186,1)"],["0.666666666666667","rgba(121,110,178,1)"],["0.708333333333333","rgba(114,96,171,1)"],["0.75","rgba(106,81,163,1)"],["0.791666666666667","rgba(99,67,156,1)"],["0.833333333333333","rgba(92,54,150,1)"],["0.875","rgba(84,39,143,1)"],["0.916666666666667","rgba(77,28,137,1)"],["0.958333333333333","rgba(70,16,131,1)"],["1","rgba(63,0,125,1)"]],"showscale":false,"color":[49,85,55,488,2821,481,123,91,25,694,326,99,89,67,575,221,94,116,84,273,257,30,283,184,188,32,36,338,13,58,29,261,76,253,645,378,79,359,357,27,131,25,180,1042,101,432,8,757,145,25,23]}},"geo":"geo","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

``` r
map2 <- plot_geo(sb_locs_state, locationmode = "USA-states") |>
  add_trace(z = ~population, text = ~hover, locations = ~state, color = ~n_stores, colors = "Purples") |>
  layout(
    title = "starbucks store by state", geo = set_map_details)
map2
```

<div class="plotly html-widget html-fill-item" id="htmlwidget-d151026eae18205a3f83" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-d151026eae18205a3f83">{"x":{"visdat":{"a9747663c755":["function () ","plotlyVisDat"]},"cur_data":"a9747663c755","attrs":{"a9747663c755":{"locationmode":"USA-states","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"z":{},"text":{},"locations":{},"color":{},"colors":"Purples","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"mapType":"geo","title":"starbucks store by state","geo":{"domain":{"x":[0,1],"y":[0,1]},"scope":"usa","projection":{"type":"albers usa"},"showlakes":true,"lakecolor":"rgba(70,130,180,1)"},"scene":{"zaxis":{"title":"population"}},"hovermode":"closest","showlegend":false,"legend":{"yanchor":"top","y":0.5}},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"colorbar":{"title":"n_stores","ticklen":2,"len":0.5,"lenmode":"fraction","y":1,"yanchor":"top"},"colorscale":[["0","rgba(252,251,253,1)"],["0.0416666666666667","rgba(248,246,250,1)"],["0.0833333333333333","rgba(243,242,248,1)"],["0.125","rgba(239,237,245,1)"],["0.166666666666667","rgba(232,231,242,1)"],["0.208333333333333","rgba(225,224,238,1)"],["0.25","rgba(218,218,235,1)"],["0.291666666666667","rgba(208,208,230,1)"],["0.333333333333333","rgba(198,199,225,1)"],["0.375","rgba(188,189,220,1)"],["0.416666666666667","rgba(178,177,213,1)"],["0.458333333333333","rgba(168,166,207,1)"],["0.5","rgba(158,154,200,1)"],["0.541666666666667","rgba(148,144,195,1)"],["0.583333333333333","rgba(138,135,191,1)"],["0.625","rgba(128,125,186,1)"],["0.666666666666667","rgba(121,110,178,1)"],["0.708333333333333","rgba(114,96,171,1)"],["0.75","rgba(106,81,163,1)"],["0.791666666666667","rgba(99,67,156,1)"],["0.833333333333333","rgba(92,54,150,1)"],["0.875","rgba(84,39,143,1)"],["0.916666666666667","rgba(77,28,137,1)"],["0.958333333333333","rgba(70,16,131,1)"],["1","rgba(63,0,125,1)"]],"showscale":true,"locationmode":"USA-states","z":[710231,4779736,2915918,6392017,37253956,5029196,3574097,601723,897934,18801310,9687653,1360301,3046355,1567582,12830632,6483802,2853118,4339367,4533372,6547629,5773552,1328361,9883640,5303925,5988927,2967297,989415,9535483,672591,1826341,1316470,8791894,2059179,2700551,19378102,11536504,3751351,3831074,12702379,1052567,4625364,814180,6346105,25145561,2763885,8001024,625741,6724540,5686986,1852994,563626,3194000,106977,165768,56882],"text":["Number of Starbucks:  49 <br> State:  Alaska <br> Population:  710231","Number of Starbucks:  85 <br> State:  Alabama <br> Population:  4779736","Number of Starbucks:  55 <br> State:  Arkansas <br> Population:  2915918","Number of Starbucks:  488 <br> State:  Arizona <br> Population:  6392017","Number of Starbucks:  2821 <br> State:  California <br> Population:  37253956","Number of Starbucks:  481 <br> State:  Colorado <br> Population:  5029196","Number of Starbucks:  123 <br> State:  Connecticut <br> Population:  3574097","Number of Starbucks:  91 <br> State:  District of Columbia <br> Population:  601723","Number of Starbucks:  25 <br> State:  Delaware <br> Population:  897934","Number of Starbucks:  694 <br> State:  Florida <br> Population:  18801310","Number of Starbucks:  326 <br> State:  Georgia <br> Population:  9687653","Number of Starbucks:  99 <br> State:  Hawaii <br> Population:  1360301","Number of Starbucks:  89 <br> State:  Iowa <br> Population:  3046355","Number of Starbucks:  67 <br> State:  Idaho <br> Population:  1567582","Number of Starbucks:  575 <br> State:  Illinois <br> Population:  12830632","Number of Starbucks:  221 <br> State:  Indiana <br> Population:  6483802","Number of Starbucks:  94 <br> State:  Kansas <br> Population:  2853118","Number of Starbucks:  116 <br> State:  Kentucky <br> Population:  4339367","Number of Starbucks:  84 <br> State:  Louisiana <br> Population:  4533372","Number of Starbucks:  273 <br> State:  Massachusetts <br> Population:  6547629","Number of Starbucks:  257 <br> State:  Maryland <br> Population:  5773552","Number of Starbucks:  30 <br> State:  Maine <br> Population:  1328361","Number of Starbucks:  283 <br> State:  Michigan <br> Population:  9883640","Number of Starbucks:  184 <br> State:  Minnesota <br> Population:  5303925","Number of Starbucks:  188 <br> State:  Missouri <br> Population:  5988927","Number of Starbucks:  32 <br> State:  Mississippi <br> Population:  2967297","Number of Starbucks:  36 <br> State:  Montana <br> Population:  989415","Number of Starbucks:  338 <br> State:  North Carolina <br> Population:  9535483","Number of Starbucks:  13 <br> State:  North Dakota <br> Population:  672591","Number of Starbucks:  58 <br> State:  Nebraska <br> Population:  1826341","Number of Starbucks:  29 <br> State:  New Hampshire <br> Population:  1316470","Number of Starbucks:  261 <br> State:  New Jersey <br> Population:  8791894","Number of Starbucks:  76 <br> State:  New Mexico <br> Population:  2059179","Number of Starbucks:  253 <br> State:  Nevada <br> Population:  2700551","Number of Starbucks:  645 <br> State:  New York <br> Population:  19378102","Number of Starbucks:  378 <br> State:  Ohio <br> Population:  11536504","Number of Starbucks:  79 <br> State:  Oklahoma <br> Population:  3751351","Number of Starbucks:  359 <br> State:  Oregon <br> Population:  3831074","Number of Starbucks:  357 <br> State:  Pennsylvania <br> Population:  12702379","Number of Starbucks:  27 <br> State:  Rhode Island <br> Population:  1052567","Number of Starbucks:  131 <br> State:  South Carolina <br> Population:  4625364","Number of Starbucks:  25 <br> State:  South Dakota <br> Population:  814180","Number of Starbucks:  180 <br> State:  Tennessee <br> Population:  6346105","Number of Starbucks:  1042 <br> State:  Texas <br> Population:  25145561","Number of Starbucks:  101 <br> State:  Utah <br> Population:  2763885","Number of Starbucks:  432 <br> State:  Virginia <br> Population:  8001024","Number of Starbucks:  8 <br> State:  Vermont <br> Population:  625741","Number of Starbucks:  757 <br> State:  Washington <br> Population:  6724540","Number of Starbucks:  145 <br> State:  Wisconsin <br> Population:  5686986","Number of Starbucks:  25 <br> State:  West Virginia <br> Population:  1852994","Number of Starbucks:  23 <br> State:  Wyoming <br> Population:  563626","Number of Starbucks:  NA <br> State:  Puerto Rico <br> Population:  3194000","Number of Starbucks:  NA <br> State:  Virgin Islands <br> Population:  106977","Number of Starbucks:  NA <br> State:  Guam <br> Population:  165768","Number of Starbucks:  NA <br> State:  Northern Mariana Islands <br> Population:  56882"],"locations":["AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY",null,null,null,null],"type":"choropleth","marker":{"line":{"colorbar":{"title":"","ticklen":2},"cmin":8,"cmax":2821,"colorscale":[["0","rgba(252,251,253,1)"],["0.0416666666666667","rgba(248,246,250,1)"],["0.0833333333333333","rgba(243,242,248,1)"],["0.125","rgba(239,237,245,1)"],["0.166666666666667","rgba(232,231,242,1)"],["0.208333333333333","rgba(225,224,238,1)"],["0.25","rgba(218,218,235,1)"],["0.291666666666667","rgba(208,208,230,1)"],["0.333333333333333","rgba(198,199,225,1)"],["0.375","rgba(188,189,220,1)"],["0.416666666666667","rgba(178,177,213,1)"],["0.458333333333333","rgba(168,166,207,1)"],["0.5","rgba(158,154,200,1)"],["0.541666666666667","rgba(148,144,195,1)"],["0.583333333333333","rgba(138,135,191,1)"],["0.625","rgba(128,125,186,1)"],["0.666666666666667","rgba(121,110,178,1)"],["0.708333333333333","rgba(114,96,171,1)"],["0.75","rgba(106,81,163,1)"],["0.791666666666667","rgba(99,67,156,1)"],["0.833333333333333","rgba(92,54,150,1)"],["0.875","rgba(84,39,143,1)"],["0.916666666666667","rgba(77,28,137,1)"],["0.958333333333333","rgba(70,16,131,1)"],["1","rgba(63,0,125,1)"]],"showscale":false,"color":[49,85,55,488,2821,481,123,91,25,694,326,99,89,67,575,221,94,116,84,273,257,30,283,184,188,32,36,338,13,58,29,261,76,253,645,378,79,359,357,27,131,25,180,1042,101,432,8,757,145,25,23,null,null,null,null]}},"geo":"geo","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

``` r
subplot <- subplot(map1, map2)
```

    ## Warning: Ignoring 4 observations

``` r
frameWidget(map1)
```

    ## Warning: Ignoring 4 observations

<div id="htmlwidget-fa3059c648ca59db2ffb" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-fa3059c648ca59db2ffb">{"x":{"url":"lab11-interactive-viz1_files/figure-gfm//widgets/widget_unnamed-chunk-12.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

``` r
frameWidget(map2)
```

<div id="htmlwidget-43369f13c07bf01afe1e" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-43369f13c07bf01afe1e">{"x":{"url":"lab11-interactive-viz1_files/figure-gfm//widgets/widget_unnamed-chunk-12.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

``` r
frameWidget(subplot)
```

<div id="htmlwidget-a6f892e16b264a53799c" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-a6f892e16b264a53799c">{"x":{"url":"lab11-interactive-viz1_files/figure-gfm//widgets/widget_unnamed-chunk-12.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

- 8)  Answer:

When put side by side, I do think that states with higher population
tend to have a higher number of Starbucks stores. For instance,
California has the highest population and also the highest number of
Starbucks stores, Texas has the second highest population and the second
largest number of stores. Overall, the maps look relatively similar
(color-wise). Therefore, there isn’t much of a difference.

- Bonus) Type in comment of submission: fav Starbucks drink

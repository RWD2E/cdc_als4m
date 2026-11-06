rm(list=ls()); gc()
setwd("C:/repos/cdc_als4m")

# install.packages("pacman")
pacman::p_load(
  DBI,
  jsonlite,
  odbc,
  tidyverse,
  magrittr,
  dbplyr,
  scales,
  stringr,
  viridis,
  tibble
)




# ============================================================
# 1. Parameters
# ============================================================

START_YEAR <- 2011
END_YEAR   <- 2020
TOP_N      <- 5

# ============================================================
# 2. Snowflake connection
# ============================================================

# make db connection
sf_conn <- DBI::dbConnect(
  drv = odbc::odbc(),
  dsn = "snowflake-id",
  uid = "xsm7f@umsystem.edu",
  pwd = ""
)

# ============================================================
# 3. Retrieve state-level RSEI scores and chemical names
# ============================================================

sql <- sprintf("
WITH CHEMICAL_LOOKUP AS (
    SELECT
        CHEMICALNUMBER,
        MAX(CHEMICAL) AS CHEMICAL
    FROM EPA_DB.EPA_RSEI.Z_REF_CHEMICAL_DATA_RSEI
    GROUP BY CHEMICALNUMBER
),

STATE_SUMMARY AS (
    SELECT
        GEOID,
        CHEMICAL_NUMBER,
        SUM(TOTAL) AS TOTAL_SCORE
    FROM GROUSE_ANALYTICS_DB.SX_ALS_GPC.CMS_RSEI_PP_SUMM_STATE
    WHERE GEO_LEVEL = 'STATE'
      AND YEAR BETWEEN %d AND %d
      AND MEASURE = '%s'
      AND CHEMICAL_NUMBER IS NOT NULL
      AND TOTAL IS NOT NULL
    GROUP BY
        GEOID,
        CHEMICAL_NUMBER
)

SELECT
    s.GEOID,
    s.CHEMICAL_NUMBER,
    COALESCE(
        c.CHEMICAL,
        'Chemical ' || TO_VARCHAR(s.CHEMICAL_NUMBER)
    ) AS CHEMICAL,
    LEFT(
        COALESCE(
            c.CHEMICAL,
            'Chemical ' || TO_VARCHAR(s.CHEMICAL_NUMBER)
        ),
        10
    ) AS CHEMICAL_SHORT,
    s.TOTAL_SCORE
FROM STATE_SUMMARY s
LEFT JOIN CHEMICAL_LOOKUP c
    ON s.CHEMICAL_NUMBER = c.CHEMICALNUMBER
ORDER BY
    s.GEOID,
    s.TOTAL_SCORE DESC
", START_YEAR, END_YEAR)

rsei <- dbGetQuery(sf_conn, sql)

# ============================================================
# 4. State GEOID -> state abbreviation
# ============================================================

state_lookup <- tibble(
  STATE = c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE",
    "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA",
    "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN",
    "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
    "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
    "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
    "WV", "WI", "WY"
  ),
  GEOID = c(
    "01", "02", "04", "05", "06", "08", "09", "10",
    "11", "12", "13", "15", "16", "17", "18", "19",
    "20", "21", "22", "23", "24", "25", "26", "27",
    "28", "29", "30", "31", "32", "33", "34", "35",
    "36", "37", "38", "39", "40", "41", "42", "44",
    "45", "46", "47", "48", "49", "50", "51", "53",
    "54", "55", "56"
  )
)

# ============================================================
# 5. State tile-map coordinates
#
# Approximate US geographic arrangement.
# All state squares have identical dimensions.
# ============================================================

state_grid <- tribble(
  ~STATE, ~COL, ~ROW,
  # 
  # "AK",  1, 8,
  # "HI",  2, 8,
  
  "WA",  2, 1,
  "OR",  2, 2,
  "CA",  2, 3,
  "NV",  3, 3,
  "ID",  3, 2,
  "MT",  3, 1,
  
  "UT",  4, 3,
  "AZ",  4, 4,
  "WY",  4, 2,
  
  "CO",  5, 3,
  "NM",  5, 4,
  "ND",  5, 1,
  "SD",  5, 2,
  
  "NE",  6, 2,
  "KS",  6, 3,
  "OK",  6, 4,
  "TX",  6, 5,
  "MN",  6, 1,
  
  "IA",  7, 2,
  "MO",  7, 3,
  "AR",  7, 4,
  "LA",  7, 5,
  "WI",  7, 1,
  
  "IL",  8, 2,
  "MS",  8, 4,
  "MI",  8, 1,
  "TN",  8, 3,
  
  "IN",  9, 2,
  "KY",  9, 3,
  "AL",  9, 4,
  
  "OH", 10, 2,
  "WV", 10, 3,
  "GA", 10, 4,
  "FL", 10, 5,
  
  "NY", 11, 1,
  "PA", 11, 2,
  "VA", 11, 3,
  "NC", 11, 4,
  "SC", 11, 5,
  
  "VT", 12, 1,
  "NJ", 12, 2,
  "MD", 12, 3,
  "DE", 12, 4,
  
  "ME", 13, 1,
  "NH", 13, 2,
  "MA", 13, 3,
  "CT", 13, 4,
  "RI", 13, 5,
  
  "DC", 12, 5
)

# ============================================================
# 6. Select top five chemicals per state
# ============================================================

top5 <- rsei %>%
  mutate(
    GEOID = str_pad(
      as.character(GEOID),
      width = 2,
      side = "left",
      pad = "0"
    ),
    TOTAL_SCORE = as.numeric(TOTAL_SCORE),
    CHEMICAL = as.character(CHEMICAL),
    CHEMICAL_SHORT = str_sub(CHEMICAL, 1, 10)
  ) %>%
  left_join(state_lookup, by = "GEOID") %>%
  filter(
    !is.na(STATE),
    is.finite(TOTAL_SCORE),
    TOTAL_SCORE > 0
  ) %>%
  group_by(STATE) %>%
  arrange(desc(TOTAL_SCORE), CHEMICAL_NUMBER, .by_group = TRUE) %>%
  slice_head(n = TOP_N) %>%
  mutate(
    RANK = row_number(),
    STATE_TOP5_TOTAL = sum(TOTAL_SCORE),
    SHARE = TOTAL_SCORE / STATE_TOP5_TOTAL,
    TOTAL_SCORE_LOG10 = log10(TOTAL_SCORE)
  ) %>%
  ungroup()

# ============================================================
# 7. Construct proportional chemical rectangles
#
# Each state occupies a 1 x 1 square.
# Each chemical occupies full width and height = SHARE.
#
# Therefore:
# chemical area / state area = chemical score / top5 total
# ============================================================

tile_data <- top5 %>%
  left_join(state_grid, by = "STATE") %>%
  filter(!is.na(COL), !is.na(ROW)) %>%
  group_by(STATE) %>%
  arrange(RANK, .by_group = TRUE) %>%
  mutate(
    CUM_SHARE = cumsum(SHARE),
    
    XMIN = COL - 0.5,
    XMAX = COL + 0.5,
    
    # Higher scores appear at the top of each square
    YMAX = -ROW + 0.5 - lag(CUM_SHARE, default = 0),
    YMIN = -ROW + 0.5 - CUM_SHARE,
    
    X_LABEL = COL,
    Y_LABEL = (YMIN + YMAX) / 2,
    
    LABEL = case_when(
      SHARE >= 0.15 ~ paste0(
        CHEMICAL_SHORT,
        "\n",
        sprintf("%.2f", TOTAL_SCORE_LOG10)
      ),
      
      SHARE >= 0.08 ~ CHEMICAL_SHORT,
      
      TRUE ~ ""
    )
  ) %>%
  ungroup()

# ============================================================
# 8. State borders and labels
# ============================================================

state_boxes <- state_grid %>%
  mutate(
    XMIN = COL - 0.5,
    XMAX = COL + 0.5,
    YMIN = -ROW - 0.5,
    YMAX = -ROW + 0.5
  )

state_labels <- state_grid %>%
  mutate(
    X = COL,
    Y = -ROW + 0.62
  )

# ============================================================
# 9. Plot
# ============================================================

p <- ggplot() +
  
  # All state squares, including states with no data
  geom_rect(
    data = state_boxes,
    aes(
      xmin = XMIN,
      xmax = XMAX,
      ymin = YMIN,
      ymax = YMAX
    ),
    fill = "grey95",
    colour = "grey30",
    linewidth = 0.5
  ) +
  
  # Proportional chemical rectangles
  geom_rect(
    data = tile_data,
    aes(
      xmin = XMIN,
      xmax = XMAX,
      ymin = YMIN,
      ymax = YMAX,
      fill = factor(RANK)
    ),
    colour = "white",
    linewidth = 0.35
  ) +
  
  # State labels
  geom_text(
    data = state_grid,
    aes(
      x = COL,
      y = -ROW,
      label = STATE
    ),
    size = 10,
    colour = "grey80",
    alpha = 0.35,
    fontface = "bold",
    family = "sans"
  ) +
  
  # Chemical labels
  geom_text(
    data = tile_data %>%
      filter(SHARE >= 0.08),
    aes(
      x = X_LABEL,
      y = Y_LABEL,
      label = LABEL
    ),
    size = 2.4,
    colour = "white",
    fontface = "bold",
    lineheight = 0.95,
    check_overlap = TRUE
  ) +
  
  scale_fill_manual(
    values = c(
      "1" = "#173F5F",  # Deep navy
      "2" = "#287A8C",  # Teal blue
      "3" = "#39734D",  # Forest green
      "4" = "#975C36",  # Burnt umber
      "5" = "#624787"   # Muted purple
    ),
    name = "Chemical rank",
    labels = paste("Rank", 1:5),
    drop = FALSE
  ) +
  
  coord_fixed(
    ratio = 1,
    clip = "off"
  ) +
  
  scale_x_continuous(
    limits = c(0, 14),
    expand = expansion(mult = 0)
  ) +
  
  scale_y_continuous(
    limits = c(-9, 0.5),
    expand = expansion(mult = 0)
  ) +
  
  labs(
    title = "Top 5 Chemicals by Total RSEI Score",
    subtitle = paste0(
      "State-level RSEI exposure | ",
      START_YEAR, "–", END_YEAR
    ),
    caption = paste(
      "Each square represents one state.",
      "Chemical area is proportional to its share",
      "of the state's top-five total RSEI score."
    )
  ) +
  
  theme_void(base_size = 12) +
  
  theme(
    plot.background = element_rect(
      fill = "#FAFAFA",
      colour = NA
    ),
    
    panel.background = element_rect(
      fill = "#FAFAFA",
      colour = NA
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      size = 11,
      face = "bold"
    ),
    
    legend.text = element_text(size = 10),
    
    plot.title = element_text(
      size = 18,
      face = "bold",
      colour = "#173F5F"
    ),
    
    plot.subtitle = element_text(
      size = 11,
      colour = "grey35"
    ),
    
    plot.caption = element_text(
      size = 9,
      colour = "grey45"
    ),
    
    plot.margin = margin(25, 25, 25, 25)
  )

# ============================================================
# 10. Export
# ============================================================

ggsave(
  "rsei_top5_state_tilemap.pdf",
  p,
  width = 18,
  height = 12
)

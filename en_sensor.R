# Last inn nødvendige pakker
library(tidyverse)

# 1. Sett parametere for simuleringen
set.seed(42)        # For at simuleringen skal gi samme resultat hver gang
n_studenter <- 200000 # Stort antall for å få stabile prosentandeler
reliabilitet <- 0.5 # Gitt at vi bruker standard normalfordeling (SD=1) for sannhet, 
                    # betyr reliabilitet på 0.5 at støyen også må ha SD=1.

# 2. Simuler data
dat <- tibble(
  # Sann ferdighet (fasit)
  sannhet = rnorm(n_studenter, mean = 0, sd = 1),
  
  # Støy fra sensor 1 og 2
  stoy1 = rnorm(n_studenter, mean = 0, sd = 1),
  stoy2 = rnorm(n_studenter, mean = 0, sd = 1)
) |> 
  mutate(
    # Kontinuerlig signal + støy
    sensor1_kont = sannhet + stoy1,
    sensor2_kont = sannhet + stoy2,
    
    # Kompromisset mellom to sensorer (gjennomsnitt)
    to_sensorer_kont = (sensor1_kont + sensor2_kont) / 2
  )

# 3. Funksjon for å "binne" kontinuerlige verdier inn i karakterer (1=F, 6=A)
# Vi bruker scale() for å standardisere fordelingen før vi kutter. 
# Dette simulerer at sensorer "kalibrerer" seg etter nivået på bunken.
kutt_til_karakter <- function(x) {
  z <- scale(x) |> as.numeric()
  # Symmetriske kuttpunkter med nøyaktig 0.8 standardavviks bredde
  kutt <- c(-Inf, -1.6, -0.8, 0, 0.8, 1.6, Inf) 
  as.numeric(cut(z, breaks = kutt, labels = 1:6))
}

# 4. Gjør om til A-F karakterer og beregn diskrepans
dat <- dat |> 
  mutate(
    karakter_fasit = kutt_til_karakter(sannhet),
    karakter_1sensor = kutt_til_karakter(sensor1_kont),
    karakter_2sensorer = kutt_til_karakter(to_sensorer_kont)
  ) %>%
  mutate(
    # Beregn absolutt feil (antall karaktertrinn feil)
    feil_1sensor = abs(karakter_fasit - karakter_1sensor),
    feil_2sensorer = abs(karakter_fasit - karakter_2sensorer)
  )

# 5. Kategoriser feilene for plotting
kategoriser_feil <- function(feil) {
  case_when(
    feil == 0 ~ "0 (Korrekt)",
    feil == 1 ~ "1 trinn feil (Mindre feil)",
    feil == 2 ~ "2 trinn feil (Stor feil)",
    feil >= 3 ~ "3+ trinn feil (Alvorlig feil)"
  )
}

# Omstrukturer data for ggplot
plot_data <- dat %>%
  select(feil_1sensor, feil_2sensorer) %>%
  pivot_longer(cols = everything(), names_to = "scenario", values_to = "antall_feil") %>%
  mutate(
    scenario = if_else(scenario == "feil_1sensor", "En sensor", "To sensorer"),
    feil_kat = kategoriser_feil(antall_feil)
  ) |> 
  count(scenario, feil_kat) |> 
  group_by(scenario) %>%
  mutate(
    prosent = (n / sum(n)) * 100 # Regn ut prosentandel
  ) |> 
  ungroup() |> 
  # Vi fjerner de som fikk korrekt karakter, for å fokusere på ulempene/feilene i grafen
  filter(feil_kat != "0 (Korrekt)")

# Sjekk fasit fordeling
dat |> 
  count(karakter_fasit) |> 
  ggplot(aes(karakter_fasit,n)) + 
  geom_col()

# 6. Lag plottet
ggplot(plot_data, aes(x = scenario, y = prosent, fill = scenario)) +
  geom_col(width = 0.6,alpha = .8) +
  # Legg til tekst på toppen av søylene for nøyaktighet
  geom_text(aes(label = sprintf("%.1f %%", prosent)), 
            vjust = -0.8, size = 7) +
  # Del opp i tre paneler basert på alvorlighetsgrad, fristill y-aksen for å se relativ økning
  facet_wrap(~ feil_kat, scales = "free_y") +
  scale_fill_manual(values = c("En sensor" = "red4", "To sensorer" = "navyblue")) +
  # Utvid y-aksen litt på toppen for at teksten ikke skal kuttes
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
  labs(
    x = NULL, # Fjerner x-akse-tittel da "En/To sensorer" er selvforklarende
    y = "Andel studenter (%)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none", # Trenger ikke legend når x-aksen sier det samme
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 13), # Tydelige overskrifter på panelene
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(2, "lines") # Litt mer luft mellom panelene
  )


library(tidyverse)
library(readxl)

Global_Protest <- read_excel("Data/Global Protest Tracker.xlsx")
names(Global_Protest)


Outcome_Categories_Global_Protest <- Global_Protest %>%
  mutate(protest_outcome = case_when(
    str_detect(Outcome, regex("resigned|ousted|fled|overthrown|coup|impeached|removed from office|stepped down|dismissed|fired|forced out|no confidence|cabinet resigned|government resigned|prime minister resigned|president resigned", ignore_case = TRUE)) ~ "Leadership Change",
    str_detect(Outcome, regex("withdrew|canceled|cancelled|reversed|scrapped|retracted|repealed|dropped|abandoned|overturned|annulled|lifted the blockade|peace accord|signed a deal|agreement", ignore_case = TRUE)) ~ "Full Concession",
    str_detect(Outcome, regex("promised|investigation|delayed|postponed|reduced|increased funding|pay raise|bonus|subsidies|compensation|amended|reformed|committee|task force|commission|partially|announced measures|agreed to meet|agreed to consider", ignore_case = TRUE)) ~ "Partial Concession",
    str_detect(Outcome, regex("killed|arrested|tear gas|water cannon|state of emergency|banned|internet shutdown|crackdown|detained|fired on protesters|opened fire|suppressed|deployed troops|deployed military", ignore_case = TRUE)) ~ "Violent Suppression",
    str_detect(Outcome, regex("No policy/leadership change|No policy or leadership change", ignore_case = TRUE)) ~ "No Change",
    
    TRUE ~ "Uncategorised"
  ))


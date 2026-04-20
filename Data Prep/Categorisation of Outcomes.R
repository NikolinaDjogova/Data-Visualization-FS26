library(tidyverse)
library(readxl)

Global_Protest <- read_csv("Data Prep/Global Protest Tracker - View Data.csv")
names(Global_Protest)


Outcome_Categories_Global_Protest <- Global_Protest %>%
  mutate(protest_outcome = case_when(
  str_detect(Outcomes, regex("resigned|ousted|fled|overthrown|coup|impeached|removed from office|stepped down|dismissed|fired|forced out|no confidence|cabinet resigned|government resigned|prime minister resigned|president resigned|expel|dissolv|new government|coalition government|tasked.*form|constitution was suspended|general.*in charge|military initiated|sworn into office|veto|ceasefire|reappointed as prime minister|announced.*would not seek|lost its majority|allowing his party to attempt|several ministers announced their resignation|election was held.*won by|constitutional council ruled.*unlawful|acquitted|sentenced|invalidating.*candidacy|signed the bill into law|enacted.*despite|flag change was enacte", ignore_case = TRUE)) ~ "Leadership Change",
  str_detect(Outcomes, regex("withdrew|canceled|cancelled|reversed|scrapped|retracted|repealed|dropped|abandoned|overturned|annulled|lifted the blockade|peace accord|signed a deal|agreement|rescind|conceded|cancel|suspend the reform|rewrite the bill|stop mining|ruled unconstitutional|injunction|struck down|invalidated|vetoed the bill|rejected the bill|failed to pass|end its operations|scrap the property bill|voted to reject", ignore_case = TRUE)) ~ "Full Concession",
  str_detect(Outcomes, regex("promised|investigation|delayed|postponed|reduced|increased funding|pay raise|bonus|subsidies|compensation|amended|reformed|committee|task force|commission|partially|announced measures|agreed to meet|agreed to consider|approved legislation|relief package|tax break|tax relief|salary|wage|job creation|hiring|extended the deadline|roundtable|plan|apologi|acknowledg|pledged|proposed|passed a law|passed a bill|approved the bill|approved the amendment|approved measures|legal reforms|hard deadline|suspended the tax|special budget|limited concession|sent the law back|negotiated settlement|court ruled|constitutional court|supreme court|electoral tribunal|parliament voted|lawmakers approved|congress voted|senate approved|mass release|protocols|death penalty|referendum | development initiatives|foreign secretaries met|work with representatives|religious harmony|agreed to increase funding|security delegation|ordered.*investigate|called for reforms|agreed to scale back|declared.*mourning|released.*bail|agreed to continue negotiations|announced.*plan|crack down|restore.*independence|pay deal|accepted.*deal|reforms to the pay|increase.*percent|selected as.*chief|intervened|compelled|census|inquiry into|approved.*senate|deregulation|privatiz|frozen by judges|new chief justice|permission to cross |released the youths|proposal passed|stripped.*hosting|bar.*team|passed its first reading|rejected.*second reading  propose amendments|would propose", ignore_case = TRUE)) ~ "Partial Concession",
  str_detect(Outcomes, regex("killed|arrested|tear gas|water cannon|state of emergency|banned|internet shutdown|crackdown|detained|fired on protesters|opened fire|suppressed|deployed troops|deployed military|riot police|violently repressed|disperse|custody|charged|expelled|blocked|ethnic violence|rocket attacks|military operation|recalled its ambassador | arrests were made", ignore_case = TRUE)) ~ "Violent Suppression",
  str_detect(Outcomes, regex("No policy/leadership change|No policy or leadership change|No policy/leadership\\.|thanked the residents|sparked an international|Erdogan made explicitly clear|Houthi leaders implemented|opposition continued to protest|allow his party to attempt|Modi responded|poverty rate rose|failing to protect|could not do so|has not issued an official statement|charges.*absolutely wrong|deploring|opposition continued|plurality", ignore_case = TRUE)) ~ "No Change",
  is.na(Outcomes) ~ "No Data",
  
  TRUE ~ "Uncategorised"
  ))

#control mechanism do avoid uncategorised cases and to refine allocation
Uncatagorised <- Outcome_Categories_Global_Protest %>%
  filter(protest_outcome == "Uncategorised") %>%
  select(Outcomes)
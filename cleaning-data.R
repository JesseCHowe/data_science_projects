# Installing Necessary Packages

# For Data Frame Manipulation
library(dplyr)
library(stringr)
library(tidyr)

# Import Transcript
KH1_raw <- readLines("game-scripts/Kingdom Hearts 1.5 Remix.txt")

# There are five cases in this document where colons are used in speech and are not used to denote a character, I filter them out here.
KH1_raw <- gsub("down again, when suddenly:","down again, when suddenly,",KH1_raw)
KH1_raw <- gsub("And for today’s weather:","And for today’s weather,",KH1_raw)
KH1_raw <- gsub("the seas:","the seas,",KH1_raw)
KH1_raw <- gsub("let’s continue. The ingredients for a heart:","let’s continue. The ingredients for a heart,",KH1_raw)
KH1_raw <- gsub("Realize the destiny:","Realize the destiny,",KH1_raw)

# Remove Header and Footer from Script
KH1_start <- str_which(KH1_raw, fixed("????: I've been having these weird thoughts lately... Like, is any of this"))
KH1_end <- str_which(KH1_raw, fixed("Everything is coming back to me, the true..."))

# Subset script
KH1_sub <- KH1_raw[(KH1_start): (KH1_end)]


KH1 <- data_frame(KH1_sub) %>%
  # Filtering out empty lines
  filter(KH1_sub != "") %>%
  # Seperating characters from their lines, fill left will allow for lines broken onto new lines to be merged. (This also merges character actions in parentheses to be merged into dialogue, we'll deal with this later on.)
  separate(KH1_sub, c("speaker", "dialogue"), sep = ":", fill = "left") %>%
  group_by(line = cumsum(!is.na(speaker))) %>%
  summarize(speaker = speaker[1], dialogue = str_c(dialogue, collapse = " "))

# Now it's time to remove the actions in parentheses
KH1$dialogue <- gsub("\\s*\\([^\\)]+\\)","",KH1$dialogue)
# Removing parentheses in speaker's name such as Kairi (Toward Riku):
KH1$speaker <- gsub("\\s*\\([^\\)]+\\)","",KH1$speaker)

#Let's see what have now
unique(KH1$speaker)

# It appears that the script denotes different mysterious characters with a different number of question marks, let's correct those

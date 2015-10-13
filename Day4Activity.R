phrases = c("Arsenal vs. ManU", "Arsenal Arsenal Arsenal", "Statistical Computing With R", "STAT 331", "STAT 805", "stat", "Stat", "STATISTICS", "gopoly.com", "805.756.1111", "805.543.1234", "#805", 
            "800.805.0000", "800-123-0805", "I like teaching STAT 331", "I wish there was STAT 805");

gregexpr("Arsenal", phrases)

#identify phrases that have stat in them
phrases[grep("STAT", phrases)]

#only at the beginning of the string
phrases[grep("^STAT", phrases)]

#only at the end
phrases[grep("STAT$", phrases)]

#at either end of the string
grep("\\b STAT", phrases)

#stat followed by any number
grep("STAT [0-9]", phrases)

#only 300 level
grep("^STAT 3", phrases)

#3 numbers, then a period
phrases[grep("[0-9][0-9][0-9]\\.|-", phrases)]

#4.2
#number of chars in first phrase
nchar(phrases[1])

#number of chars in each phrase
nchar(phrases)

#get first seven characters from first string and all the strings
substring(phrases[1], 1,7)
substring(phrases, 1, 7)

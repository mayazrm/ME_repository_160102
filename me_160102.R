
# long live the narwhals (disguised as unicorns)
# We are going to have to make a difficult decision regarding Halloween. 

# MR Loading info:
#setwd("~/Google Drive/Columbia/RESEARCH/CURRENT/Mode_Emotions/mturk.study.2/ME_repository_160102")
#medata <- read.csv("me.151111.csv", header=TRUE)

#KZ Loading info:
medata <- read.csv("/Users/zeekatherine/Mode&Emotions/me.151111.csv", header=TRUE)

# MC Loading info:
# medata <- read.csv("/Users/markalexanderconley/Desktop/me.151111.csv", header=TRUE)
# options(digits=3)
# one more line test
# and another, already pushed test script from project ME.  Now for the real 160102
  
# Mturk Study Fall 2015

# test test Mac2393 making a commit (changes) and then pushing it to Git, because Git bumped into me. 
# test attempt 2 line 8 mac
# test again line 9

####### EXCLUSIONS ######
# Original N = 117

# exclude those who failed attn1: at 111
medata <- medata[ which(medata$attn1 == "emotion") , ]

# Final N = 111

####### COMPUTING VARIABLES #########

#focus
medata$rfq_1r <- 6-medata$rfq_1
medata$rfq_9r <- 6-medata$rfq_9
medata$rfq_11r <- 6-medata$rfq_11
medata$rfq_2r <- 6-medata$rfq_2
medata$rfq_4r <- 6-medata$rfq_4
medata$rfq_6r <- 6-medata$rfq_6
medata$rfq_8r <- 6-medata$rfq_8

medata$prom.matrix <- cbind(medata$rfq_1r, medata$rfq_3, medata$medata$rfq_7, medata$rfq_9r, medata$rfq_10, medata$rfq_11r)
medata$prom.v <- as.vector(rowMeans(medata$prom.matrix, na.rm=T))
medata$prom.c <- scale(medata$prom.v, center=T, scale=F)

medata$prev.matrix <- cbind(medata$rfq_2r, medata$rfq_4r, medata$rfq_5, medata$rfq_6r, medata$rfq_8r)
medata$prev.v <- as.vector(rowMeans(medata$prev.matrix, na.rm=T))
medata$prev.c <- scale(medata$prev.v, center=T, scale=F)

# mode
medata$rmq_2r <- 7-medata$rmq_2
medata$rmq_10r <- 7-medata$rmq_10
medata$rmq_27r <- 7-medata$rmq_27
medata$rmq_13r <- 7-medata$rmq_13
medata$rmq_24r <- 7-medata$rmq_24

medata$loc.matrix <- cbind(medata$rmq_1,medata$rmq_3, medata$rmq_4, medata$rmq_5, medata$rmq_8, medata$rmq_13r, medata$rmq_16, medata$rmq_21, medata$rmq_24r, medata$rmq_25, medata$rmq_28, medata$rmq_29)
medata$loc.v <- as.vector(rowMeans(medata$loc.matrix, na.rm=T))
medata$loc.c <- scale (medata$loc.v, center=T, scale=F)

medata$ass.matrix <- cbind(medata$rmq_2r, medata$rmq_6, medata$rmq_7, medata$rmq_9, medata$rmq_10r, medata$rmq_11, medata$rmq_15, medata$rmq_19, medata$rmq_20, medata$rmq_22, medata$rmq_27r, medata$rmq_30)
medata$ass.v <- as.vector(rowMeans(medata$ass.matrix, na.rm =T))
medata$ass.c <- scale (medata$ass.v, center=T, scale=F)

medata$locXass <- medata$loc.c*medata$ass.c
alpha(as.data.frame(medata$loc.matrix))
alpha(as.data.frame(medata$ass.matrix))

medata$predom <- medata$loc.c - medata$ass.c
medata$predom.c <- scale(medata$predom, center=T, scale=F)
medata$predom <- as.vector(medata$predom)

medata$gender    <- as.factor(medata$Gender)
medata$age       <- medata$Age
medata$eth       <- as.factor(medata$Ethnic)
medata$edu       <- as.factor(medata$Education)

### RE-CODING EMOTIONS ####

medata$afraid       <- medata$Afraid_1
medata$scared       <- medata$Afraid_2
medata$frightened   <- medata$Afraid_3
medata$nervous      <- medata$Afraid_4
medata$jittery      <- medata$Afraid_5
medata$shaky        <- medata$Afraid_6
medata$afraidsum    <- medata$afraid + medata$scared + medata$frightened + medata$nervous + medata$jittery + medata$shaky
medata$afraidave    <- medata$afraidsum/6

# angry 6
medata$angry       <- medata$Angry_1
medata$hostile     <- medata$Angry_2
medata$irratible   <- medata$Angry_3
medata$scornful    <- medata$Angry_4
medata$disgusted   <- medata$Angry_5
medata$loathing    <- medata$Angry_6
medata$angrysum    <- medata$angry + medata$hostile + medata$irratible + medata$scornful + medata$disgusted + medata$loathing
medata$angryave    <- medata$angrysum/6

# guilty 6
medata$guilty       <- medata$Guilty_1
medata$ashamed      <- medata$Guilty_2
medata$blameworthy  <- medata$Guilty_3
medata$angryatself  <- medata$Guilty_4
medata$disgustws    <- medata$Guilty_5
medata$dissatws     <- medata$Guilty_6
medata$guiltysum    <- medata$guilty + medata$ashamed + medata$blameworthy + medata$angryatself + medata$disgustws + medata$dissatws
# above line doesn't work
medata$guiltyave    <- medata$guiltysum/6

# sad 5
medata$sad          <- medata$Sad_1
medata$blue         <- medata$Sad_2
medata$downhearted  <- medata$Sad_3
medata$alone        <- medata$Sad_4
medata$lonely       <- medata$Sad_5
medata$sadsum       <- medata$sad + medata$blue + medata$downhearted + medata$alone + medata$lonely
medata$sadave       <- medata$sadsum/5

# happy 8
medata$happy        <- medata$Happy_1
medata$joyful       <- medata$Happy_2
medata$delighted    <- medata$Happy_3
medata$cheerful     <- medata$Happy_4
medata$excited      <- medata$Happy_5
medata$enthusiastic <- medata$Happy_6
medata$lively       <- medata$Happy_7
medata$energetic    <- medata$Happy_8
medata$happysum     <- medata$happy + medata$joyful + medata$delighted + medata$cheerful + medata$excited + medata$enthusiastic + medata$lively + medata$energetic
medata$happyave     <- medata$happysum/8

# proud 5
medata$proud        <- medata$Proud_1
medata$strong       <- medata$Proud_2
medata$confident    <- medata$Proud_3
medata$bold         <- medata$Proud_4
medata$daring       <- medata$Proud_5
medata$proudsum     <- medata$proud + medata$strong + medata$confident + medata$bold + medata$daring 
medata$proudave     <- medata$proudsum/5

# alert 4
medata$alert         <- medata$Alert_1
medata$attentive     <- medata$Alert_2
medata$concentrating <- medata$Alert_3
medata$determined    <- medata$Alert_4
medata$alertsum      <- medata$alert + medata$attentive + medata$concentrating + medata$determined
medata$alertave      <- medata$alertsum/4

# shy 4
medata$shy         <- medata$Shy_1 
medata$bashful     <- medata$Shy_2
medata$sheepish    <- medata$Shy_3
medata$timid       <- medata$Shy_4
medata$shysum      <- medata$shy + medata$bashful + medata$sheepish + medata$timid
medata$shyave      <- medata$shysum/6

# sleepy 4
medata$sleepy     <- medata$Sleepy_1
medata$tired      <- medata$Sleepy_2
medata$sluggish   <- medata$Sleepy_3
medata$drowsy     <- medata$Sleepy_4
medata$sleepysum  <- medata$sleepy + medata$tired + medata$sluggish + medata$drowsy 
medata$sleepyave  <- medata$sleepysum/4

# calm 3
medata$calm       <- medata$Calm_1
medata$relaxed    <- medata$Calm_2
medata$atease     <- medata$Calm_3
medata$calmsum    <- medata$calm + medata$relaxed + medata$atease
medata$calmave    <- medata$calmsum/3 

# amazed 3
medata$amazed        <- medata$Amazed_1
medata$suprised      <- medata$Amazed_2
medata$astonished    <- medata$Amazed_3
medata$amazedsum     <- medata$amazed + medata$suprised + medata$astonished
medata$amazedave     <- medata$amazedsum/3

# dejected 5
medata$dejected      <- medata$Dejected_1
medata$hopeless      <- medata$Dejected_2
medata$despairing    <- medata$Dejected_3
medata$depressed     <- medata$Dejected_4
medata$upset         <- medata$Dejected_5
medata$dejectedsum   <- medata$dejected + medata$hopeless + medata$despairing + medata$depressed + medata$upset
medata$dejectedave   <- medata$dejectedsum/5

# hopeful 4
medata$hopeful        <- medata$Hopeful_1 
medata$elated         <- medata$Hopeful_2 
medata$cheerful       <- medata$Hopeful_3 
medata$playful        <- medata$Hopeful_4 
medata$hopefulsum     <- medata$hopeful + medata$elated + medata$cheerful + medata$playful 
medata$hopefulave     <- medata$hopefulsum/4

# anxious 6
medata$anxious        <- medata$Anxious_1
medata$worried        <- medata$Anxious_2
medata$restless       <- medata$Anxious_3
medata$tense          <- medata$Anxious_4
medata$fearful        <- medata$Anxious_5
medata$hesitant       <- medata$Anxious_6
medata$anxioussum     <- medata$anxious + medata$worried + medata$restless + medata$tense + medata$fearful + medata$hesitant
medata$anxiousave     <- medata$anxioussum/6

# peaceful 2
medata$peaceful       <- medata$Peaceful_1
medata$tranquil       <- medata$Peaceful_2
medata$peacefulsum    <- medata$peaceful + medata$tranquil
medata$peacefulave    <- medata$peacefulsum/2

# frustrated 8
medata$frustrated     <- medata$Frustrated_1
medata$confused       <- medata$Frustrated_2
medata$bored          <- medata$Frustrated_3
medata$impotent       <- medata$Frustrated_4
medata$insignificant  <- medata$Frustrated_5
medata$lazy           <- medata$Frustrated_6
medata$weak           <- medata$Frustrated_7
medata$stupid         <- medata$Frustrated_8
medata$frustratedsum  <- medata$frustrated + medata$confused + medata$bored + medata$impotent + medata$insignificant + medata$lazy + medata$weak + medata$stupid 
medata$frustratedave  <- medata$frustratedsum/8

# virtuous 9
medata$virtuous       <- medata$Virtuous_1
medata$powerful       <- medata$Virtuous_2
medata$certain        <- medata$Virtuous_3
medata$intrigued      <- medata$Virtuous_4
medata$curious        <- medata$Virtuous_5
medata$capable        <- medata$Virtuous_6
medata$competent      <- medata$Virtuous_7
medata$creative       <- medata$Virtuous_8
medata$perceptive     <- medata$Virtuous_9
medata$virtuoussum    <- medata$virtuous + medata$powerful + medata$certain + medata$intrigued + medata$curious + medata$capable + medata$competent + medata$creative + medata$perceptive
medata$virtuousave    <- medata$virtuoussum/9

#############
### ANALYSES ###
#############

attach(medata)
summary(lm(shyave~loc.c+ass.c)) #
summary(lm(shyave~loc.c*ass.c)) #
summary(lm(shyave~prom.c+prev.c))
summary(lm(shyave~prom.c*prev.c))
summary(lm(shyave~loc.c+ass.c+prom.c+prev.c))

summary(lm(sleepyave~loc.c+ass.c))
summary(lm(sleepyave~loc.c*ass.c))
summary(lm(sleepyave~prom.c+prev.c))
summary(lm(sleepyave~prom.c*prev.c))
summary(lm(sleepyave~loc.c+ass.c+prom.c+prev.c))

summary(lm(calmave~loc.c+ass.c))
summary(lm(calmave~loc.c*ass.c))
summary(lm(calmave~prom.c+prev.c))
summary(lm(calmave~prom.c*prev.c))
summary(lm(calmave~loc.c+ass.c+prom.c+prev.c))

summary(lm(amazedave~loc.c+ass.c))
summary(lm(amazedave~loc.c*ass.c))
summary(lm(amazedave~prom.c+prev.c))
summary(lm(amazedave~prom.c*prev.c))
summary(lm(amazedave~loc.c+ass.c+prom.c+prev.c))

summary(lm(dejectedave~loc.c+ass.c))
summary(lm(dejectedave~loc.c*ass.c))
summary(lm(dejectedave~prom.c+prev.c))
summary(lm(dejectedave~prom.c*prev.c))
summary(lm(dejectedave~loc.c+ass.c+prom.c+prev.c))

summary(lm(hopefulave~loc.c+ass.c))
summary(lm(hopefulave~loc.c*ass.c))
summary(lm(hopefulave~prom.c+prev.c))
summary(lm(hopefulave~prom.c*prev.c))
summary(lm(hopefulave~loc.c+ass.c+prom.c+prev.c))

summary(lm(anxiousave~loc.c+ass.c))
summary(lm(anxiousave~loc.c*ass.c))
summary(lm(anxiousave~prom.c+prev.c))
summary(lm(anxiousave~prom.c*prev.c))
summary(lm(anxiousave~loc.c+ass.c+prom.c+prev.c))

summary(lm(peacefulave~loc.c+ass.c))
summary(lm(peacefulave~loc.c*ass.c))
summary(lm(peacefulave~prom.c+prev.c))
summary(lm(peacefulave~prom.c*prev.c))
summary(lm(peacefulave~loc.c+ass.c+prom.c+prev.c))

summary(lm(stupid~loc.c+ass.c, data= medata))
summary(lm(afraid~loc.c+ass.c, data=medata))
summary(lm(curious~prom.c*prev.c.c, data=medata))
summary(lm(timid~prev.c.c*prom.c, data=medata))

summary(lm(frustrated~loc.c+ass.c))
summary(lm(frustrated~loc.c*ass.c))
summary(lm(frustrated~prom.c+prev.c))
summary(lm(frustrated~prom.c*prev.c))
summary(lm(frustrated~loc.c+ass.c+prom.c+prev.c))

summary(lm(confused~loc.c+ass.c))
summary(lm(confused~loc.c*ass.c))
summary(lm(confused~prom.c+prev.c))
summary(lm(confused~prom.c*prev.c))
summary(lm(confused~loc.c+ass.c+prom.c+prev.c))

summary(lm(bored~loc.c+ass.c))
summary(lm(bored~loc.c*ass.c))
summary(lm(bored~prom.c+prev.c))
summary(lm(bored~prom.c*prev.c))
summary(lm(bored~loc.c+ass.c+prom.c+prev.c))

summary(lm(lazy~loc.c+ass.c))
summary(lm(lazy~loc.c*ass.c))
summary(lm(lazy~prom.c+prev.c))
summary(lm(lazy~prom.c*prev.c))
summary(lm(lazy~loc.c+ass.c+prom.c+prev.c))

summary(lm(stupid~loc.c+ass.c))
summary(lm(stupid~loc.c*ass.c))
summary(lm(stupid~prom.c+prev.c))
summary(lm(stupid~prom.c*prev.c))
summary(lm(stupid~loc.c+ass.c+prom.c+prev.c))

summary(lm(weak~loc.c+ass.c))
summary(lm(weak~loc.c*ass.c))
summary(lm(weak~prom.c+prev.c))
summary(lm(weak~prom.c*prev.c))
summary(lm(weak~loc.c+ass.c+prom.c+prev.c))

summary(lm(weak~loc.c+ass.c))
summary(lm(weak~loc.c*ass.c))
summary(lm(weak~prom.c+prev.c))
summary(lm(weak~prom.c*prev.c))
summary(lm(impotent~loc.c+ass.c+prom.c+prev.c))

summary(lm(weak~loc.c+ass.c))
summary(lm(weak~loc.c*ass.c))
summary(lm(weak~prom.c+prev.c))
summary(lm(weak~prom.c*prev.c))
summary(lm(insignificant~loc.c+ass.c+prom.c+prev.c))

summary(lm(virtuous~loc.c+ass.c))
summary(lm(virtuous~loc.c*ass.c))
summary(lm(virtuous~prom.c+prev.c))
summary(lm(virtuous~prom.c*prev.c))
summary(lm(virtuous~loc.c+ass.c+prom.c+prev.c))

summary(lm(powerful~loc.c+ass.c))
summary(lm(powerful~loc.c*ass.c))
summary(lm(powerful~prom.c+prev.c))
summary(lm(powerful~prom.c*prev.c))
summary(lm(powerful~loc.c+ass.c+prom.c+prev.c))

summary(lm(certain~loc.c+ass.c))
summary(lm(certain~loc.c*ass.c))
summary(lm(certain~prom.c+prev.c))
summary(lm(certain~prom.c*prev.c))
summary(lm(certain~loc.c+ass.c+prom.c+prev.c))

summary(lm(intrigued~loc.c+ass.c))
summary(lm(intrigued~loc.c*ass.c))
summary(lm(intrigued~prom.c+prev.c))
summary(lm(intrigued~prom.c*prev.c))
summary(lm(intrigued~loc.c+ass.c+prom.c+prev.c))

summary(lm(curious~loc.c+ass.c))
summary(lm(curious~loc.c*ass.c))
summary(lm(curious~prom.c+prev.c))
summary(lm(curious~prom.c*prev.c))
summary(lm(curious~loc.c+ass.c+prom.c+prev.c))

summary(lm(capable~loc.c+ass.c))
summary(lm(capable~loc.c*ass.c))
summary(lm(capable~prom.c+prev.c))
summary(lm(capable~prom.c*prev.c))
summary(lm(capable~loc.c+ass.c+prom.c+prev.c))

summary(lm(competent~loc.c+ass.c))
summary(lm(competent~loc.c*ass.c))
summary(lm(competent~prom.c+prev.c))
summary(lm(competent~prom.c*prev.c))
summary(lm(competent~loc.c+ass.c+prom.c+prev.c))

summary(lm(creative~loc.c+ass.c))
summary(lm(creative~loc.c*ass.c))
summary(lm(creative~prom.c+prev.c))
summary(lm(creative~prom.c*prev.c))
summary(lm(creative~loc.c+ass.c+prom.c+prev.c))

summary(lm(perceptive~loc.c+ass.c))
summary(lm(perceptive~loc.c*ass.c))
summary(lm(perceptive~prom.c+prev.c))
summary(lm(perceptive~prom.c*prev.c))
summary(lm(perceptive~loc.c+ass.c+prom.c+prev.c))

summary(lm(afraidave~loc.c+ass.c))
summary(lm(afraidave~loc.c*ass.c))
summary(lm(afraidave~prom.c+prev.c))
summary(lm(afraidave~prom.c*prev.c))
summary(lm(afraidave~loc.c+ass.c+prom.c+prev.c))

summary(lm(angryave~loc.c+ass.c))
summary(lm(angryave~loc.c*ass.c))
summary(lm(angryave~prom.c+prev.c))
summary(lm(angryave~prom.c*prev.c))
summary(lm(angryave~loc.c+ass.c+prom.c+prev.c))

summary(lm(guiltyave~loc.c+ass.c))
summary(lm(guiltyave~loc.c*ass.c))
summary(lm(guiltyave~prom.c+prev.c))
summary(lm(guiltyave~prom.c*prev.c))
summary(lm(guiltyave~loc.c+ass.c+prom.c+prev.c))

summary(lm(sadave~loc.c+ass.c))
summary(lm(sadave~loc.c*ass.c))
summary(lm(sadave~prom.c+prev.c))
summary(lm(sadave~prom.c*prev.c))
summary(lm(sadave~loc.c+ass.c+prom.c+prev.c))

summary(lm(happyave~loc.c+ass.c))
summary(lm(happyave~loc.c*ass.c))
summary(lm(happyave~prom.c+prev.c))
summary(lm(happyave~prom.c*prev.c))
summary(lm(happyave~loc.c+ass.c+prom.c+prev.c))

summary(lm(proudave~loc.c+ass.c))
summary(lm(proudave~loc.c*ass.c))
summary(lm(proudave~prom.c+prev.c))
summary(lm(proudave~prom.c*prev.c))
summary(lm(proudave~loc.c+ass.c+prom.c+prev.c))

summary(lm(alertave~loc.c+ass.c))
summary(lm(alertave~loc.c*ass.c))
summary(lm(alertave~prom.c+prev.c))
summary(lm(alertave~prom.c*prev.c))
summary(lm(alertave~loc.c+ass.c+prom.c+prev.c))

summary(lm(shyave~loc.c+ass.c))
summary(lm(shyave~loc.c*ass.c))
summary(lm(shyave~prom.c+prev.c))
summary(lm(shyave~prom.c*prev.c))
summary(lm(shyave~loc.c+ass.c+prom.c+prev.c))

summary(lm(sleepyave~loc.c+ass.c))
summary(lm(sleepyave~loc.c*ass.c))
summary(lm(sleepyave~prom.c+prev.c))
summary(lm(sleepyave~prom.c*prev.c))
summary(lm(sleepyave~loc.c+ass.c+prom.c+prev.c))

summary(lm(calmave~loc.c+ass.c))
summary(lm(calmave~loc.c*ass.c))
summary(lm(calmave~prom.c+prev.c))
summary(lm(calmave~prom.c*prev.c))
summary(lm(calmave~loc.c+ass.c+prom.c+prev.c))

summary(lm(amazedave~loc.c+ass.c))
summary(lm(amazedave~loc.c*ass.c))
summary(lm(amazedave~prom.c+prev.c))
summary(lm(amazedave~prom.c*prev.c))
summary(lm(amazedave~loc.c+ass.c+prom.c+prev.c))

summary(lm(dejectedave~loc.c+ass.c))
summary(lm(dejectedave~loc.c*ass.c))
summary(lm(dejectedave~prom.c+prev.c))
summary(lm(dejectedave~prom.c*prev.c))
summary(lm(dejectedave~loc.c+ass.c+prom.c+prev.c))

summary(lm(hopefulave~loc.c+ass.c))
summary(lm(hopefulave~loc.c*ass.c))
summary(lm(hopefulave~prom.c+prev.c))
summary(lm(hopefulave~prom.c*prev.c))
summary(lm(hopefulave~loc.c+ass.c+prom.c+prev.c))

summary(lm(anxiousave~loc.c+ass.c))
summary(lm(anxiousave~loc.c*ass.c))
summary(lm(anxiousave~prom.c+prev.c))
summary(lm(anxiousave~prom.c*prev.c))
summary(lm(anxiousave~loc.c+ass.c+prom.c+prev.c))

summary(lm(peacefulave~loc.c+ass.c))
summary(lm(peacefulave~loc.c*ass.c))
summary(lm(peacefulave~prom.c+prev.c))
summary(lm(peacefulave~prom.c*prev.c))
summary(lm(peacefulave~loc.c+ass.c+prom.c+prev.c))


summary(lm(frustrated~loc.c+ass.c))
summary(lm(frustrated~loc.c*ass.c))
summary(lm(frustrated~prom.c+prev.c))
summary(lm(frustrated~prom.c*prev.c))
summary(lm(frustrated~loc.c+ass.c+prom.c+prev.c))

summary(lm(confused~loc.c+ass.c))
summary(lm(confused~loc.c*ass.c))
summary(lm(confused~prom.c+prev.c))
summary(lm(confused~prom.c*prev.c))
summary(lm(confused~loc.c+ass.c+prom.c+prev.c))

summary(lm(bored~loc.c+ass.c))
summary(lm(bored~loc.c*ass.c))
summary(lm(bored~prom.c+prev.c))
summary(lm(bored~prom.c*prev.c))
summary(lm(bored~loc.c+ass.c+prom.c+prev.c))

summary(lm(lazy~loc.c+ass.c))
summary(lm(lazy~loc.c*ass.c))
summary(lm(lazy~prom.c+prev.c))
summary(lm(lazy~prom.c*prev.c))
summary(lm(lazy~loc.c+ass.c+prom.c+prev.c))

summary(lm(stupid~loc.c+ass.c))
summary(lm(stupid~loc.c*ass.c))
summary(lm(stupid~prom.c+prev.c))
summary(lm(stupid~prom.c*prev.c))
summary(lm(stupid~loc.c+ass.c+prom.c+prev.c))

weak <- mean(weak, impotent, insignificant)
summary(lm(weak~loc.c+ass.c))
summary(lm(weak~loc.c*ass.c))
summary(lm(weak~prom.c+prev.c))
summary(lm(weak~prom.c*prev.c))
summary(lm(weak~loc.c+ass.c+prom.c+prev.c))

summary(lm(virtuous~loc.c+ass.c))
summary(lm(virtuous~loc.c*ass.c))
summary(lm(virtuous~prom.c+prev.c))
summary(lm(virtuous~prom.c*prev.c))
summary(lm(virtuous~loc.c+ass.c+prom.c+prev.c))

summary(lm(powerful~loc.c+ass.c))
summary(lm(powerful~loc.c*ass.c))
summary(lm(powerful~prom.c+prev.c))
summary(lm(powerful~prom.c*prev.c))
summary(lm(powerful~loc.c+ass.c+prom.c+prev.c))

summary(lm(certain~loc.c+ass.c))
summary(lm(certain~loc.c*ass.c))
summary(lm(certain~prom.c+prev.c))
summary(lm(certain~prom.c*prev.c))
summary(lm(certain~loc.c+ass.c+prom.c+prev.c))

summary(lm(intrigued~loc.c+ass.c))
summary(lm(intrigued~loc.c*ass.c))
summary(lm(intrigued~prom.c+prev.c))
summary(lm(intrigued~prom.c*prev.c))
summary(lm(intrigued~loc.c+ass.c+prom.c+prev.c))

summary(lm(curious~loc.c+ass.c))
summary(lm(curious~loc.c*ass.c))
summary(lm(curious~prom.c+prev.c))
summary(lm(curious~prom.c*prev.c))
summary(lm(curious~loc.c+ass.c+prom.c+prev.c))

summary(lm(capable~loc.c+ass.c))
summary(lm(capable~loc.c*ass.c))
summary(lm(capable~prom.c+prev.c))
summary(lm(capable~prom.c*prev.c))
summary(lm(capable~loc.c+ass.c+prom.c+prev.c))

summary(lm(competent~loc.c+ass.c))
summary(lm(competent~loc.c*ass.c))
summary(lm(competent~prom.c+prev.c))
summary(lm(competent~prom.c*prev.c))
summary(lm(competent~loc.c+ass.c+prom.c+prev.c))

summary(lm(creative~loc.c+ass.c))
summary(lm(creative~loc.c*ass.c))
summary(lm(creative~prom.c+prev.c))
summary(lm(creative~prom.c*prev.c))
summary(lm(creative~loc.c+ass.c+prom.c+prev.c))

summary(lm(perceptive~loc.c+ass.c))
summary(lm(perceptive~loc.c*ass.c))
summary(lm(perceptive~prom.c+prev.c))
summary(lm(perceptive~prom.c*prev.c))
summary(lm(perceptive~loc.c+ass.c+prom.c+prev.c))

detach(medata)



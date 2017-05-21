# sound generator loop



# Andrey Anikin's (andrey.anikin@lucs.lu.se) parametric sound generator:
# I used the Oct 2016 version
# https://cogsci.shinyapps.io/shinyApp/
source("sound_generator.R")



# permittedValues



get_vowel <- function(vowel = "e", formantStrength = 45, speaker = "M1", durTotal = 300) {

    a  <- permittedValues[,1]
    a["durTotal"] <- durTotal

    pf <- presets[[speaker]]$Formants[[vowel]]

    a["formantStrength"] <- formantStrength
    a["proportionNoisy"] <- 0
    a["megaFormant_strength"] <- 30

    input = as.list(a)
    s = generateBout (nSyl=input$nSyl, durTotal=input$durTotal, pauseDuration_mean=input$pauseDuration_mean,
                      var_within_syllables=input$var_within_syllables, proportionNoisy=input$proportionNoisy,
                      var_bw_syllables=input$var_bw_syllables, attackLen=input$attackLen,
                      pitch_start=input$pitch_start, pitch_anchor=input$pitch_anchor,
                      pitch_end=input$pitch_end, pitch_anchor_location=input$pitch_anchor_location,
                      jitterDep=input$jitterDep, vibratoLen=input$vibratoLen, vibratoDep=input$vibratoDep,
                      shimmerDep=input$shimmerDep, driftDep=input$driftDep, formantStrength=input$formantStrength,
                      creakyBreathy=input$creakyBreathy, rolloff=input$rolloff, breathingStrength=input$breathingStrength,
                      breathingStrength_diff=input$breathingStrength_diff, breathing_dur=input$breathing_dur,
                      breathingType=input$breathingType, playSound=F, spectralSlope=input$spectralSlope,
                      spectralNoise_strength=input$spectralNoise_strength, megaFormant_strength=input$megaFormant_strength,
                      nSubharm=input$nSubharm, subharmDep=input$subharmDep, proportion_octaveJumps=input$proportion_octaveJumps,
                      nFormants=nrow(pf), randomVowel = F, exactFormants=pf)

    return(s)
}
#
#
# s <- get_vowel()
# savewav(s,44100, "test.wav")

folder <- "wav/"

# allsounds <- vector()

for (v in c("e", "i", "u", "o")) {
    for (s in c(seq(5,26, 3),60)) {

        sound <- get_vowel(v,s, durTotal = 450)
        # allsounds <- c(allsounds, sound)
        savewav(sound, 44100, stringr::str_c(folder, v, s,".wav"))
    }
}

# savewav(allsounds, 44100, "full.wav")



# a = generateBout (nSyl=1, durTotal=300, randomVowel=T, pitch_start = 100, pitch_anchor=100, pitch_end=200, pitch_anchor_location=90)

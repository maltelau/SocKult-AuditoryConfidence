# Predicting collective benefit from the language of interaction partners

This is the public repository for all data and code related to the project

Malte Lau Petersen, Cognitive Science, Århus University
Exam paper for Social and Cultural Dynamics

### Abstract
How do dyads that successfully collaborate coordinate their communication? Some tasks, such as coming up with words or foraging, can easily be parallelized, and the results from many individuals can simply be added together. Other tasks require collaborative effort in an ongoing social interaction to benefit from multiple people working on the same problem. But why do some groups benefit more from collaboration than others? Language is central for collaboration, and various mechanisms have been proposed to explain differences in collective benefit. This paper extends a previously reported experimental setup (Bahrami et al. 2010, Fusaroli et al 2012a, Bang et al. 2014, Fusaroli & Tylén 2015, Bang et al 2017) to the auditory domain – and compares linguistic similarity and diversity, linguistic alignment and synergy, and explicit metacognition as predictors of collective benefit in a bayesian computational model.

Keywords: social cognition; group performance; linguistic alignment; bayesian modelling; auditory discrimination

### Index
Most of the R code that was actuall used is in the R/ folder, but some of it is in the Analysis.Rmd file for now.

In data/ the raw data is in Chat messages (accessed 2017-04-26).csv and confidence_audio (accessed 2017-04-26).csv and the other files are intermediate, tidied up data.

model backup/ holds some models I decided to abandon (too complex for now with multiple predictors)

model/ holds the stan models and some summary tables. This is also where R/run_models.R will store the samples in Rdata files ending in .model

pilot data/ holds some data and initial look at that data from a pilot I ran with just four participants for a shorter time

wav/ holds the stimulus sound files named `[vowel][intensity].wav`

The excellently named odt and pdf file `Paper` obviously holds the paper I handed in ..

confidence_audio/ has the otree app used to run the experiment

and finally, codingscheme.txt has the regular expressions used to extract confidence expressions. I got this from by contacting Fusaroli as it was used in his 2012 paper.

Fusaroli, R., Bahrami, B., Olsen, K., Roepstorff, A., Rees, G., Frith, C., & Tylén, K. (2012). Coming to terms: quantifying the benefits of linguistic coordination. Psychological science, 23(8), 931-939.
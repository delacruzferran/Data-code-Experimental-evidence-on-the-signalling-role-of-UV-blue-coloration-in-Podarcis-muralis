This file contains information on the data and metadata for the following manuscript:
Experimental evidence on the signaling role of UV-blue coloration in male European wall lizards (Podarcis muralis)

Ferran de la Cruz*1,2,3, Shania Lam4, Ella Guilhem5, Miguel Ángel Carretero1,2,3, Guillem Pérez i de Lanuza6 & Enrique Font6

1 CIBIO Centro de Investigação em Biodiversidade e Recursos Genéticos, InBIO Laboratório Associado, Campus de Vairão, Universidade do Porto, 4485-661 Vairão, Portugal
2 Departamento de Biologia, Faculdade de Ciências, Universidade do Porto, 4099-002 Porto, Portugal
3 BIOPOLIS Program in Genomics, Biodiversity and Land Planning, CIBIO, Campus de Vairão, 4485-661 Vairão, Portugal
4 Faculty of Science, University of Amsterdam, 1098 XH Amsterdam, Netherlands
5 Faculty of Science and Engineering, University of Toulouse III Paul Sabatier, 31400 Toulouse, France
6 Ethology Lab, Cavanilles Institute of Biodiversity and Evolutionary Biology, University of Valencia, 46071 Valencia, Spain

*Corresponding author: Ferran de la Cruz, delacruz.ferran@gmail.com


The files uploaded to Github include the following:

# Morphometry.csv

This file contains morphometric and capture information for all lizards.

 	# ID: Identification of the lizard.
 	# Sex: Sex of the lizard (F-Female; M-Male).
 	# Morph: Ventral morph of the lizard (W-White; Y-Yellow; O-Orange; WO-White orange; YO-Yellow orange).
 	# Type: Group of the lizard (F-Female; Big-Large male; Small-Small male).
 	# SVL: Snout-vent length in mm.
 	# HL: Head length in mm.
 	# HW: Head width in mm.
 	# Body mass: Body mass in g.
 	# BCI: Body index condition as the residual from a least-squares linear regression of log(bodymass) against log(SVL) calculted by 	Type of lizard.
 	# Location: Population of capture.
 	# Date: Date of capture.

# Trials.csv

This file contains all the information regarding the trials.
 
 	# Number: Number of trial.
 	# ID: Identification of the large male.
 	# Trial: Number of trial of the large male (1-8).
 	# Group: Group of small lizard (Control or Treatment).
 	# Rival_ID: Identification of the small male.
 	# Rival_trial: Number of trial of the small male (1-3).
 	# Date: Date of trial.
 	# Time: Time of the day for each trial.
 	# Duration_1: Total duration of the trial.
 	# Duration_2: Duration since large male responded to the presence of the small male.
 	# Time_see: Time when large male responded to the presence of the small male.
 	# Time first rock: Time when small male went into the High-quality territory.
 	# Duration_both: Duration both lizards were in the High-quality territory.
 	# Duration_rock: Duration small lizard was in the High-quality territory.
 	# Encounter: Number of encounters.
 	# B_: Number of specific behaviors displayed by the large male.
 	# B_aggre: Number of aggressive behaviors displayed by the large male (Display, Bite, Lunge, Mouth gaping, Chase).
 	# B_submi: Number of submissive behaviors displayed by the large male (Flight, Foot shake, Tail shake).
 	# B_aggre_enc: Ratio of aggressive behaviors displayed by the large male (B_aggre/Encounters).
 	# B_submi: Ratio of submissive behaviors displayed by the large male (B_submi/Encounters).
 	# s_: Number of specific behaviors displayed by the small male.
 	# s_aggre: Number of aggressive behaviors displayed by the small male (Display, Bite, Lunge, Mouth gaping, Chase).
 	# s_submi: Number of submissive behaviors displayed by the small male (Flight, Foot shake, Tail shake).
 	# s_aggre_enc: Ratio of aggressive behaviors displayed by the small male (B_aggre/Encounters).
 	# s_submi: Ratio of submissive behaviors displayed by the small male (B_submi/Encounters).
 	# Size_dif: Body size difference in mm between large and small males.
 	# Acclimation: Time of acclimation for the large male (M-1 day; W-2 days).
 	# T_moving: Time large male was moving during the pre-trial observation period.
 	# T_resting: Time large male was resting or basking during the pre-trial observation period.
 	# T_female: Time large male was interacting with the female during pre-trial observation period.
 	# IB: Number of Interaction with Boundaries of the large male during pre-trial observation period.
 	# T1: Temperature of the chamber at the beginnig of the trial.
 	# H1: Humidity of the chamber at the beginnig of the trial.
 	# T2: Temperature of the chamber at the end of the trial.
 	# H2: Humidity of the chamber at the end of the trial.
 	# Population_B: Population of the large male.
 	# Lat_first: Latency to first aggressive behaviour of the large male (calculated in Behaviour.csv).
 	# Frequency: Frequency of aggressive behaviours of the large male (B_aggre/Duration_2).

# Behaviours.csv

This file contains behaviors for each pair of lizards with time of encounters. This file was used to obtain latency to first aggressive behavior.

 	# Trial: Number of trial.
 	# Time: Time of encounter.
 	# B1-B9: Behaviour displayed by the large male (B-Bite; D-Display; MG-Mouth gaping; L-Lunge; C-Chase; F-Flight; FS-Foot shake; TS-	Tail shake).
 	# s1-s6: Behaviour displayed by the small male (B-Bite; D-Display; MG-Mouth gaping; L-Lunge; C-Chase; F-Flight; FS-Foot shake; TS-	Tail shake).
 	# Time_aggre: Time of first aggressive displapey by the large male.
 	# Time_start: Time of beginning of the trial.
 	# Time_see: Minutes passed until large male responded to the presence of small male.
 	# Time_lat: Time to calculate latency to first aggression (Time_start + Time_see).
 	# Lat_first: Latency to first aggression by large male (Time_aggre - Time_lat).

# Spectra.csv

This file contains spectral data for Figure 2

 	# wl: Wavelength in nm (300-700).
 	# UV_control: Spectrum of the UV-blue patch unmanipulated.
 	# Dor_control: Spectrum of the dorsum painted.
 	# UV_control: Spectrum of the UV-blue patch painted.
 	# Dor_control: Spectrum of the dorsum unmanipulated.

# Code.R

This file contains the R script for running statistical analyses and plot figures.

All analyses were run in R 4.3.1.

Required packages:
 	# library(lme4)
 	# library(nlme)
 	# library(ggplot2)
 	# library(lubridate)
 	# library(hms)
 	# library(car)
 	# library(MuMIn)
 	# library(tidyverse)
 	# library(performance)
 	# library(see)
 	# library(effects)
 	# library(corrplot)
 	# library(stats)
 	# library(Hmisc)
 	# library(igraph)
 	# library(ggraph)
 	# library(pavo)

# labelling script

library(labelled)

outcomedatlist  <- list(
  cad = "CAD",
  afib = "Atrial Fibrillation",
  stroke = "Stroke",
  hypertension = "Hypertension",
   diabetes_t2 = "Type 2 Diabetes")
covardatlist  <- list(
    age_when_attended_assessment_centre_f21003_0_0="Baseline age (21003)",
    standing_height_f50_0_0="Height (cm) (50)",
    body_mass_index_bmi_f21001_0_0= "BMI (21001)",
    waist_circumference_f48_0_0= "Waist circumference (cm) (48)",
    hip_circumference_f49_0_0= "Hip circumference (cm) (49)",
    body_fat_percentage_f23099_0_0= "Bodyfat % (23099)",
    trunk_fat_percentage_f23127_0_0= "Trunk fat % (23127)",
    impedance_of_whole_body_f23106_0_0= "Whole body impedance (23106)",
    current_smoker= "Current Smoker (1239)",
    had_menopause = "Had menopause",
    ever_had_hysterectomy = "Hysterectomy (ever) (3591)",
    age_at_hysterectomy_f2824_0_0="Age at Hysterectomy (2824)",
    ever_used_hrt = "HRT Use (ever) (2814)",
    age_started_hormonereplacement_therapy_hrt_f3536_0_0= "Age started HRT (3536)",
    age_last_used_hormonereplacement_therapy_hrt_f3546_0_0="Age last used HRT (3546)",
    pulse_rate_automated_reading_f102_0_0= "Pulse (automated) (102)",
    systolic_blood_pressure_manual_reading_f93_0_0= "Systolic BP (manual) (93)",
    diastolic_blood_pressure_manual_reading_f94_0_0="Diastolic BP (manual) (94)",
    diastolic_blood_pressure_automated_reading_f4079_0_0= "Diastolic BP (auto) (4079)",
    systolic_blood_pressure_automated_reading_f4080_0_0= "Systolic BP (auto) (4080)",
    pulse_rate_during_bloodpressure_measurement_f95_0_0= "Pulse during BP (manual) (95)",
    pulse_wave_arterial_stiffness_index_f21021_0_0= "Pulse wave arterial stiffness (21021)",
    peak_expiratory_flow_pef_f3064_0_0= "PEF (3064)",
    glycated_haemoglobin_hba1c_f30750_0_0="HbA1c (30750)",
    heel_bone_mineral_density_bmd_tscore_automated_right_f4125_0_0= "Heel BMD (auto, Tscore) (4125)",
    heel_bone_mineral_density_bmd_tscore_manual_entry_left_f4138_0_0="Heel BMD (manual, Tscore) (4138)",
    heel_bone_ultrasound_tscore_manual_entry_f77_0_0= "Heel bone ultrasound (manual, Tscore) (77)",
    heel_bone_mineral_density_bmd_tscore_automated_f78_0_0="Heel BMD (automated, Tscore) (78)"
)
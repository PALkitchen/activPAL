format.summary.outcomes <-
  function(summary_data){
    original_column_names <- c("period_date", "period_name", "period_start", "period_end", "uid",
                               "Time in Bed (< 30 min)", "Time in Bed (30 min - 1 hour)", "Time in Bed (1 - 2 hours)", "Time in Bed (2 - 4 hours)", "Time in Bed (4 hours +)", "Time in Bed Breaks",
                               "Sedentary (< 30 min)", "Sedentary (30 min - 1 hour)", "Sedentary (1 - 2 hours)", "Sedentary (2 - 4 hours)", "Sedentary (4 hours +)",
                               "Steps", "Quiet Standing", "Stepping (< 1 minute)", "Stepping (1 - 10 minutes)", "Stepping (10 minutes +)",
                               "Non Wear",
                               "LPA (< 75 spm)_short (< 60s)", "LPA (< 75 spm)_long (>= 60s)", "MPA (75 - 100 spm)_short (< 60s)", "MPA (75 - 100 spm)_long (>= 60s)",
                               "MVPA (100 - 125 spm)_short (< 60s)", "MVPA (100 - 125 spm)_long (>= 60s)", "VPA (> 125 spm)_short (< 60s)", "VPA (> 125 spm)_long (>= 60s)",
                               "Median Cadence < 1 minute", "Median Cadence 1 - 10 minutes", "Median Cadence 10 minutes +",
                               "Active_Walking", "Indoor_Walking", "Cycling", "Seated_Transport",
                               "peak_steps_30_seconds", "peak_steps_2_minute", "peak_steps_6_minute", "peak_steps_12_minute",
                               "total_events", "total_duration", "sedentary_events", "sedentary_duration", "standing_events", "standing_duration", "stepping_events", "stepping_duration",
                               "cycling_events", "cycling_duration", "lying_events", "lying_duration", "non_wear_events", "non_wear_duration", "no_data_duration")

    mapped_column_names <- c("SliceDate", "SliceName", "SliceStartTime", "SliceEndTime", "FileCode",
                             "tLyingBouts_<30m_hours", "tLyingBouts_30m<1h_hours", "tLyingBouts_1h<2h_hours", "tLyingBouts_2h<4h_hours", "tLyingBouts_>4h_hours", "nLyingBreaks",
                             "tSedBouts_<30m_hours", "tSedBouts_30m<1h_hours", "tSedBouts_1h<2h_hours", "tSedBouts_2h<4h_hours", "tSedBouts_>4h_hours",
                             "nSteps", "StandingTime(min)", "tSteppingBouts_<1m_hours", "tSteppingBouts_1m<10m_hours", "tSteppingBouts_>10m_hours",
                             "Non_Wear",
                             "tSteppingBouts_<c75_<1m_min","tSteppingBouts_<c75_>1m_min", "tSteppingBouts_c75<c100_<1m_min", "tSteppingBouts_c75<c100_>1m_min",
                             "tSteppingBouts_c100<c125_<1m_min", "tSteppingBouts_c100<c125_>1m_min", "tSteppingBouts_>c125_<1m_min", "tSteppingBouts_>c125_>1m_min",
                             "cPreferred_<1m", "cPreferred_1m<10m", "cPreferred_>10m",
                             "tSteppingBouts_UC_<1m_hours", "tSteppingBouts_UC_>1m_hours", "CyclingTime(hr)", "SeatedTransportTime(hr)",
                             "nPeakSteps_30s", "nPeakSteps_2m", "nPeakSteps_6m", "nPeakSteps_12m",
                             "nEvents", "tEvents_min", "nSed", "tSed_min", "nStand", "tStand_min", "nStepping", "tSteppingBouts_min",
                             "nCycling", "tCycling_min", "nPrimLying", "tPrimLying_min", "nNonWear", "tNonWear_min", "tNoData_min")

    mapped_column_order <- c(5,2,1,3,4,43,42,
                             17,49,48,51,50,47,46,45,
                             44,53,52,55,54,56,19,20,
                             21,34,35,23,24,25,26,27,28,29,
                             30,11,12,13,14,15,6,
                             7,8,9,10,16,38,39,40,
                             41,31,32,33)

    summary_data <- rename.summary.outcomes.columns(summary_data, original_column_names, mapped_column_names)
    summary_data <- reorder.summary.outcomes.folder(summary_data, mapped_column_names, mapped_column_order)
  }

rename.summary.outcomes.columns <-
  function(summary_data, original_column_names, mapped_column_names){
    for(i in (1:length(original_column_names))){
      pos <- which(colnames(summary_data) == original_column_names[i])
      if(length(pos) == 1){
        colnames(summary_data)[pos] <- mapped_column_names[i]
      }
    }

    return(summary_data)
  }

reorder.summary.outcomes.folder <-
  function(summary_data, mapped_column_names, mapped_column_order){

    summary_data <- summary_data[,c(which(colnames(summary_data) %in% mapped_column_names))]
    summary_data <- summary_data[,mapped_column_order]
    return(summary_data)
  }

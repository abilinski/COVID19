calibration_p_d <- function(){
  #manipulate data for calibration target
  CA_daily <- read.csv(system.file('states-daily.csv', package='covid.epi')) %>% filter(state=="CA")
  #calibration target mapping to model output
  #colnames(CA_daily)
  #positive[t] -- cum_DI[t] (assuming k_det_a==rdetecta==0 because now there are nearly none asymptomatic tested)
  #death[t] -- cum_D[t]
  #death[t]/constant fatality = 0.04%? -- cum_I[t]
  #total/totalTestResults -- how to use this data?
  #        Tested      Untested     Total
  # Sym    DI+flu_sym  UI           n*alpha
  # Asym   DA==0?      UA+flu_asym
  # Total  total       n-total      n
  # note, the Sym and Asym above can be flu too
}

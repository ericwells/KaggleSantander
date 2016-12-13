# Awk script to convert "P1" to "ind_ahor_fin_ult1" etceteras

BEGIN {FS = ","}
{gsub("P24","ind_recibo_ult1");
gsub("P23","ind_nom_pens_ult1");
gsub("P22","ind_nomina_ult1");
gsub("P21","ind_viv_fin_ult1");
gsub("P20","ind_valo_fin_ult1");
gsub("P19","ind_tjcr_fin_ult1");
gsub("P18","ind_reca_fin_ult1");
gsub("P17","ind_pres_fin_ult1");
gsub("P16","ind_plan_fin_ult1");
gsub("P15","ind_hip_fin_ult1");
gsub("P14","ind_fond_fin_ult1");
gsub("P13","ind_ecue_fin_ult1");
gsub("P12","ind_dela_fin_ult1");
gsub("P11","ind_deme_fin_ult1");
gsub("P10","ind_deco_fin_ult1");
gsub("P9","ind_ctpp_fin_ult1");
gsub("P8","ind_ctop_fin_ult1");
gsub("P7","ind_ctma_fin_ult1");
gsub("P6","ind_ctju_fin_ult1");
gsub("P5","ind_cno_fin_ult1");
gsub("P4","ind_cder_fin_ult1");
gsub("P3","ind_cco_fin_ult1");
gsub("P2","ind_aval_fin_ult1");
gsub("P1","ind_ahor_fin_ult1");
print $0}

# reminder on how to run it:  awk -f test.awk test.csv > output.csv
# awk -f postprocess_mysolutioncsv.awk ../ksb_model4_solution_temp.csv > ../ksb_model4_solution.csv

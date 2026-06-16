#' Create website upload CSV
#' 
#' To create the CSV needed to upload reports to the OBI website. The CSV files should be in the following [format](https://docs.google.com/spreadsheets/d/19Ua83FMIsT9RjrugrvPD4NJEZcfI8e_cBFfrudDSDeM/edit#gid=141729511) 
#' (see the [SOP](https://docs.google.com/document/d/1g0BcXkA-rcm0AAtkTjK9uE7Bh2o5NzB_fBr_UDr_niw/edit) for more details). If you are uploading a report multiple times in a month and would like to overwrite past versions, 
#' you do not need to re-run this function; you can reuse the previous CSV as long as the report name remains unchanged. If you do NOT want to overwrite previous versions uploaded in the same month, you will need to ensure
#' the report does not have the same name as the previous version.
#' 
#' @param main_data The input dataset containing the main wide dataset or analytictable. 
#' @param cervical_data The input dataset containing the Cervical and Amniotomy cangrow. 
#' @param induction_data The input dataset containing the Induction cangrow. 
#' @param group A logical value that indicates whether the data should be grouped. Default is FALSE. 
#' @param ... Grouping arguments
#' 
#' @return If group = F, a dataset contain patient-level missingness counts is returned. If group = T, a dataset with the group-level missingness counts and rate is returend. 
#' @examples 
#' /dontrun{
#' library(tidyverse)
#' 
#' test <- iol_ebp_missingness(obi_26_dt, cerv_cangrow, iol_cangrow)
#' 
#' grouped_test <- iol_ebp_missingness(obi_26_dt, cerv_cangrow, iol_cangrow, group = T, site_name)
#' }
#' 
#' @import tidyr
#' @export


iol_ebp_missingness <- function(main_data, cervical_data, induction_data, group = F, ...) {
  
  missingness_main_dt <- main_data |> 
    filter(infant_year >= 2026) |> 
    mutate(
      # required variables - start with 0 and add as appropriate
      main_iol_var_ct = 0,
      ## cervical dilation at admission
      ## needed for pts w/ planned mode of delivery = vag AND 
      ## actual mode != planned 
      main_iol_var_ct = case_when(
        planned_mode_of_delivery_cd == 1 &
          mode_of_delivery_cd %in% c(1, 2, 3, 4) ~ main_iol_var_ct + 1,
        TRUE ~ main_iol_var_ct
      ),
      ## date/time ROM 
      ## needed for pts w/ planned mode of delivery = vag
      main_iol_var_ct = case_when(
        planned_mode_of_delivery_cd == 1 ~ main_iol_var_ct + 1,
        TRUE ~ main_iol_var_ct
      ),
      ## ROM type
      ## needed for pts in IOL denominator 
      main_iol_var_ct = case_when(
        iol_ebp_denom_flg == 1 ~ main_iol_var_ct + 1,
        TRUE ~ main_iol_var_ct
      ),
      ## Attending/CNM 4.5 hours after first 4cm exam
      ## needed for pts who are noncompliant for early amnnio 4.5 hours after first 
      ## 4 cm exam
      main_iol_var_ct = case_when(
        amnio_offer_before_4cm_flg == 0 & 
          amnio_not_offered_before_4cm_acceptable_flg == 1 &
          amnio_offer_after_4cm_flg == 0 & 
          amnio_not_offered_after_4cm_acceptable_flg == 0 ~ main_iol_var_ct + 1,
        TRUE ~ main_iol_var_ct
      ),
      ## Reason mech agent not attempted w/in 30 min of first chem agent
      ## needed for pts whose first admin agent was chem and who did not get offered
      ## a mech agent in 30 mins
      main_iol_var_ct = case_when(
        chem_first_iol_flg == 1 & 
          chem_to_mech_offer_30M_flg == 0 ~ main_iol_var_ct + 1,
        TRUE ~ main_iol_var_ct
      ),
      ## Reason mech agent not attempted w/in 4.5H of first chem agent
      ## needed for pts whose first admin agent was chem and who were offered a 
      ## mech agent in 30 mins but declined/unsuccessful and mech agent not offered
      ## w/in 4.5H
      main_iol_var_ct = case_when(
        chem_first_iol_flg == 1 & 
          chem_to_mech_offer_30M_flg == 1 &
          chem_to_mech_administered_30M_flg == 0 &
          chem_to_mech_not_administered_30M_acceptable_flg == 1 & 
          chem_to_mech_offer_4H_flg == 0 ~ main_iol_var_ct + 1,
        TRUE ~ main_iol_var_ct
      ),
      ## Attending info for dual agent noncompliance
      ## needed for anyone noncompliant for dual agent ripening
      main_iol_var_ct = case_when(
        dual_agent_compliant_flg == 0 ~ main_iol_var_ct + 1,
        TRUE ~ main_iol_var_ct
      ),
      # missing variables
      ## cervical dilation at admission
      missing_admit_cerv_dil_flg = case_when(
        planned_mode_of_delivery_cd == 1 &
          mode_of_delivery_cd %in% c(1, 2, 3, 4) & 
          is.na(admit_cerv_dil_no_calc) ~ 1 ,
        TRUE ~ 0
      ),
      ## date/time ROM 
      missing_dttm_rom_flg = case_when(
        planned_mode_of_delivery_cd == 1 & 
          rupt_mem_dt_not_documented_b == 1 &
          rupt_mem_before_admit_b != 1 ~ 1,
        TRUE ~ 0
      ),
      ## ROM type
      missing_rom_type_flg = case_when(
        iol_ebp_denom_flg == 1 & 
          rupt_mem_cd == 3 ~ 1,
        TRUE ~ 0
      ),
      ## Attending/CNM 4.5 hours after first 4cm exam
      missing_attending_4h_amnio_flg = case_when(
        amnio_offer_before_4cm_flg == 0 & 
          amnio_not_offered_before_4cm_acceptable_flg == 1 &
          amnio_offer_after_4cm_flg == 0 & 
          amnio_not_offered_after_4cm_acceptable_flg == 0 & 
          attending_amnio_4H_not_recommend_not_doc_b ~ 1,
        TRUE ~ 0
      ),
      ## Reason mech agent not attempted w/in 30 min of first chem agent 
      missing_reason_mech_30m_after_chem_flg = case_when(
        chem_first_iol_flg == 1 & 
          chem_to_mech_offer_30M_flg == 0 & 
          is.na(mechanical_method_not_attempted_within_30M_chemical_method_reason_e) ~ 1,
        chem_first_iol_flg == 1 & 
          chem_to_mech_offer_30M_flg == 0 &
          mechanical_method_not_attempted_within_30M_chemical_method_reason_e == 99 ~ 1,
        TRUE ~ 0
      ),
      ## Reason mech agent not attempted w/in 4.5H of first chem agent
      missing_reason_mech_4H_after_chem_flg = case_when(
        chem_first_iol_flg == 1 & 
          chem_to_mech_offer_30M_flg == 1 &
          chem_to_mech_administered_30M_flg == 0 &
          chem_to_mech_not_administered_30M_acceptable_flg == 1 & 
          chem_to_mech_offer_4H_flg == 0 & 
          is.na(mechanical_method_not_attempted_within_4H_chemical_method_reason_e) ~ 1,
        chem_first_iol_flg == 1 & 
          chem_to_mech_offer_30M_flg == 1 &
          chem_to_mech_administered_30M_flg == 0 &
          chem_to_mech_not_administered_30M_acceptable_flg == 1 & 
          chem_to_mech_offer_4H_flg == 0 & 
          mechanical_method_not_attempted_within_4H_chemical_method_reason_e == 99 ~ 1,
        TRUE ~ 0
      ),
      ## Attending info for dual agent noncompliance
      missing_attending_dual_agent_noncompliant_flg = case_when(
        dual_agent_compliant_flg == 0 & 
          ((is.na(attending_30M_post_chemical_ripening_agent) |
              attending_30M_post_chemical_ripening_agent_not_doc_b == 1)) &
          ((is.na(attending_4H_post_chemical_ripening_agent) |
              attending_4H_post_chemical_ripening_agent_not_doc_b == 1)) &
          ((is.na(attending_30M_post_mechanical_ripening_agent) |
              attending_30M_post_mechanical_ripening_agent_ND == 1)) &
          ((is.na(attending_4H_post_mechanical_ripening_agent) |
              attending_4H_post_mechanical_ripening_agent_ND == 1)) ~ 1, 
        TRUE ~ 0
      ), 
      # sum missing flags 
      main_iol_missing_ct = missing_admit_cerv_dil_flg + 
        missing_dttm_rom_flg + 
        missing_rom_type_flg + 
        missing_attending_4h_amnio_flg + 
        missing_reason_mech_30m_after_chem_flg + 
        missing_reason_mech_4H_after_chem_flg + 
        missing_attending_dual_agent_noncompliant_flg
    ) |> 
    select(patientid, main_iol_missing_ct, main_iol_var_ct)
  
  cerv_missingness_dt <- cervical_data |> 
    right_join(
      obi_26_dt |> 
        select(
          patientid, 
          iol_ebp_denom_flg,
          mode_of_delivery_cd,
          early_amniotomy_compliant_flg,
          first_cerv_exam_4cm_dttm,
          rupt_mem_dt,
          admission_dt,
          rupt_mem_before_admit_b
        )
    ) |> 
    mutate(
      # if complete time is missing, offer is used for some vars
      cerv_exam_combined_dttm = case_when(
        is.na(cervical_exam_dttm) ~ cervical_exam_offer_dttm, 
        TRUE ~ cervical_exam_dttm
      ),
      # calculate time from 4cm to other exams
      time_first_4cm_exam_to_other_exams = difftime(cerv_exam_combined_dttm, first_cerv_exam_4cm_dttm, units = "mins"),
      # required variables - start with 0 and add as appropriate
      cerv_iol_var_ct = 0,
      ## cervical dilation 
      ## needed for patients in the IOL denominator for all exams between admit date
      ## and ROM or 4.5 hours after 4cm exam. 
      cerv_iol_var_ct = case_when(
        iol_ebp_denom_flg == 1 & 
          cerv_exam_combined_dttm > admission_dt & 
          (cerv_exam_combined_dttm < rupt_mem_dt |
             time_first_4cm_exam_to_other_exams < 270) ~ cerv_iol_var_ct + 1,
        TRUE ~ cerv_iol_var_ct
      ),
      ## Date/time of cervical exam offer OR date/time cervical exam completed
      ## needed for all exam for pts with mode of delivery != planned 
      cerv_iol_var_ct = case_when(
        mode_of_delivery_cd %in% c(1, 2, 3, 4) ~ cerv_iol_var_ct + 1,
        TRUE ~ cerv_iol_var_ct
      ),
      ## Amniotomy offered with cervical exam
      ## needed for pts in IOL denominator for all exams > 0 between admit date
      ## and ROM or 4.5 hours after 4cm exam
      cerv_iol_var_ct = case_when(
        iol_ebp_denom_flg == 1 & 
          cerv_exam_combined_dttm > admission_dt & 
          (cerv_exam_combined_dttm < rupt_mem_dt |
             time_first_4cm_exam_to_other_exams < 270) & 
          cervical_exam_dilation_no > 0 ~ cerv_iol_var_ct + 1,
        TRUE ~ cerv_iol_var_ct
      ),
      ## Primary reason that amniotomy was not offered
      ## needed for pts in IOL denominator for all exams > 0 between admit date
      ## and ROM or 4.5 hours after 4cm exam where amnio not offered
      cerv_iol_var_ct = case_when(
        iol_ebp_denom_flg == 1 & 
          cerv_exam_combined_dttm > admission_dt & 
          (cerv_exam_combined_dttm < rupt_mem_dt |
             time_first_4cm_exam_to_other_exams < 270) & 
          cervical_exam_dilation_no > 0 &
          amniotomy_offered_e == 0 ~ cerv_iol_var_ct + 1,
        TRUE ~ cerv_iol_var_ct
      ),
      ## Attending/CNM at the time of cervical exam offer
      ## needed for pts in IOL denominator for all exams > 0 between admit date
      ## and ROM or 4.5 hours after 4cm exam
      cerv_iol_var_ct = case_when(
        iol_ebp_denom_flg == 1 & 
          cerv_exam_combined_dttm > admission_dt & 
          (cerv_exam_combined_dttm < rupt_mem_dt |
             time_first_4cm_exam_to_other_exams < 270) & 
          cervical_exam_dilation_no > 0 ~ cerv_iol_var_ct + 1,
        TRUE ~ cerv_iol_var_ct
      ),
      # missing variables
      ## cervical dilation 
      cerv_exam_missing_flg = case_when(
        iol_ebp_denom_flg == 1 & 
          cerv_exam_combined_dttm > admission_dt & 
          (cerv_exam_combined_dttm < rupt_mem_dt |
             time_first_4cm_exam_to_other_exams < 270) & 
          cervical_exam_dilation_not_doc_b == 1 ~ 1, 
        TRUE ~ 0 
      ),
      ## cervical exam dttm/offer dttm
      cerv_dttm_missing_flg = case_when(
        mode_of_delivery_cd %in% c(1, 2, 3, 4) & 
          cervical_exam_dttm_ND == 1 & 
          cervical_exam_offer_dttm_ND == 1 ~ 1, 
        TRUE ~ 0
      ),
      ## Amniotomy offered with cervical exam
      amnio_offer_missing_flg = case_when(
        iol_ebp_denom_flg == 1 & 
          cerv_exam_combined_dttm > admission_dt & 
          (cerv_exam_combined_dttm < rupt_mem_dt |
             time_first_4cm_exam_to_other_exams < 270) & 
          cervical_exam_dilation_no > 0 & 
          amniotomy_offered_e == 99 ~ 1, 
        TRUE ~ 0
      ),
      ## Primary reason that amniotomy was not offered
      reason_amnio_not_offer_missing_flg = case_when(
        iol_ebp_denom_flg == 1 & 
          cerv_exam_combined_dttm > admission_dt & 
          (cerv_exam_combined_dttm < rupt_mem_dt |
             time_first_4cm_exam_to_other_exams < 270) & 
          cervical_exam_dilation_no > 0 &
          amniotomy_offered_e == 0 & 
          amniotomy_not_offered_reason_e == 9 ~ 1, 
        TRUE ~ 0
      ),
      ## Attending/CNM at the time of cervical exam offer
      attending_cerv_exam_missing_flg = case_when(
        iol_ebp_denom_flg == 1 & 
          cerv_exam_combined_dttm > admission_dt & 
          (cerv_exam_combined_dttm < rupt_mem_dt |
             time_first_4cm_exam_to_other_exams < 270) & 
          cervical_exam_dilation_no > 0 & 
          cervical_exam_attending_not_doc_b == 1 ~ 1, 
        TRUE ~ 0
      )
    ) |> 
    # group by patient and sum required and missing variables 
    group_by(patientid) |> 
    summarize(
      cerv_exam_missing_ct = sum(cerv_exam_missing_flg),
      cerv_dttm_missing_ct = sum(cerv_dttm_missing_flg),
      amnio_offer_missing_ct = sum(amnio_offer_missing_flg),
      reason_amnio_not_offer_missing_ct = sum(reason_amnio_not_offer_missing_flg),
      attending_cerv_exam_missing_ct = sum(attending_cerv_exam_missing_flg),
      cerv_iol_var_ct = sum(cerv_iol_var_ct)
    ) |> 
    mutate(
      cerv_iol_missing_ct = cerv_exam_missing_ct + 
        cerv_dttm_missing_ct +
        amnio_offer_missing_ct + 
        reason_amnio_not_offer_missing_ct +
        attending_cerv_exam_missing_ct
    ) |> 
    distinct(patientid, cerv_iol_missing_ct, cerv_iol_var_ct)
  
  iol_missingness_dt <- induction_data |> 
    # method offered must be chemical/mechanical
    filter(method_offered_e %in% c(1, 4, 5, 6, 9)) |> 
    right_join(
      obi_26_dt |> 
        # must be in the IOL denominator AND 
        # first method administered must be mechanical/chemical
        filter(
          iol_ebp_denom_flg == 1 & 
            (chem_first_iol_flg == 1 |
            mech_first_iol_flg == 1)
        ) |> 
        select(
          patientid,
          first_iol_agent_dttm,
        )
    ) |> 
    mutate(
      # calculate time from first agent to subsequent agents
      time_from_first_iol_agent = difftime(start_dttm, first_iol_agent_dttm, units = "mins"),
      # required variables - start at 1 because all mech/chem require a start OR 
      # offer time and add 1 as appropriate
      method_iol_var_ct = 1,
      ## Attending/CNM at time of intervention method offer    
      ## Result of intervention method offer
      ## both needed for pts in IOL denominator w/ mech/chem agent as first agent
      ## for all mech/chem agents w/in 4.5H
      method_iol_var_ct = case_when(
        time_from_first_iol_agent <= 270 ~ method_iol_var_ct + 2,
        TRUE ~ method_iol_var_ct
      ),
      # missing variables
      ## Intervention method offered timing OR initiation/attempt date/time
      method_offer_start_missing_flg = case_when(
        method_offered_e_dttm_not_doc_b == 1 &
          start_dttm_not_documented_b == 1 ~ 1, 
        TRUE ~ 0
      ), 
      ## Attending/CNM at time of intervention method offer    
      method_attending_missing_flg = case_when(
        time_from_first_iol_agent <= 270 & 
          attending_method_offer_e_not_doc_b == 1 ~ 1, 
        TRUE ~ 0
      ),
      ## Result of intervention method offer
      method_offer_result_missing_flg = case_when(
        time_from_first_iol_agent <= 270 &
          method_offered_result_e == 99 ~ 1,
        TRUE ~ 0 
      )
    ) |> 
    # group by patient and sum required and missing variables 
    group_by(patientid) |> 
    summarize(
      method_offer_result_missing_ct = sum(method_offer_result_missing_flg),
      method_offer_start_missing_ct = sum(method_offer_start_missing_flg),
      method_attending_missing_ct = sum(method_attending_missing_flg),
      method_iol_var_ct = sum(method_iol_var_ct)
    ) |> 
    mutate(
      method_iol_missing_ct = method_offer_result_missing_ct + 
        method_offer_start_missing_ct + 
        method_attending_missing_ct
    ) |> 
    distinct(patientid, method_iol_missing_ct, method_iol_var_ct)
  
  missingness_all_dt <- main_data |> 
    left_join(missingness_main_dt) |> 
    left_join(iol_missingness_dt) |> 
    left_join(cerv_missingness_dt) |> 
    mutate(
      cerv_iol_missing_ct = ifelse(is.na(cerv_iol_missing_ct), 0, cerv_iol_missing_ct),
      method_iol_missing_ct = ifelse(is.na(method_iol_missing_ct), 0, method_iol_missing_ct),
      cerv_iol_var_ct = ifelse(is.na(cerv_iol_var_ct), 0, cerv_iol_var_ct),
      method_iol_var_ct = ifelse(is.na(method_iol_var_ct), 0, method_iol_var_ct),
      iol_missing_ct = main_iol_missing_ct + cerv_iol_missing_ct + method_iol_missing_ct,
      iol_var_ct = main_iol_var_ct + cerv_iol_var_ct + method_iol_var_ct
    ) 
  
  if(group) {
    missingness_all_dt |> 
      summarize(
        total_missing = sum(iol_missing_ct), 
        total_var = sum(iol_var_ct),
        .by = c(...)
      ) |> 
      mutate(missing_rate = total_missing/total_var) |> 
      arrange(desc(missing_rate))
  } else {
    main_data |>
      left_join(missingness_all_dt)
  }
}
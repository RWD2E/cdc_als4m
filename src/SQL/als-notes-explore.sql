/*
# Copyright (c) 2021-2025 University of Missouri                   
# Author: Xing Song, xsm7f@umsystem.edu                            
# File: als-notes-explore.sql
*/

create schema if not exists SX_ALS_NOTES;

select * from als4m_db.notes.sample_potentially_deid_alsnotes limit 5;

select count(distinct patid) from als4m_db.notes.sample_potentially_deid_alsnotes;

select * from als4m_db.notes.sample_potentially_deid_alsnotes
where lower(deid_note) like '%national als registry%';

-- select count(distinct patid) from als4m_db.notes.sample_potentially_deid_alsnotes
-- where lower(deid_note) like '%national als registry%';
-- 12

select * from 
where patid = '255089'
order by obsgen_start_date
;

select * from als4m_db.notes.sample_potentially_deid_alsnotes
where lower(deid_note) like '%subject: general message%';


-- General Message
-- Medication Management
-- Nutrition Narrative
-- Nutrition Note
-- Clinic Triage Message
-- Research Note
-- Discharge Information
-- Neurology Clinic Note
-- Portal Message Documentation
-- Admission Note
-- Plan for Day-Care Coordination
-- Care Coordination Documentation
-- AM PAC Smart Template, Activity Measure for Post-Acute Care
-- Care Coordination Narrative
-- Discharge Summary
-- Hospital Course Documentation
-- Food & Nutrient Intake
-- Patient Depart Summary
-- Progress Note
-- AM PAC Mobility Goal Smart Template, Activity Measure for Post-Acute Care
-- Nursing Assessment Narrative
-- Report
-- Op/Procedure Note
-- PE, Pulmonary embolism?
-- AandP, Anatomy and Physiology Notes
-- HPI, History of Present Illness
-- ROS, Review of Systems
-- Preoperative Document
-- Clinic Nursing Note
-- Family Medicine Clinic Note
-- Genetics CH Clinic Note
-- Ambulatory Patient Depart Summary
-- EEG
-- Borderline EKG
-- Palliative/Supportive Care Clinic Note
-- Electrocardiogram - Adult
-- Letter to Patient
-- Speech Therapy Note
-- Physical Therapy Note
-- Occupational Therapy Note
-- PT Contact Note
-- Perioperative SurgiNet Note
-- Daily Living Notes
-- OT Assessment Comments
-- Hx of Present Illness/PMH
-- Patient Progressing/Regressing
-- PT Hx of Present Illness/PMH, past medical history
-- Treatment Intervention
-- Pt/Caregiver Subjective Comments- PT
-- MoCA Score Review, Montreal Cognitive Assessment
-- OT Narrative
-- Post-anesthesia Note
-- Referral Information
-- OT Contact Note
-- Utilization Review Documentation
-- Pre-anesthesia Note
-- Pharmacy Medication History Note
-- Neurosurgery Clinic Note
-- Phone Msg
-- Neurology Consult Note
-- Emergency Services Note
-- D-Opioid Use Disorder Risk Prediction
-- Urgent Care Clinic Note
-- Clinic Visit Narrative
-- Culture, Blood
-- Group A Beta Strep, Rapid
-- Culture, Respiratory
-- Culture, Beta Group Strep A
-- Patient Questionnaires
-- Palliative Care (ESAS) Outpt Form,  Edmonton Symptom Assessment Scale
-- Nutrition Glucose/Endocrine Profile
-- Pharmacy Consult Note
-- Patient Medical/Health History
-- Patient Instructions
-- EMG Report
-- Orthopaedic Clinic Note
-- Coding Summary
-- Results Notification to Patient
-- Daily Patient Summary
-- Daily Chief Complaint
-- General Med IM Consult Note
-- Plan for Next Session
-- Immediate Post Operative Note
-- CSA Clinic Note Archive,  Clinical Skills Assessment?
-- Letter to Provider
-- Phys Med and Rehab Clinic Note
-- Quick Care Clinic Note
-- Path Scanned Report
-- ED/UC Pat Edu
-- Dermatology Clinic Note
-- Patient Portal Add Home Meds
-- Oral Surgery Clinic Note
-- 6 Minute Walk Test Symptoms/Goals/Plan
-- Six Minute Walk Test/SpO2 Eval Form
-- Travel Clinic Note
-- Pulmonary Function Test
-- Ellis Fischel Nursing Narrative
-- I-PML Notes?
-- SPE Interpretation, Serum protein electrophoresis 
-- FANA Interpretation, fluorescent antinuclear antibody
-- Consents/Legal
-- Respiratory Therapy Narrative
-- Anthropometric Notes
-- Procedure Documentation
-- SLP Outpatient Progress/Discharge -Text
-- SLP Discharge Recommendations
-- SLP Outpatient Daily Doc - Text
-- Swallowing Additional Details
-- SLP Subjective
-- History of Swallow Studies
-- History of Problems, Comorbidities Rehab
-- Team Communication
-- Clinical Assessment
-- SLP Modified Barium Swallow Study-Text
-- SLP Assessment Comments
-- Therapeutic Swallow Exercises Details
-- Expression Additional Details
-- Speech Production/Voice Details
-- Outpatient SLP Certification Letter-Text
-- Restricted Health Psychology Clinic Note
-- Education Narrative
-- Impression of Swallow Narrative
-- SLP Results Comments
-- Cognitive Behavioral Evaluation
-- Penetration Aspiration Scale Reference, PAS
-- SLP History of Present Illness-PMH, Positive Mental Health Scale? 

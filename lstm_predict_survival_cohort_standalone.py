#!/usr/bin/env python3
"""
lstm_predict_survival_cohort_standalone.py
===========================================
Generate LSTM predictions for survival cohort WITHOUT sequence_generator dependency

This is a standalone version that processes data directly.
"""

import os
import pandas as pd
import numpy as np
import torch
import pickle
from datetime import datetime
import warnings
warnings.filterwarnings('ignore')

from model import ImprovedLSTM
import snowflake.connector


# Snowflake configuration
SNOWFLAKE_CONFIG = {
    'user': '',
    'password': '',
    'account': '',
    'warehouse':'' ,
    'database': '',
    'role': '',
    'schema': ''

}

TABLE_NAME = ''


def load_best_model():
    """Load the most recent trained LSTM model"""
    print("\n Loading trained LSTM model...")
    
    import glob
    model_files = glob.glob('models/best_lstm_model_*.ckpt')
    
    if not model_files:
        print("   No trained model found!")
        print("  Please run main.py first to train the model")
        return None
    
    # Use most recent model
    model_file = max(model_files, key=os.path.getctime)
    print(f"  Loading: {model_file}")
    
    try:
        model = ImprovedLSTM.load_from_checkpoint(model_file)
        model.eval()
        print(f"   Model loaded successfully")
        return model
    except Exception as e:
        print(f"   Error loading model: {e}")
        import traceback
        traceback.print_exc()
        return None


def get_survival_cohort():
    """Get patients with survival data from Snowflake"""
    print("\n Loading survival cohort from Snowflake...")
    
    try:
        conn = snowflake.connector.connect(**SNOWFLAKE_CONFIG)
        print("   Connected to Snowflake")
        
        cursor = conn.cursor()
        
        # Query for patients with survival data
        # FIXED: Calculate months from LAST observation to death
        query = f"""
        SELECT 
            PATID,
            MAX(DIED) as died,
            MAX(MONTHS_TO_DEATH) as months_from_diagnosis_to_death,
            MAX(MONTHS_SINCE_DIAGNOSIS) as months_at_last_observation,
            MAX(MONTHS_TO_DEATH) - MAX(MONTHS_SINCE_DIAGNOSIS) as months_to_death,
            COUNT(*) as n_windows,
            MIN(CASE WHEN WINDOW_INDEX = 1 THEN CURRENT_BMI END) as baseline_bmi,
            MIN(AGE_AT_DIAGNOSIS) as age_at_diagnosis
        FROM {TABLE_NAME}
        WHERE MONTHS_TO_DEATH IS NOT NULL
          AND MONTHS_TO_DEATH > 0
          AND CURRENT_BMI IS NOT NULL
          AND CURRENT_BMI BETWEEN 15 AND 70
        GROUP BY PATID
        HAVING COUNT(*) >= 2
          AND MAX(MONTHS_TO_DEATH) - MAX(MONTHS_SINCE_DIAGNOSIS) > 0
        """
        
        print(f"  Querying survival cohort...")
        cursor.execute(query)
        results = cursor.fetchall()
        columns = [desc[0].lower() for desc in cursor.description]
        
        df = pd.DataFrame(results, columns=columns)
        
        cursor.close()
        conn.close()
        
        # Convert to float64 to avoid Decimal issues
        numeric_cols = ['died', 'months_from_diagnosis_to_death', 'months_at_last_observation', 
                       'months_to_death', 'n_windows', 'baseline_bmi', 'age_at_diagnosis']
        for col in numeric_cols:
            if col in df.columns:
                df[col] = pd.to_numeric(df[col], errors='coerce').astype('float64')
        
        # Remove any with negative or zero follow-up time
        df = df[df['months_to_death'] > 0].copy()
        
        print(f"   Found {len(df)} patients with survival data")
        print(f"    Deaths: {df['died'].sum():.0f} ({df['died'].mean()*100:.1f}%)")
        print(f"    Mean survival FROM LAST OBSERVATION: {df['months_to_death'].mean():.1f} months")
        print(f"    Median survival: {df['months_to_death'].median():.1f} months")
        print(f"    Range: {df['months_to_death'].min():.1f} to {df['months_to_death'].max():.1f} months")
        
        return df
        
    except Exception as e:
        print(f"   Error: {e}")
        import traceback
        traceback.print_exc()
        return None


def load_patient_data_with_features(patient_list):
    """Load complete temporal data with all features for specific patients"""
    print(f"\n Loading temporal data for {len(patient_list)} patients...")
    
    try:
        conn = snowflake.connector.connect(**SNOWFLAKE_CONFIG)
        cursor = conn.cursor()
        
        # Create patient list for SQL
        patid_list = "', '".join([str(p) for p in patient_list])
        
        query = f"""
        SELECT *
        FROM {TABLE_NAME}
        WHERE PATID IN ('{patid_list}')
        ORDER BY PATID, WINDOW_INDEX
        """
        
        print(f"  Executing query...")
        cursor.execute(query)
        results = cursor.fetchall()
        columns = [desc[0] for desc in cursor.description]
        
        df = pd.DataFrame(results, columns=columns)
        
        cursor.close()
        conn.close()
        
        print(f"   Loaded {len(df)} records")
        print(f"    Patients: {df['PATID'].nunique()}")
        
        return df
        
    except Exception as e:
        print(f"   Error: {e}")
        import traceback
        traceback.print_exc()
        return None


def simple_preprocess(df):
    """Simple preprocessing without using data_preprocessing module"""
    print("\n Simple preprocessing...")
    
    # Convert numeric columns
    numeric_cols = ['CURRENT_BMI', 'ROLLING_PERCENTAGE_CHANGE', 'AGE_AT_DIAGNOSIS']
    for col in numeric_cols:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')
    
    # Basic cleaning
    df = df[
        (df['CURRENT_BMI'] >= 15) & 
        (df['CURRENT_BMI'] <= 65) &
        (df['AGE_AT_DIAGNOSIS'] >= 15) &
        (df['AGE_AT_DIAGNOSIS'] <= 90)
    ].copy()
    
    # Remove extreme BMI changes
    if 'ROLLING_PERCENTAGE_CHANGE' in df.columns:
        df = df[
            (df['ROLLING_PERCENTAGE_CHANGE'] >= -40) &
            (df['ROLLING_PERCENTAGE_CHANGE'] <= 40)
        ].copy()
    
    print(f"   Cleaned data: {len(df)} records")
    
    return df


def extract_features_simple(df, model):
    """
    Extract features in a simple way that matches model's expected input
    Uses only the most important features that are commonly available
    """
    print("\n Extracting features...")
    
    # Get model's expected input size from hyperparameters
    input_size = model.hparams.input_size
    print(f"  Model expects {input_size} features")
    
    # Define feature columns (prioritize most important)
    feature_candidates = []
    
    # Always include these if available
    always_include = ['AGE_AT_DIAGNOSIS', 'CURRENT_BMI', 'WINDOW_INDEX']
    
    for col in always_include:
        if col in df.columns:
            feature_candidates.append(col)
    
    # Add demographic features
    for col in df.columns:
        if col.startswith(('SEX_', 'RACE_', 'HISPANIC_')):
            feature_candidates.append(col)
    
    # Add BMI-related features
    for col in df.columns:
        if 'BMI' in col and col not in feature_candidates:
            if col not in ['NEXT_BMI', 'BMI_PCT_CHANGE']:
                feature_candidates.append(col)
    
    # Add lab values
    for col in df.columns:
        if col.startswith('LAB_'):
            feature_candidates.append(col)
    
    # Add medications
    for col in df.columns:
        if col.startswith('ING_'):
            feature_candidates.append(col)
    
    # Add diagnoses
    for col in df.columns:
        if col.startswith('PHECD_'):
            feature_candidates.append(col)
    
    # Remove exclusions
    exclude = ['PATID', 'NEXT_BMI', 'BMI_PCT_CHANGE', 'ROLLING_PERCENTAGE_CHANGE',
               'DIED', 'DEATH_DATE', 'MONTHS_TO_DEATH', 'ALS_DIAGNOSIS_DATE']
    
    feature_candidates = [f for f in feature_candidates if f not in exclude]
    
    # Take only the number we need
    selected_features = feature_candidates[:input_size]
    
    # If we don't have enough, pad with zeros
    if len(selected_features) < input_size:
        print(f"   Only found {len(selected_features)} features, will pad to {input_size}")
    
    print(f"   Selected {len(selected_features)} features")
    
    return selected_features


def create_patient_level_data(df, selected_features):
    """Create one sample per patient using their last window"""
    print("\n Creating patient-level data...")
    
    # For each patient, use their LAST window (most recent)
    patient_data = []
    
    for patid in df['PATID'].unique():
        patient_df = df[df['PATID'] == patid].sort_values('WINDOW_INDEX')
        
        if len(patient_df) < 1:
            continue
        
        # Use last window
        last_window = patient_df.iloc[-1]
        
        # Extract features
        features = []
        for feat in selected_features:
            if feat in last_window.index:
                val = last_window[feat]
                if pd.isna(val):
                    val = 0.0
                features.append(float(val))
            else:
                features.append(0.0)
        
        # Pad if needed
        while len(features) < len(selected_features):
            features.append(0.0)
        
        # Get target if available
        target = last_window.get('ROLLING_PERCENTAGE_CHANGE', np.nan)
        
        patient_data.append({
            'patid': patid,
            'features': features,
            'target': target
        })
    
    print(f"   Created data for {len(patient_data)} patients")
    
    return patient_data


def generate_predictions(model, patient_data, input_size):
    """Generate predictions for all patients"""
    print("\n Generating predictions...")
    
    device = 'cuda' if torch.cuda.is_available() else 'cpu'
    model = model.to(device)
    model.eval()
    
    predictions = []
    actuals = []
    patient_ids = []
    
    with torch.no_grad():
        for data in patient_data:
            # Get features and pad/truncate to correct size
            features = data['features'][:input_size]
            while len(features) < input_size:
                features.append(0.0)
            
            # Reshape to (1, 1, input_size) - batch_size=1, seq_len=1, features
            x = torch.FloatTensor([features]).unsqueeze(0).to(device)
            
            # Predict
            pred = model(x)
            
            predictions.append(pred.item())
            actuals.append(data['target'])
            patient_ids.append(data['patid'])
    
    print(f"   Generated {len(predictions)} predictions")
    
    # Create DataFrame
    pred_df = pd.DataFrame({
        'patid': patient_ids,
        'lstm_predicted_bmi_change': predictions,
        'actual_bmi_change': actuals
    })
    
    return pred_df


def merge_predictions_with_survival(pred_df, survival_df):
    """Merge predictions with survival outcomes"""
    print("\n Merging predictions with survival data...")
    
    # Ensure consistent PATID format
    pred_df['patid'] = pred_df['patid'].astype(str)
    survival_df['patid'] = survival_df['patid'].astype(str)
    
    # Merge
    merged = survival_df.merge(pred_df, on='patid', how='inner')
    
    print(f"  Survival cohort: {len(survival_df)} patients")
    print(f"  Predictions: {len(pred_df)} patients")
    print(f"  Merged: {len(merged)} patients")
    print(f"  Match rate: {len(merged)/len(survival_df)*100:.1f}%")
    
    if len(merged) > 0:
        print(f"\n   Successfully merged!")
        print(f"    Mean LSTM predicted: {merged['lstm_predicted_bmi_change'].mean():.2f}%")
        if merged['actual_bmi_change'].notna().sum() > 0:
            print(f"    Mean actual: {merged['actual_bmi_change'].mean():.2f}%")
        print(f"    Deaths: {merged['died'].sum():.0f}")
    
    return merged


def save_predictions(merged_df, output_dir='results'):
    """Save predictions for survival analysis"""
    print("\n Saving predictions for survival analysis...")
    
    os.makedirs(output_dir, exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    # Save as pickle
    pkl_file = os.path.join(output_dir, f'lstm_survival_predictions_{timestamp}.pkl')
    with open(pkl_file, 'wb') as f:
        pickle.dump({
            'predictions': merged_df['lstm_predicted_bmi_change'].values,
            'actual': merged_df['actual_bmi_change'].values,
            'patient_ids': merged_df['patid'].tolist(),
            'died': merged_df['died'].values,
            'months_to_death': merged_df['months_to_death'].values,
            'baseline_bmi': merged_df['baseline_bmi'].values if 'baseline_bmi' in merged_df.columns else None
        }, f)
    
    print(f"   Saved pickle: {pkl_file}")
    
    # Save as CSV
    csv_file = os.path.join(output_dir, f'lstm_survival_predictions_{timestamp}.csv')
    merged_df.to_csv(csv_file, index=False)
    
    print(f"   Saved CSV: {csv_file}")
    
    return pkl_file, csv_file


def main():
    """Main execution pipeline"""
    
    print("="*70)
    print("GENERATE LSTM PREDICTIONS FOR SURVIVAL COHORT (STANDALONE)")
    print("="*70)
    
    # Step 1: Load trained model
    model = load_best_model()
    if model is None:
        return
    
    # Get model's input size
    input_size = model.hparams.input_size
    print(f"\n Model expects {input_size} input features")
    
    # Step 2: Get survival cohort from Snowflake
    survival_df = get_survival_cohort()
    if survival_df is None or len(survival_df) == 0:
        print("\n No survival cohort found")
        return
    
    # Step 3: Load full temporal data for these patients
    patient_list = survival_df['patid'].tolist()
    df_full = load_patient_data_with_features(patient_list)
    if df_full is None or len(df_full) == 0:
        print("\n Could not load patient data")
        return
    
    # Step 4: Simple preprocessing
    df_clean = simple_preprocess(df_full)
    if len(df_clean) == 0:
        print("\n No data after preprocessing")
        return
    
    # Step 5: Extract features
    selected_features = extract_features_simple(df_clean, model)
    
    # Step 6: Create patient-level data
    patient_data = create_patient_level_data(df_clean, selected_features)
    
    # Step 7: Generate predictions
    pred_df = generate_predictions(model, patient_data, input_size)
    
    # Step 8: Merge with survival data
    merged_df = merge_predictions_with_survival(pred_df, survival_df)
    
    if len(merged_df) == 0:
        print("\n No patients matched between predictions and survival data")
        return
    
    # Step 9: Save predictions
    pkl_file, csv_file = save_predictions(merged_df)
    
    # Summary
    print("\n" + "="*70)
    print(" PREDICTION GENERATION COMPLETE")
    print("="*70)
    print(f"Total patients with predictions: {len(merged_df)}")
    print(f"Deaths: {merged_df['died'].sum():.0f} ({merged_df['died'].mean()*100:.1f}%)")
    print(f"Mean survival time: {merged_df['months_to_death'].mean():.1f} months")
    print(f"\nPrediction statistics:")
    print(f"  Mean LSTM predicted BMI change: {merged_df['lstm_predicted_bmi_change'].mean():.2f}%")
    print(f"  Std LSTM predicted: {merged_df['lstm_predicted_bmi_change'].std():.2f}%")
    print(f"  Range: [{merged_df['lstm_predicted_bmi_change'].min():.2f}%, {merged_df['lstm_predicted_bmi_change'].max():.2f}%]")
    print(f"\nFiles saved:")
    print(f"  Pickle: {pkl_file}")
    print(f"  CSV: {csv_file}")
    print("\n Next step: Run survival analysis:")
    print("="*70)
    
    return merged_df


if __name__ == "__main__":
    try:
        result = main()
    except KeyboardInterrupt:
        print("\n\n Cancelled by user")
    except Exception as e:
        print(f"\n ERROR: {e}")
        import traceback
        traceback.print_exc()
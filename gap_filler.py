"""
gap_filler.py
=============
Fill missing windows using linear interpolation
Call this right after loading data, before preprocessing
"""

import pandas as pd
import numpy as np


def fill_missing_windows(df, patient_id_col='PATID', window_col='WINDOW_INDEX', verbose=True):
    """
    Fill missing windows with linear interpolation
    
    Args:
        df: Raw dataframe from data_loader
        patient_id_col: Patient ID column name
        window_col: Window column name
        verbose: Print progress
        
    Returns:
        df: DataFrame with gaps filled
    """
    if verbose:
        print("FILLING MISSING WINDOWS")
        print("="*60)
    
    initial_rows = len(df)
    patients_with_gaps = 0
    windows_added = 0
    
    # Identify columns to interpolate vs copy
    # Demographics - just copy (don't interpolate)
    demo_cols = [col for col in df.columns if any(x in col for x in 
                 ['SEX_', 'RACE_', 'HISPANIC_', 'ALS_DIAGNOSIS_DATE'])]
    
    # Target - don't interpolate (will be calculated later)
    target_cols = [col for col in df.columns if any(x in col for x in 
                   ['ROLLING_PERCENTAGE_CHANGE', 'NEXT_BMI', 'BMI_PCT_CHANGE'])]
    
    # All other numeric columns - interpolate
    numeric_cols = df.select_dtypes(include=[np.number]).columns.tolist()
    interpolate_cols = [col for col in numeric_cols 
                       if col not in [patient_id_col, window_col] 
                       and col not in demo_cols 
                       and col not in target_cols]
    
    if verbose:
        print(f"Columns to interpolate: {len(interpolate_cols)}")
        print(f"Columns to copy: {len(demo_cols)}")
    
    # Store new rows
    new_rows = []
    
    # Process each patient
    for patient_id in df[patient_id_col].unique():
        patient_data = df[df[patient_id_col] == patient_id].sort_values(window_col)
        windows = sorted(patient_data[window_col].unique())
        
        # Check for gaps
        min_window = windows[0]
        max_window = windows[-1]
        expected_windows = list(range(min_window, max_window + 1))
        missing_windows = [w for w in expected_windows if w not in windows]
        
        if not missing_windows:
            continue  # No gaps
        
        patients_with_gaps += 1
        
        # Fill each missing window
        for missing_w in missing_windows:
            # Find surrounding windows
            lower_windows = [w for w in windows if w < missing_w]
            upper_windows = [w for w in windows if w > missing_w]
            
            if not lower_windows or not upper_windows:
                continue  # Can't interpolate at edges
            
            lower_w = max(lower_windows)
            upper_w = min(upper_windows)
            
            # Get data for surrounding windows
            lower_data = patient_data[patient_data[window_col] == lower_w].iloc[0]
            upper_data = patient_data[patient_data[window_col] == upper_w].iloc[0]
            
            # Create new row
            new_row = {}
            
            # Copy patient ID and set window
            new_row[patient_id_col] = patient_id
            new_row[window_col] = missing_w
            
            # Copy demographics (don't interpolate)
            for col in demo_cols:
                if col in df.columns:
                    new_row[col] = lower_data[col]
            
            # Interpolate numeric features
            for col in interpolate_cols:
                if col in df.columns:
                    lower_val = pd.to_numeric(lower_data[col], errors='coerce')
                    upper_val = pd.to_numeric(upper_data[col], errors='coerce')
                    
                    if pd.notna(lower_val) and pd.notna(upper_val):
                        # Linear interpolation
                        new_row[col] = (lower_val + upper_val) / 2.0
                    elif pd.notna(lower_val):
                        new_row[col] = lower_val  # Forward fill if upper missing
                    elif pd.notna(upper_val):
                        new_row[col] = upper_val  # Backward fill if lower missing
                    else:
                        new_row[col] = np.nan
            
            # Set target columns to NaN (will be recalculated)
            for col in target_cols:
                if col in df.columns:
                    new_row[col] = np.nan
            
            # Copy any remaining columns
            for col in df.columns:
                if col not in new_row:
                    new_row[col] = lower_data[col]
            
            new_rows.append(new_row)
            windows_added += 1
    
    # Add new rows to dataframe
    if new_rows:
        df_filled = pd.concat([df, pd.DataFrame(new_rows)], ignore_index=True)
        df_filled = df_filled.sort_values([patient_id_col, window_col]).reset_index(drop=True)
    else:
        df_filled = df.copy()
    
    if verbose:
        print(f"Gap filling complete:")
        print(f"   Patients with gaps:    {patients_with_gaps}")
        print(f"   Windows added:         {windows_added}")
        print(f"   Original rows:         {initial_rows:,}")
        print(f"   Final rows:            {len(df_filled):,}")
        print(f"   New rows:              {len(df_filled) - initial_rows:,}")
        print("="*60)
    
    return df_filled


# Quick test function
def test_gap_filling():
    """Test with sample data"""
    # Create sample patient with gap
    test_data = {
        'PATID': ['P1', 'P1', 'P1'],
        'WINDOW_INDEX': [1, 3, 5],
        'CURRENT_BMI': [25.0, 23.0, 21.0],
        'AGE_AT_DIAGNOSIS': [60, 60, 60],
        'SEX_M': [1, 1, 1]
    }
    
    df_test = pd.DataFrame(test_data)
    print("BEFORE:")
    print(df_test)
    
    df_filled = fill_missing_windows(df_test)
    print("\nAFTER:")
    print(df_filled[['PATID', 'WINDOW_INDEX', 'CURRENT_BMI', 'AGE_AT_DIAGNOSIS']])


if __name__ == "__main__":
    test_gap_filling()
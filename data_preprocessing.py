"""
data_preprocessing.py - Clean and prepare data
==============================================
"""

import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.feature_selection import SelectKBest, f_regression

from config import (
    PATIENT_ID_COLUMN, TARGET_COLUMN, EXCLUDE_FEATURES,
    USE_FEATURE_SELECTION, MIN_FEATURES, MAX_FEATURES,
    TRAIN_SIZE, VAL_SIZE, VERBOSE
)


def clean_data(df):
    """Basic data cleaning"""
    if VERBOSE:
        print("\n CLEANING DATA")
        print("="*60)
    
    initial_rows = len(df)
    
    # Remove missing target
    df_clean = df[df[TARGET_COLUMN].notna()].copy()
    
    # Remove extreme outliers (already done in SQL, but double-check)
    df_clean = df_clean[
        (df_clean[TARGET_COLUMN] >= -50) & 
        (df_clean[TARGET_COLUMN] <= 50)
    ]
    
    if VERBOSE:
        print(f"Rows: {initial_rows:,} → {len(df_clean):,}")
        print(f"Patients: {df_clean[PATIENT_ID_COLUMN].nunique():,}")
    
    return df_clean


def create_feature_matrix(df):
    """Extract features and target"""
    if VERBOSE:
        print("\n CREATING FEATURE MATRIX")
        print("="*60)
    
    # Get feature columns (exclude IDs, target, etc.)
    feature_cols = [col for col in df.columns if col not in EXCLUDE_FEATURES]
    
    # Separate features and target
    X = df[feature_cols].copy()
    y = df[TARGET_COLUMN].copy()
    patient_ids = df[PATIENT_ID_COLUMN].copy()
    
    # Convert to numeric
    for col in X.columns:
        X[col] = pd.to_numeric(X[col], errors='coerce')
    
    # Fill missing values
    X = X.fillna(X.median())
    
    if VERBOSE:
        print(f"Features: {X.shape[1]}")
        print(f"Samples: {len(X):,}")
        print(f"Target range: [{y.min():.2f}, {y.max():.2f}]")
    
    return X, y, patient_ids


def select_features(X, y, max_features=MAX_FEATURES):
    """Improved feature selection"""
    if not USE_FEATURE_SELECTION or X.shape[1] <= max_features:
        return X.columns.tolist()
    
    if VERBOSE:
        print("\n FEATURE SELECTION")
        print("="*60)
    
    # Remove features with zero variance
    from sklearn.feature_selection import VarianceThreshold
    selector = VarianceThreshold(threshold=0.01)
    selector.fit(X)
    X_var = X.loc[:, selector.get_support()]
    
    if VERBOSE:
        print(f"After variance threshold: {X_var.shape[1]} features")
    
    # Use SelectKBest with more features
    from sklearn.feature_selection import SelectKBest, f_regression
    
    k = min(max_features, X_var.shape[1])
    selector = SelectKBest(score_func=f_regression, k=k)
    selector.fit(X_var, y)
    
    selected_features = X_var.columns[selector.get_support()].tolist()
    
    # Get feature scores
    scores = selector.scores_
    feature_scores = list(zip(X_var.columns, scores))
    feature_scores.sort(key=lambda x: x[1], reverse=True)
    
    if VERBOSE:
        print(f"Selected: {len(selected_features)} features")
        print(f"\nTop 15 features by score:")
        for i, (feat, score) in enumerate(feature_scores[:15], 1):
            print(f"  {i:2d}. {feat:40s} (score: {score:,.2f})")
    
    return selected_features

def engineer_features(X):
    """Create additional engineered features"""
    if VERBOSE:
        print("\n ENGINEERING FEATURES")
        print("="*60)
    
    X_eng = X.copy()
    
    # BMI-related interactions
    if 'CURRENT_BMI' in X_eng.columns and 'AGE_AT_DIAGNOSIS' in X_eng.columns:
        X_eng['BMI_AGE_INTERACTION'] = X_eng['CURRENT_BMI'] * X_eng['AGE_AT_DIAGNOSIS']
    
    # Squared features for important variables
    bmi_cols = [col for col in X_eng.columns if 'BMI' in col.upper()]
    for col in bmi_cols[:3]:  # Top 3 BMI features
        X_eng[f'{col}_SQUARED'] = X_eng[col] ** 2
    
    # Log transform for skewed features (prevent log(0))
    if 'WINDOW_INDEX' in X_eng.columns:
        X_eng['LOG_WINDOW'] = np.log1p(X_eng['WINDOW_INDEX'])
    
    if VERBOSE:
        print(f"Features after engineering: {X_eng.shape[1]} (added {X_eng.shape[1] - X.shape[1]})")
    
    return X_eng

def create_patient_splits(patient_ids, train_size=TRAIN_SIZE, val_size=VAL_SIZE):
    """Split patients into train/val/test (prevent data leakage)"""
    if VERBOSE:
        print("\n CREATING PATIENT SPLITS")
        print("="*60)
    
    unique_patients = patient_ids.unique()
    n_patients = len(unique_patients)
    
    # Shuffle patients
    np.random.seed(42)
    shuffled_patients = np.random.permutation(unique_patients)
    
    # Split
    n_train = int(n_patients * train_size)
    n_val = int(n_patients * val_size)
    
    train_patients = set(shuffled_patients[:n_train])
    val_patients = set(shuffled_patients[n_train:n_train+n_val])
    test_patients = set(shuffled_patients[n_train+n_val:])
    
    # Create masks
    train_mask = patient_ids.isin(train_patients)
    val_mask = patient_ids.isin(val_patients)
    test_mask = patient_ids.isin(test_patients)
    
    if VERBOSE:
        print(f"Train: {len(train_patients):,} patients ({train_mask.sum():,} samples)")
        print(f"Val:   {len(val_patients):,} patients ({val_mask.sum():,} samples)")
        print(f"Test:  {len(test_patients):,} patients ({test_mask.sum():,} samples)")
    
    return train_mask, val_mask, test_mask


def normalize_features(X_train, X_val, X_test):
    """Normalize features using training statistics"""
    if VERBOSE:
        print("\n NORMALIZING FEATURES")
        print("="*60)
    
    scaler = StandardScaler()
    
    X_train_scaled = scaler.fit_transform(X_train)
    X_val_scaled = scaler.transform(X_val)
    X_test_scaled = scaler.transform(X_test)
    
    if VERBOSE:
        print(f" Features normalized")
    
    return X_train_scaled, X_val_scaled, X_test_scaled, scaler


def preprocess_pipeline(df):
    """Complete preprocessing pipeline"""
    if VERBOSE:
        print("\n" + "="*70)
        print("PREPROCESSING PIPELINE")
        print("="*70)
    
    # 1. Clean
    df_clean = clean_data(df)
    
    # 2. Create feature matrix
    X, y, patient_ids = create_feature_matrix(df_clean)

    X = engineer_features(X)
    
    # 3. Feature selection
    selected_features = select_features(X, y)
    X_selected = X[selected_features]
    
    # 4. Split by patient
    train_mask, val_mask, test_mask = create_patient_splits(patient_ids)
    
    X_train = X_selected[train_mask]
    X_val = X_selected[val_mask]
    X_test = X_selected[test_mask]
    
    y_train = y[train_mask].values
    y_val = y[val_mask].values
    y_test = y[test_mask].values
    
    # 5. Normalize
    X_train_norm, X_val_norm, X_test_norm, scaler = normalize_features(
        X_train, X_val, X_test
    )
    
    if VERBOSE:
        print("\n PREPROCESSING COMPLETE")
        print("="*70)
    
    return {
        'X_train': X_train_norm,
        'X_val': X_val_norm,
        'X_test': X_test_norm,
        'y_train': y_train,
        'y_val': y_val,
        'y_test': y_test,
        'feature_names': selected_features,
        'scaler': scaler
    }


if __name__ == "__main__":
    from data_loader import load_data
    df = load_data()
    data = preprocess_pipeline(df)
    print(f"\n Preprocessing test passed!")
    print(f"   Train: {data['X_train'].shape}")
    print(f"   Val:   {data['X_val'].shape}")
    print(f"   Test:  {data['X_test'].shape}")
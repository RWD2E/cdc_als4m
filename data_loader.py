"""
data_loader.py - Load and JOIN all feature tables
=================================================
"""

import pandas as pd
import snowflake.connector
from config import SNOWFLAKE_CONFIG, PATIENT_ID_COLUMN, VERBOSE, BASE_TABLE, PHECD_TABLE, INGREDIENTS_TABLE, LAB_TABLE

def load_data():
    """Load data from Snowflake - JOIN all feature tables"""
    if VERBOSE:
        print("\n LOADING DATA FROM SNOWFLAKE (JOINING ALL TABLES)")
        print("="*60)
    
    try:
        # Connect
        conn = snowflake.connector.connect(**SNOWFLAKE_CONFIG)
        cursor = conn.cursor()
        
        if VERBOSE:
            print("Connected to Snowflake")
        
        # JOIN all tables with SELECT *
        query = f"""
        SELECT 
            base.*,
            phe.* EXCLUDE (PATID, WINDOW_INDEX),
            ing.* EXCLUDE (PATID, WINDOW_INDEX),
            lab.* EXCLUDE (PATID, WINDOW_INDEX)
        FROM {BASE_TABLE} base
        
        LEFT JOIN {PHECD_TABLE} phe
            ON base.PATID = phe.PATID 
            AND base.WINDOW_INDEX = phe.WINDOW_INDEX
        
        LEFT JOIN {INGREDIENTS_TABLE} ing
            ON base.PATID = ing.PATID 
            AND base.WINDOW_INDEX = ing.WINDOW_INDEX
        
        LEFT JOIN {LAB_TABLE} lab
            ON base.PATID = lab.PATID 
            AND base.WINDOW_INDEX = lab.WINDOW_INDEX
        
        ORDER BY base.PATID, base.WINDOW_INDEX
        """
        
        if VERBOSE:
            print("\nExecuting JOIN query...")
        
        cursor.execute(query)
        
        # Fetch results
        if VERBOSE:
            print("Fetching results...")
        
        results = cursor.fetchall()
        columns = [desc[0] for desc in cursor.description]
        
        # Create DataFrame
        df = pd.DataFrame(results, columns=columns)
        
        cursor.close()
        conn.close()
        
        # Convert types
        if VERBOSE:
            print("Converting data types...")
        
        for col in df.columns:
            if df[col].dtype == 'object':
                # Try numeric first
                try:
                    df[col] = pd.to_numeric(df[col])
                except:
                    # Try datetime
                    try:
                        df[col] = pd.to_datetime(df[col])
                    except:
                        # Keep as string
                        df[col] = df[col].astype(str)
        
        if VERBOSE:
            print(f"\n LOADED SUCCESSFULLY!")
            print("="*60)
            print(f"Total rows:    {len(df):,}")
            print(f"Total columns: {len(df.columns):,}")
            print(f"Patients:      {df[PATIENT_ID_COLUMN].nunique():,}")
            
            # Count feature types
            phecd_cols = [c for c in df.columns if c.startswith('PHECD_')]
            ing_cols = [c for c in df.columns if c.startswith('ING_')]
            lab_cols = [c for c in df.columns if c.startswith('LAB_')]
            base_cols = len(df.columns) - len(phecd_cols) - len(ing_cols) - len(lab_cols)
            
            print(f"\nFeature breakdown:")
            print(f"  Base features:         {base_cols}")
            print(f"  Diagnosis (PHECD_*):   {len(phecd_cols)}")
            print(f"  Medications (ING_*):   {len(ing_cols)}")
            print(f"  Labs (LAB_*):          {len(lab_cols)}")
            print(f"  TOTAL FEATURES:        {len(df.columns)}")
            
            if 'ALS_DIAGNOSIS_DATE' in df.columns:
                print(f"\nDate range: {df['ALS_DIAGNOSIS_DATE'].min()} to {df['ALS_DIAGNOSIS_DATE'].max()}")
            
            print("="*60)
        
        return df
        
    except Exception as e:
        print(f" Error loading data: {e}")
        import traceback
        traceback.print_exc()
        raise


if __name__ == "__main__":
    df = load_data()
    print("\n Data loader test passed!")
    print(f"\nSample columns (first 20):")
    for i, col in enumerate(df.columns[:20], 1):
        print(f"  {i:2d}. {col}")
    
    print(f"\nSample data (first row, first 10 columns):")
    print(df.iloc[0, :10])
"""
config.py - Configuration for LSTM with Full Features
====================================================
"""

import os
from datetime import datetime

# ============================================================================
# SNOWFLAKE CONNECTION
# ============================================================================
SNOWFLAKE_CONFIG = {
    'user': '',
    'password': '',
    'account': '',
    'warehouse': '',
    'database': '',
    'role': '',
    'schema': ''
}
# ============================================================================
# SNOWFLAKE TABLES
# ============================================================================
BASE_TABLE = ''
PHECD_TABLE = ''
INGREDIENTS_TABLE = ''
LAB_TABLE = ''

SNOWFLAKE_TABLE = ''  # Base table (will join others)

# ============================================================================
# DATA PARAMETERS
# ============================================================================
PATIENT_ID_COLUMN = 'PATID'
TARGET_COLUMN = 'ROLLING_PERCENTAGE_CHANGE'

# Data splits
TRAIN_SIZE = 0.70
VAL_SIZE = 0.15
TEST_SIZE = 0.15

# Feature selection (NOW WE HAVE 900+ FEATURES!)
USE_FEATURE_SELECTION = True
MIN_FEATURES = 15      # Increased from 15
MAX_FEATURES = 20     # Increased from 50

# Exclude columns (keep minimal - we want most features!)
EXCLUDE_FEATURES = [
    'PATID', 
    'WINDOW_INDEX',
    'IS_INTERPOLATED',
    'ALS_DIAGNOSIS_DATE',
    'DEATH_DATE',
    'ROLLING_PERCENTAGE_CHANGE',  # This is our target
    'ABSOLUTE_CHANGE',
    'PREVIOUS_WINDOW_BMI',
    'DIED',                        # Survival outcome (keep for later)
    'MONTHS_TO_DEATH',            # Survival outcome (keep for later)
    'PATIENT_TOTAL_WINDOWS'
]

# ============================================================================
# MODEL HYPERPARAMETERS
# ============================================================================
HIDDEN_SIZE = 128      # Keep at 128
NUM_LAYERS = 2
DROPOUT = 0.5
LEARNING_RATE = 0.001

# ============================================================================
# TRAINING PARAMETERS
# ============================================================================
BATCH_SIZE = 32
MAX_EPOCHS = 200
EARLY_STOPPING_PATIENCE = 20
GRADIENT_CLIP_VAL = 1.0

USE_GPU = True
NUM_WORKERS = 0
RANDOM_SEED = 42

# ============================================================================
# PATHS
# ============================================================================
PROJECT_ROOT = os.path.dirname(os.path.abspath(__file__))
RESULTS_DIR = os.path.join(PROJECT_ROOT, 'results')
MODELS_DIR = os.path.join(PROJECT_ROOT, 'models')
LOGS_DIR = os.path.join(PROJECT_ROOT, 'logs')

for directory in [RESULTS_DIR, MODELS_DIR, LOGS_DIR]:
    os.makedirs(directory, exist_ok=True)

RUN_TIMESTAMP = datetime.now().strftime("%Y%m%d_%H%M%S")

BEST_MODEL_PATH = os.path.join(MODELS_DIR, f'best_model_{RUN_TIMESTAMP}.ckpt')
TENSORBOARD_DIR = os.path.join(LOGS_DIR, f'tensorboard_{RUN_TIMESTAMP}')

# ============================================================================
# LOGGING
# ============================================================================
VERBOSE = True


def print_config():
    """Print configuration summary"""
    print("="*70)
    print("LSTM CONFIGURATION - WITH FULL FEATURES")
    print("="*70)
    print(f"Run: {RUN_TIMESTAMP}")
    print()
    print("DATA:")
    print(f"  Base Table: {SNOWFLAKE_TABLE}")
    print(f"  + PHECD features (diagnoses)")
    print(f"  + ING features (medications)")
    print(f"  + LAB features (labs)")
    print(f"  Splits: {TRAIN_SIZE}/{VAL_SIZE}/{TEST_SIZE}")
    print(f"  Feature Selection: {MIN_FEATURES}-{MAX_FEATURES} features")
    print()
    print("MODEL:")
    print(f"  Hidden Size: {HIDDEN_SIZE}")
    print(f"  Layers: {NUM_LAYERS}")
    print(f"  Dropout: {DROPOUT}")
    print()
    print("TRAINING:")
    print(f"  Batch Size: {BATCH_SIZE}")
    print(f"  Learning Rate: {LEARNING_RATE}")
    print(f"  Max Epochs: {MAX_EPOCHS}")
    print(f"  Early Stopping: {EARLY_STOPPING_PATIENCE}")
    print(f"  GPU: {USE_GPU}")
    print("="*70)


if __name__ == "__main__":
    print_config()
    print("\n Config loaded successfully!")
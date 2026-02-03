"""
main.py - Main execution pipeline
==================================
"""

import os
import sys
import warnings
warnings.filterwarnings('ignore')

import torch
import time
from datetime import datetime

from config import print_config, VERBOSE
from data_loader import load_data
from data_preprocessing import preprocess_pipeline
from dataset import create_dataloaders
from model import SimpleLSTM
from trainer import train_model, test_model, get_predictions
from evaluator import evaluate_model


def print_banner():
    """Print welcome banner"""
    print("\n" + "="*70)
    print(" "*15 + "SIMPLE LSTM FOR BMI PREDICTION")
    print(" "*20 + "Rolling Percentage Change")
    print("="*70)


def main():
    """Main execution pipeline"""
    
    print_banner()
    print_config()
    
    print("\n" + "="*70)
    print("PIPELINE START")
    print("="*70)
    
    # STAGE 1: DATA LOADING
    print("\n[1/6] DATA LOADING")
    try:
        df = load_data()
        from gap_filler import fill_missing_windows
        df= fill_missing_windows(df)
        print(" Gap filling complete")
    except Exception as e:
        print(f"Data loading failed: {e}")
        return None
    
    # STAGE 2: PREPROCESSING
    print("\n[2/6] DATA PREPROCESSING")
    try:
        data = preprocess_pipeline(df)
    except Exception as e:
        print(f"Preprocessing failed: {e}")
        import traceback
        traceback.print_exc()
        return None
    
    # STAGE 3: CREATE DATALOADERS
    print("\n[3/6] CREATING DATALOADERS")
    try:
        train_loader, val_loader, test_loader = create_dataloaders(data)
    except Exception as e:
        print(f"DataLoader creation failed: {e}")
        return None
    
    # STAGE 4: CREATE MODEL
    print("\n[4/6] MODEL INITIALIZATION")
    try:
        n_features = data['X_train'].shape[1]
        model = SimpleLSTM(input_size=n_features)
    except Exception as e:
        print(f"Model creation failed: {e}")
        return None
    
    # STAGE 5: TRAINING
    print("\n[5/6] MODEL TRAINING")
    try:
        trainer, checkpoint = train_model(model, train_loader, val_loader)
        test_results = test_model(trainer, model, test_loader)
    except Exception as e:
        print(f"Training failed: {e}")
        import traceback
        traceback.print_exc()
        return None
    
    # STAGE 6: EVALUATION
    print("\n[6/6] MODEL EVALUATION")
    try:
        device = 'cuda' if torch.cuda.is_available() else 'cpu'
        y_pred, y_true = get_predictions(model, test_loader, device)
        
        eval_results = evaluate_model(y_true, y_pred)
    except Exception as e:
        print(f"Evaluation failed: {e}")
        import traceback
        traceback.print_exc()
        return None
    
    # FINAL SUMMARY
    print("\n" + "="*70)
    print("PIPELINE COMPLETE - SUMMARY")
    print("="*70)
    
    print("\nDATA:")
    print(f"  Train samples: {len(train_loader.dataset):,}")
    print(f"  Val samples:   {len(val_loader.dataset):,}")
    print(f"  Test samples:  {len(test_loader.dataset):,}")
    print(f"  Features:      {n_features}")
    
    print("\nTRAINING:")
    print(f"  Best model:    {os.path.basename(checkpoint.best_model_path)}")
    print(f"  Best val loss: {checkpoint.best_model_score:.4f}")
    print(f"  Epochs:        {trainer.current_epoch}")
    
    print("\nTEST PERFORMANCE:")
    print(f"  RMSE:          {test_results['test_rmse']:.4f}%")
    print(f"  MAE:           {test_results['test_mae']:.4f}%")
    print(f"  R-squared:     {eval_results['metrics']['r2']:.4f}")
    print(f"  Pearson Corr:  {eval_results['metrics']['pcc']:.4f}")
    
    print("\nOUTPUT FILES:")
    print(f"  Model:         {os.path.basename(checkpoint.best_model_path)}")
    print(f"  Report:        {os.path.basename(eval_results['files']['report'])}")
    print(f"  Predictions:   {os.path.basename(eval_results['files']['csv'])}")
    print(f"  Plots:         results/*.png")
    
    print("\n" + "="*70)
    print("SUCCESS!")
    print("="*70)
    
    print("\nNext steps:")
    print("  1. Review report: results/report_*.txt")
    print("  2. Check plots: results/*.png")
    print("  3. TensorBoard: tensorboard --logdir logs/")
    print("  4. Survival analysis: python predict_survival_cohort.py")
    
    return {
        'data': data,
        'model': model,
        'trainer': trainer,
        'test_results': test_results,
        'eval_results': eval_results
    }


if __name__ == "__main__":
    print("\nStarting Simple LSTM Pipeline...")
    print("This will take approximately 10-20 minutes.\n")
    
    try:
        start_time = time.time()
        
        results = main()
        
        end_time = time.time()
        elapsed = (end_time - start_time) / 60
        
        if results is not None:
            print(f"\nTotal time: {elapsed:.1f} minutes")
            print("\nPipeline completed successfully!")
        else:
            print("\nPipeline failed. Check errors above.")
            sys.exit(1)
            
    except KeyboardInterrupt:
        print("\n\nPipeline cancelled by user.")
        sys.exit(0)
    except Exception as e:
        print(f"\nPipeline failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
"""
evaluator.py - Model evaluation and visualization
================================================
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
import os
from datetime import datetime

from config import RESULTS_DIR, VERBOSE

plt.style.use('default')
sns.set_palette("husl")


def calculate_metrics(y_true, y_pred):
    """Calculate evaluation metrics"""
    
    mse = mean_squared_error(y_true, y_pred)
    rmse = np.sqrt(mse)
    mae = mean_absolute_error(y_true, y_pred)
    r2 = r2_score(y_true, y_pred)
    
    # Pearson correlation
    pcc, pcc_pvalue = stats.pearsonr(y_true, y_pred)
    
    # Additional metrics
    medae = np.median(np.abs(y_true - y_pred))
    max_error = np.max(np.abs(y_true - y_pred))
    
    # Within tolerance
    within_5 = np.mean(np.abs(y_true - y_pred) < 5) * 100
    within_10 = np.mean(np.abs(y_true - y_pred) < 10) * 100
    
    metrics = {
        'mse': mse,
        'rmse': rmse,
        'mae': mae,
        'r2': r2,
        'pcc': pcc,
        'pcc_pvalue': pcc_pvalue,
        'medae': medae,
        'max_error': max_error,
        'within_5pct': within_5,
        'within_10pct': within_10,
        'n_samples': len(y_true),
        'mean_true': np.mean(y_true),
        'std_true': np.std(y_true),
        'mean_pred': np.mean(y_pred),
        'std_pred': np.std(y_pred)
    }
    
    if VERBOSE:
        print("\n" + "="*60)
        print("EVALUATION METRICS")
        print("="*60)
        print(f"Samples:          {metrics['n_samples']:,}")
        print(f"RMSE:             {metrics['rmse']:.4f}%")
        print(f"MAE:              {metrics['mae']:.4f}%")
        print(f"Median AE:        {metrics['medae']:.4f}%")
        print(f"Max Error:        {metrics['max_error']:.4f}%")
        print(f"R-squared:        {metrics['r2']:.4f}")
        print(f"Pearson Corr:     {metrics['pcc']:.4f} (p={metrics['pcc_pvalue']:.2e})")
        print(f"Within +/-5%%:     {metrics['within_5pct']:.1f}%")
        print(f"Within +/-10%%:    {metrics['within_10pct']:.1f}%")
        print("="*60)
    
    return metrics


def plot_predictions(y_true, y_pred, save_path):
    """Plot predictions vs actual"""
    
    fig, axes = plt.subplots(1, 2, figsize=(14, 6))
    
    # Scatter plot
    ax1 = axes[0]
    ax1.scatter(y_true, y_pred, alpha=0.5, s=20)
    
    min_val = min(y_true.min(), y_pred.min())
    max_val = max(y_true.max(), y_pred.max())
    ax1.plot([min_val, max_val], [min_val, max_val], 'r--', lw=2, label='Perfect')
    
    r2 = r2_score(y_true, y_pred)
    rmse = np.sqrt(mean_squared_error(y_true, y_pred))
    
    ax1.set_xlabel('True BMI Change (%)', fontsize=12)
    ax1.set_ylabel('Predicted BMI Change (%)', fontsize=12)
    ax1.set_title('LSTM Predictions vs Actual Values', fontsize=14, fontweight='bold')
    ax1.legend()
    ax1.grid(True, alpha=0.3)
    ax1.text(0.05, 0.95, f'R2 = {r2:.4f}\nRMSE = {rmse:.4f}%', 
             transform=ax1.transAxes, fontsize=11, va='top',
             bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))
    
    # Density plot
    ax2 = axes[1]
    hb = ax2.hexbin(y_true, y_pred, gridsize=30, cmap='YlOrRd', mincnt=1)
    ax2.plot([min_val, max_val], [min_val, max_val], 'b--', lw=2, label='Perfect')
    ax2.set_xlabel('True BMI Change (%)', fontsize=12)
    ax2.set_ylabel('Predicted BMI Change (%)', fontsize=12)
    ax2.set_title('Prediction Density', fontsize=14, fontweight='bold')
    ax2.legend()
    ax2.grid(True, alpha=0.3)
    plt.colorbar(hb, ax=ax2, label='Count')
    
    plt.tight_layout()
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    plt.close()
    
    if VERBOSE:
        print(f"Saved: {os.path.basename(save_path)}")


def plot_residuals(y_true, y_pred, save_path):
    """Plot residual analysis"""
    
    residuals = y_pred - y_true
    
    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    
    # Residuals vs Predicted
    ax1 = axes[0, 0]
    ax1.scatter(y_pred, residuals, alpha=0.5, s=20)
    ax1.axhline(y=0, color='r', linestyle='--', lw=2)
    ax1.set_xlabel('Predicted (%)', fontsize=11)
    ax1.set_ylabel('Residuals (%)', fontsize=11)
    ax1.set_title('Residuals vs Predicted', fontsize=12, fontweight='bold')
    ax1.grid(True, alpha=0.3)
    
    # Residuals vs Actual
    ax2 = axes[0, 1]
    ax2.scatter(y_true, residuals, alpha=0.5, s=20)
    ax2.axhline(y=0, color='r', linestyle='--', lw=2)
    ax2.set_xlabel('Actual (%)', fontsize=11)
    ax2.set_ylabel('Residuals (%)', fontsize=11)
    ax2.set_title('Residuals vs Actual', fontsize=12, fontweight='bold')
    ax2.grid(True, alpha=0.3)
    
    # Histogram
    ax3 = axes[1, 0]
    ax3.hist(residuals, bins=50, edgecolor='black', alpha=0.7)
    ax3.axvline(x=0, color='r', linestyle='--', lw=2)
    ax3.set_xlabel('Residuals (%)', fontsize=11)
    ax3.set_ylabel('Frequency', fontsize=11)
    ax3.set_title('Residual Distribution', fontsize=12, fontweight='bold')
    ax3.text(0.05, 0.95, f'Mean: {residuals.mean():.2f}\nStd: {residuals.std():.2f}',
             transform=ax3.transAxes, va='top',
             bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))
    ax3.grid(True, alpha=0.3)
    
    # Q-Q Plot
    ax4 = axes[1, 1]
    stats.probplot(residuals, dist="norm", plot=ax4)
    ax4.set_title('Q-Q Plot (Normality)', fontsize=12, fontweight='bold')
    ax4.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(save_path, dpi=300, bbox_inches='tight')
    plt.close()
    
    if VERBOSE:
        print(f"Saved: {os.path.basename(save_path)}")


def save_results_report(metrics, save_path):
    """Save text report with UTF-8 encoding"""
    
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    
    with open(save_path, 'w', encoding='utf-8') as f:
        f.write("="*70 + "\n")
        f.write("LSTM MODEL - EVALUATION REPORT\n")
        f.write("="*70 + "\n")
        f.write(f"Generated: {timestamp}\n\n")
        
        f.write("MODEL PERFORMANCE\n")
        f.write("-"*70 + "\n")
        f.write(f"Samples evaluated:        {metrics['n_samples']:,}\n")
        f.write(f"Root Mean Squared Error:  {metrics['rmse']:.4f}%\n")
        f.write(f"Mean Absolute Error:      {metrics['mae']:.4f}%\n")
        f.write(f"Median Absolute Error:    {metrics['medae']:.4f}%\n")
        f.write(f"Maximum Error:            {metrics['max_error']:.4f}%\n")
        f.write(f"R-squared:                {metrics['r2']:.4f}\n")
        f.write(f"Pearson Correlation:      {metrics['pcc']:.4f} (p={metrics['pcc_pvalue']:.2e})\n")
        f.write(f"\n")
        
        f.write("PREDICTION ACCURACY\n")
        f.write("-"*70 + "\n")
        f.write(f"Within +/-5%:             {metrics['within_5pct']:.1f}%\n")
        f.write(f"Within +/-10%:            {metrics['within_10pct']:.1f}%\n")
        f.write(f"\n")
        
        f.write("DATA STATISTICS\n")
        f.write("-"*70 + "\n")
        f.write(f"True values:\n")
        f.write(f"  Mean:                   {metrics['mean_true']:.4f}%\n")
        f.write(f"  Std Dev:                {metrics['std_true']:.4f}%\n")
        f.write(f"Predicted values:\n")
        f.write(f"  Mean:                   {metrics['mean_pred']:.4f}%\n")
        f.write(f"  Std Dev:                {metrics['std_pred']:.4f}%\n")
        f.write(f"\n")
        
        f.write("="*70 + "\n")
    
    if VERBOSE:
        print(f"Saved: {os.path.basename(save_path)}")


def evaluate_model(y_true, y_pred):
    """Complete evaluation pipeline"""
    
    if VERBOSE:
        print("\n" + "="*70)
        print("COMPREHENSIVE MODEL EVALUATION")
        print("="*70)
    
    os.makedirs(RESULTS_DIR, exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    # Calculate metrics
    metrics = calculate_metrics(y_true, y_pred)
    
    # Create plots
    if VERBOSE:
        print("\nCREATING VISUALIZATIONS")
        print("="*60)
    
    pred_plot = os.path.join(RESULTS_DIR, f'predictions_{timestamp}.png')
    plot_predictions(y_true, y_pred, pred_plot)
    
    resid_plot = os.path.join(RESULTS_DIR, f'residuals_{timestamp}.png')
    plot_residuals(y_true, y_pred, resid_plot)
    
    # Save predictions CSV
    csv_path = os.path.join(RESULTS_DIR, f'predictions_{timestamp}.csv')
    pd.DataFrame({
        'true': y_true,
        'predicted': y_pred,
        'error': y_pred - y_true,
        'abs_error': np.abs(y_pred - y_true)
    }).to_csv(csv_path, index=False)
    
    if VERBOSE:
        print(f"Saved: {os.path.basename(csv_path)}")
    
    # Save report
    report_path = os.path.join(RESULTS_DIR, f'report_{timestamp}.txt')
    save_results_report(metrics, report_path)
    
    if VERBOSE:
        print("\n" + "="*70)
        print("EVALUATION COMPLETE")
        print("="*70)
        print(f"Results saved to: {RESULTS_DIR}")
        print("="*70)
    
    return {
        'metrics': metrics,
        'plots': {'predictions': pred_plot, 'residuals': resid_plot},
        'files': {'report': report_path, 'csv': csv_path}
    }


if __name__ == "__main__":
    # Test with dummy data
    np.random.seed(42)
    y_true = np.random.randn(1000) * 5
    y_pred = y_true + np.random.randn(1000) * 2
    
    results = evaluate_model(y_true, y_pred)
    print("\nEvaluator test passed!")
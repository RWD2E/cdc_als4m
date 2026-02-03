#!/usr/bin/env python3
"""
LSTM Survival Analysis v3 - WITH CENSORING
===========================================
Improved version that censors unrealistic long-term survivors
"""

import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import pickle
from datetime import datetime
from lifelines import KaplanMeierFitter
from lifelines.statistics import logrank_test
from lifelines.utils import concordance_index
import warnings
warnings.filterwarnings('ignore')

OUTPUT_DIR = 'lstm_survival_results'
CENSORING_THRESHOLD = 60  # months - censor at 5 years (generous for ALS)

plt.style.use('default')
sns.set_palette("Set2")


def load_survival_predictions():
    """Load predictions generated for survival cohort"""
    print("\n Loading LSTM predictions for survival cohort...")
    
    import glob
    
    # Look for survival-specific predictions
    pred_pkl_files = glob.glob('results/lstm_survival_predictions_*.pkl')
    
    if pred_pkl_files:
        pred_file = max(pred_pkl_files, key=os.path.getctime)
        print(f"  Found: {pred_file}")
        
        with open(pred_file, 'rb') as f:
            data = pickle.load(f)
        
        # Create DataFrame
        df = pd.DataFrame({
            'patid': data['patient_ids'],
            'lstm_predicted_bmi_change': data['predictions'],
            'actual_bmi_change': data['actual'],
            'died': data['died'],
            'months_to_death': data['months_to_death']
        })
        
        # Add baseline BMI if available
        if data.get('baseline_bmi') is not None:
            df['baseline_bmi'] = data['baseline_bmi']
        
        # Convert to float64
        numeric_cols = ['lstm_predicted_bmi_change', 'actual_bmi_change', 
                       'died', 'months_to_death']
        
        if 'baseline_bmi' in df.columns:
            numeric_cols.append('baseline_bmi')
        
        for col in numeric_cols:
            if col in df.columns:
                df[col] = pd.to_numeric(df[col], errors='coerce').astype('float64')
        
        print(f"   Loaded {len(df)} patients with predictions and survival data")
        
        # Apply censoring
        print(f"\n  Applying censoring at {CENSORING_THRESHOLD} months...")
        n_censored = (df['months_to_death'] > CENSORING_THRESHOLD).sum()
        
        # Censor survival times and update event indicator
        df['months_to_death_censored'] = df['months_to_death'].clip(upper=CENSORING_THRESHOLD)
        df['died_censored'] = df['died'].copy()
        df.loc[df['months_to_death'] > CENSORING_THRESHOLD, 'died_censored'] = 0  # Mark as censored
        
        print(f"    Censored {n_censored} patients ({n_censored/len(df)*100:.1f}%)")
        print(f"    Actual deaths: {df['died'].sum():.0f}")
        print(f"    Deaths within {CENSORING_THRESHOLD} months: {df['died_censored'].sum():.0f}")
        print(f"    Median survival (censored): {df['months_to_death_censored'].median():.1f} months")
        
        return df
    
    # Fallback: try CSV
    pred_csv_files = glob.glob('results/lstm_survival_predictions_*.csv')
    
    if pred_csv_files:
        pred_file = max(pred_csv_files, key=os.path.getctime)
        print(f"  Found CSV: {pred_file}")
        
        df = pd.read_csv(pred_file)
        
        # Convert to float64
        numeric_cols = ['lstm_predicted_bmi_change', 'actual_bmi_change', 
                       'died', 'months_to_death']
        
        if 'baseline_bmi' in df.columns:
            numeric_cols.append('baseline_bmi')
        
        for col in numeric_cols:
            if col in df.columns:
                df[col] = pd.to_numeric(df[col], errors='coerce').astype('float64')
        
        # Apply censoring
        print(f"\n  Applying censoring at {CENSORING_THRESHOLD} months...")
        n_censored = (df['months_to_death'] > CENSORING_THRESHOLD).sum()
        
        df['months_to_death_censored'] = df['months_to_death'].clip(upper=CENSORING_THRESHOLD)
        df['died_censored'] = df['died'].copy()
        df.loc[df['months_to_death'] > CENSORING_THRESHOLD, 'died_censored'] = 0
        
        print(f"    Censored {n_censored} patients ({n_censored/len(df)*100:.1f}%)")
        print(f"   Loaded {len(df)} patients from CSV")
        
        return df
    
    print("   No survival predictions found!")
    print("  Please run: python lstm_predict_survival_cohort_standalone.py")
    return None


def create_risk_stratifications(df):
    """Create risk groups using tertiles"""
    
    print("\n Creating risk stratifications...")
    
    # 1. Baseline BMI stratification (if available)
    df_baseline = df.copy()
    
    if 'baseline_bmi' in df_baseline.columns:
        bmi_tertiles = np.percentile(df_baseline['baseline_bmi'].dropna(), [33.33, 66.67])
        
        df_baseline['risk_group'] = pd.cut(
            df_baseline['baseline_bmi'],
            bins=[-np.inf, bmi_tertiles[0], bmi_tertiles[1], np.inf],
            labels=['Low BMI', 'Normal BMI', 'High BMI']
        )
        df_baseline['risk_score'] = df_baseline['baseline_bmi']
        
        print(f"   Baseline BMI tertiles: {bmi_tertiles[0]:.1f}, {bmi_tertiles[1]:.1f}")
    else:
        print("   No baseline BMI available, using LSTM predictions only")
        df_baseline = None
    
    # 2. LSTM predicted risk stratification
    df_lstm = df.copy()
    
    # Use LSTM prediction as primary risk score 
    risk_score = df_lstm['lstm_predicted_bmi_change'].fillna(0)
    
    # Tertile groups
    risk_tertiles = np.percentile(risk_score, [33.33, 66.67])
    
    df_lstm['risk_group'] = pd.cut(
        risk_score,
        bins=[-np.inf, risk_tertiles[0], risk_tertiles[1], np.inf],
        labels=['Low Risk', 'Moderate Risk', 'High Risk']
    )
    df_lstm['risk_score'] = risk_score
    
    print(f"   LSTM risk tertiles: {risk_tertiles[0]:.3f}, {risk_tertiles[1]:.3f}")
    print(f"   Risk stratification complete")
    
    # Print group sizes
    print(f"\n  Risk group distribution:")
    for group in df_lstm['risk_group'].value_counts().sort_index().items():
        print(f"    {group[0]}: {group[1]} patients")
    
    return df_baseline, df_lstm


def calculate_cindex(df, risk_score_col='risk_score'):
    """Calculate C-index (concordance index)"""
    try:
        risk_scores = df[risk_score_col].values
        survival_times = df['months_to_death_censored'].values
        events = df['died_censored'].values
        
        c_index = concordance_index(survival_times, -risk_scores, events)
        return c_index
    except Exception as e:
        print(f"   C-index calculation failed: {e}")
        return None


def perform_logrank_test(df):
    """Log-rank test between risk groups"""
    
    groups = df['risk_group'].unique()
    
    if len(groups) < 2:
        return None
    
    results = {}
    
    if len(groups) >= 2:
        high_risk = df[df['risk_group'].astype(str).str.contains('High|high')]
        low_risk = df[df['risk_group'].astype(str).str.contains('Low|low')]
        
        if len(high_risk) > 0 and len(low_risk) > 0:
            try:
                logrank_result = logrank_test(
                    high_risk['months_to_death_censored'], low_risk['months_to_death_censored'],
                    high_risk['died_censored'], low_risk['died_censored']
                )
                
                results['high_vs_low'] = {
                    'groups': "High Risk vs Low Risk",
                    'test_statistic': logrank_result.test_statistic,
                    'p_value': logrank_result.p_value,
                    'significant': logrank_result.p_value < 0.05
                }
            except Exception as e:
                print(f"   Log-rank test failed: {e}")
    
    return results


def create_kaplan_meier_plot(df_baseline, df_lstm, output_dir, censoring_threshold):
    """Create Kaplan-Meier survival curves"""
    
    print("\n Creating Kaplan-Meier curves...")
    
    # Calculate statistics
    c_index_baseline = calculate_cindex(df_baseline) if df_baseline is not None else None
    c_index_lstm = calculate_cindex(df_lstm)
    
    logrank_baseline = perform_logrank_test(df_baseline) if df_baseline is not None else None
    logrank_lstm = perform_logrank_test(df_lstm)
    
    # Determine number of subplots
    n_plots = 2 if df_baseline is not None else 1
    
    # Create plot
    fig, axes = plt.subplots(1, n_plots, figsize=(8*n_plots, 8))
    if n_plots == 1:
        axes = [axes]
    
    fig.suptitle(f'Kaplan-Meier Survival Analysis: LSTM BMI Predictions\n(Censored at {censoring_threshold} months)', 
                 fontsize=16, fontweight='bold', y=0.98)
    
    kmf = KaplanMeierFitter()
    
    plot_idx = 0
    
    # Left: Baseline BMI (if available)
    if df_baseline is not None:
        ax1 = axes[plot_idx]
        plot_idx += 1
        colors_baseline = {'Low BMI': '#e74c3c', 'Normal BMI': '#f39c12', 'High BMI': '#3498db'}
        
        for group in sorted(df_baseline['risk_group'].dropna().unique()):
            group_data = df_baseline[df_baseline['risk_group'] == group]
            kmf.fit(group_data['months_to_death_censored'], group_data['died_censored'],
                    label=f"{group} (n={len(group_data)})")
            kmf.plot_survival_function(ax=ax1, color=colors_baseline.get(group, '#95a5a6'),
                                       linewidth=3, alpha=0.8)
        
        ax1.set_title('Baseline BMI Stratification', fontsize=14, fontweight='bold', pad=20)
        ax1.set_xlabel('Months from Last Observation', fontsize=12)
        ax1.set_ylabel('Survival Probability', fontsize=12)
        ax1.grid(True, alpha=0.3)
        ax1.legend(loc='upper right', fontsize=10)
        ax1.set_ylim(0, 1.05)
        ax1.set_xlim(0, censoring_threshold)
        
        # Stats
        stats_text = []
        if c_index_baseline:
            stats_text.append(f"C-index: {c_index_baseline:.3f}")
        if logrank_baseline and 'high_vs_low' in logrank_baseline:
            stats_text.append(f"Log-rank p: {logrank_baseline['high_vs_low']['p_value']:.4f}")
        if stats_text:
            ax1.text(0.02, 0.02, '\n'.join(stats_text), transform=ax1.transAxes,
                    fontsize=11, bbox=dict(boxstyle='round', facecolor='lightblue', alpha=0.8))
    
    # Right (or only): LSTM predictions
    ax2 = axes[plot_idx]
    colors_lstm = {'Low Risk': '#2ecc71', 'Moderate Risk': '#f39c12', 'High Risk': '#e74c3c'}
    
    for group in sorted(df_lstm['risk_group'].dropna().unique()):
        group_data = df_lstm[df_lstm['risk_group'] == group]
        kmf.fit(group_data['months_to_death_censored'], group_data['died_censored'],
                label=f"{group} (n={len(group_data)})")
        kmf.plot_survival_function(ax=ax2, color=colors_lstm.get(group, '#95a5a6'),
                                   linewidth=3, alpha=0.8)
    
    ax2.set_title('LSTM Predicted Risk Stratification', fontsize=14, fontweight='bold', pad=20)
    ax2.set_xlabel('Months from Last Observation', fontsize=12)
    ax2.set_ylabel('Survival Probability', fontsize=12)
    ax2.grid(True, alpha=0.3)
    ax2.legend(loc='upper right', fontsize=10)
    ax2.set_ylim(0, 1.05)
    ax2.set_xlim(0, censoring_threshold)
    
    # Stats
    stats_text = []
    if c_index_lstm:
        stats_text.append(f"C-index: {c_index_lstm:.3f}")
    if logrank_lstm and 'high_vs_low' in logrank_lstm:
        stats_text.append(f"Log-rank p: {logrank_lstm['high_vs_low']['p_value']:.4f}")
    if c_index_baseline and c_index_lstm:
        improvement = c_index_lstm - c_index_baseline
        stats_text.append(f"Δ C-index: {improvement:+.3f}")
    if stats_text:
        ax2.text(0.02, 0.02, '\n'.join(stats_text), transform=ax2.transAxes,
                fontsize=11, bbox=dict(boxstyle='round', facecolor='lightgreen', alpha=0.8))
    
    plt.tight_layout()
    
    # Save
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f"{output_dir}/kaplan_meier_lstm_censored_{timestamp}.png"
    plt.savefig(filename, dpi=300, bbox_inches='tight', facecolor='white')
    plt.close()
    
    print(f"   Saved: {filename}")
    
    return {
        'filename': filename,
        'baseline': {'c_index': c_index_baseline, 'logrank': logrank_baseline},
        'lstm': {'c_index': c_index_lstm, 'logrank': logrank_lstm}
    }


def save_detailed_results(df_lstm, plot_stats, output_dir, censoring_threshold):
    """Save detailed results to text file"""
    
    print("\n Saving detailed results...")
    
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    results_file = f"{output_dir}/survival_analysis_results_censored_{timestamp}.txt"
    
    with open(results_file, 'w') as f:
        f.write("="*70 + "\n")
        f.write("LSTM SURVIVAL ANALYSIS RESULTS (WITH CENSORING)\n")
        f.write("="*70 + "\n")
        f.write(f"Analysis Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write(f"Censoring Threshold: {censoring_threshold} months\n")
        f.write(f"Total Patients: {len(df_lstm)}\n")
        f.write(f"Deaths (uncensored): {df_lstm['died'].sum():.0f} ({df_lstm['died'].mean()*100:.1f}%)\n")
        f.write(f"Deaths (within {censoring_threshold}m): {df_lstm['died_censored'].sum():.0f} ({df_lstm['died_censored'].mean()*100:.1f}%)\n")
        f.write(f"Median Survival (censored): {df_lstm['months_to_death_censored'].median():.1f} months\n")
        f.write("\n")
        
        f.write("="*70 + "\n")
        f.write("RISK GROUP DISTRIBUTION\n")
        f.write("="*70 + "\n")
        for group in df_lstm['risk_group'].value_counts().sort_index().items():
            group_data = df_lstm[df_lstm['risk_group'] == group[0]]
            f.write(f"\n{group[0]}:\n")
            f.write(f"  N: {group[1]}\n")
            f.write(f"  Deaths (censored): {group_data['died_censored'].sum():.0f} ({group_data['died_censored'].mean()*100:.1f}%)\n")
            f.write(f"  Median survival: {group_data['months_to_death_censored'].median():.1f} months\n")
            f.write(f"  Mean LSTM prediction: {group_data['lstm_predicted_bmi_change'].mean():.2f}%\n")
        
        f.write("\n")
        f.write("="*70 + "\n")
        f.write("STATISTICAL TESTS\n")
        f.write("="*70 + "\n")
        
        if plot_stats['lstm']['c_index']:
            f.write(f"\nC-index (LSTM Risk): {plot_stats['lstm']['c_index']:.3f}\n")
            
        if plot_stats['baseline']['c_index']:
            f.write(f"C-index (Baseline BMI): {plot_stats['baseline']['c_index']:.3f}\n")
            improvement = plot_stats['lstm']['c_index'] - plot_stats['baseline']['c_index']
            f.write(f"Improvement: {improvement:+.3f}\n")
        
        if plot_stats['lstm']['logrank'] and 'high_vs_low' in plot_stats['lstm']['logrank']:
            f.write(f"\nLog-rank test (High vs Low Risk):\n")
            f.write(f"  Test statistic: {plot_stats['lstm']['logrank']['high_vs_low']['test_statistic']:.3f}\n")
            f.write(f"  P-value: {plot_stats['lstm']['logrank']['high_vs_low']['p_value']:.4f}\n")
            f.write(f"  Significant (p<0.05): {plot_stats['lstm']['logrank']['high_vs_low']['significant']}\n")
        
        f.write("\n")
        f.write("="*70 + "\n")
        f.write("LSTM PREDICTIONS SUMMARY\n")
        f.write("="*70 + "\n")
        f.write(f"Mean predicted BMI change: {df_lstm['lstm_predicted_bmi_change'].mean():.2f}%\n")
        f.write(f"Std predicted BMI change: {df_lstm['lstm_predicted_bmi_change'].std():.2f}%\n")
        f.write(f"Range: [{df_lstm['lstm_predicted_bmi_change'].min():.2f}%, {df_lstm['lstm_predicted_bmi_change'].max():.2f}%]\n")
        
        if 'actual_bmi_change' in df_lstm.columns:
            f.write(f"\nMean actual BMI change: {df_lstm['actual_bmi_change'].mean():.2f}%\n")
            f.write(f"Std actual BMI change: {df_lstm['actual_bmi_change'].std():.2f}%\n")
            
            # Correlation
            corr = np.corrcoef(df_lstm['lstm_predicted_bmi_change'], df_lstm['actual_bmi_change'])[0,1]
            f.write(f"Correlation (predicted vs actual): {corr:.3f}\n")
        
        f.write("\n")
        f.write("="*70 + "\n")
    
    print(f"   Saved: {results_file}")
    
    return results_file


def main():
    """Main execution"""
    
    print("="*70)
    print("LSTM SURVIVAL ANALYSIS v3 (WITH CENSORING)")
    print("="*70)
    
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    
    # Load predictions with survival data
    df = load_survival_predictions()
    if df is None or len(df) == 0:
        print("\n No predictions available")
        print("\nPlease run:")
        print("  python lstm_predict_survival_cohort_standalone.py")
        return
    
    # Create risk groups
    df_baseline, df_lstm = create_risk_stratifications(df)
    
    # Create KM plots
    plot_stats = create_kaplan_meier_plot(df_baseline, df_lstm, OUTPUT_DIR, CENSORING_THRESHOLD)
    
    # Save detailed results
    results_file = save_detailed_results(df_lstm, plot_stats, OUTPUT_DIR, CENSORING_THRESHOLD)
    
    # Summary
    print("\n" + "="*70)
    print(" LSTM SURVIVAL ANALYSIS COMPLETE")
    print("="*70)
    print(f"Patients analyzed: {len(df)}")
    print(f"Deaths (uncensored): {df['died'].sum():.0f} ({df['died'].mean()*100:.1f}%)")
    print(f"Deaths (within {CENSORING_THRESHOLD}m): {df['died_censored'].sum():.0f} ({df['died_censored'].mean()*100:.1f}%)")
    print(f"Median survival (censored): {df['months_to_death_censored'].median():.1f} months")
    
    if plot_stats['lstm']['c_index']:
        print(f"\n C-index (LSTM Risk): {plot_stats['lstm']['c_index']:.3f}")
        
        if plot_stats['baseline']['c_index']:
            improvement = plot_stats['lstm']['c_index'] - plot_stats['baseline']['c_index']
            print(f" C-index (Baseline BMI): {plot_stats['baseline']['c_index']:.3f}")
            print(f" Improvement: {improvement:+.3f}")
            
            if improvement > 0.02:
                print(f"\n LSTM SIGNIFICANTLY IMPROVES survival stratification!")
            elif improvement > 0:
                print(f"\n LSTM modestly improves survival stratification")
            else:
                print(f"\n LSTM does not improve survival stratification")
    
    if plot_stats['lstm']['logrank'] and 'high_vs_low' in plot_stats['lstm']['logrank']:
        lr = plot_stats['lstm']['logrank']['high_vs_low']
        print(f" Log-rank test (High vs Low):")
        print(f"   P-value: {lr['p_value']:.4f}")
        if lr['significant']:
            print(f"    Statistically significant (p < 0.05)")
        else:
            print(f"    Not statistically significant")
    
    print(f"\n Files saved:")
    print(f"   Plot: {plot_stats['filename']}")
    print(f"   Results: {results_file}")
    print("="*70)


if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(f"\n ERROR: {e}")
        import traceback
        traceback.print_exc()
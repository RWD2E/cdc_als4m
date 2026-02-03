"""
dataset.py - PyTorch Dataset
============================
"""

import torch
from torch.utils.data import Dataset, DataLoader
import numpy as np

from config import BATCH_SIZE, NUM_WORKERS, VERBOSE


class SimpleLSTMDataset(Dataset):
    """
    Simple dataset - each sample is one window
    We reshape to (1, n_features) to create a sequence of length 1
    """
    
    def __init__(self, X, y):
        # X: (n_samples, n_features)
        # Reshape to (n_samples, 1, n_features) for LSTM
        self.X = torch.FloatTensor(X).unsqueeze(1)  # Add sequence dimension
        self.y = torch.FloatTensor(y)
        
        if VERBOSE:
            print(f"   Dataset created: {len(self)} samples, X shape: {self.X.shape}")
    
    def __len__(self):
        return len(self.y)
    
    def __getitem__(self, idx):
        return self.X[idx], self.y[idx]


def create_dataloaders(data, batch_size=BATCH_SIZE, num_workers=NUM_WORKERS):
    """Create PyTorch DataLoaders"""
    if VERBOSE:
        print("\n CREATING DATALOADERS")
        print("="*60)
    
    train_dataset = SimpleLSTMDataset(data['X_train'], data['y_train'])
    val_dataset = SimpleLSTMDataset(data['X_val'], data['y_val'])
    test_dataset = SimpleLSTMDataset(data['X_test'], data['y_test'])
    
    train_loader = DataLoader(
        train_dataset, batch_size=batch_size, shuffle=True,
        num_workers=num_workers, pin_memory=True
    )
    
    val_loader = DataLoader(
        val_dataset, batch_size=batch_size, shuffle=False,
        num_workers=num_workers, pin_memory=True
    )
    
    test_loader = DataLoader(
        test_dataset, batch_size=batch_size, shuffle=False,
        num_workers=num_workers, pin_memory=True
    )
    
    if VERBOSE:
        print(f" DataLoaders created")
        print(f"   Train: {len(train_loader)} batches")
        print(f"   Val:   {len(val_loader)} batches")
        print(f"   Test:  {len(test_loader)} batches")
    
    return train_loader, val_loader, test_loader


if __name__ == "__main__":
    from data_loader import load_data
    from data_preprocessing import preprocess_pipeline
    
    df = load_data()
    data = preprocess_pipeline(df)
    train_loader, val_loader, test_loader = create_dataloaders(data)
    
    # Test batch
    X_batch, y_batch = next(iter(train_loader))
    print(f"\n Dataset test passed!")
    print(f"   Batch X shape: {X_batch.shape}")
    print(f"   Batch y shape: {y_batch.shape}")
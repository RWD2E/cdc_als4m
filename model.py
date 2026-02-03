"""
model.py - Improved LSTM Model
==============================
"""

import torch
import torch.nn as nn
import pytorch_lightning as pl

from config import HIDDEN_SIZE, NUM_LAYERS, DROPOUT, LEARNING_RATE, VERBOSE


class ImprovedLSTM(pl.LightningModule):
    """
    Improved LSTM with better architecture
    """
    
    def __init__(self, input_size, hidden_size=HIDDEN_SIZE, 
                 num_layers=NUM_LAYERS, dropout=DROPOUT, lr=LEARNING_RATE):
        super().__init__()
        self.save_hyperparameters()
        
        # Input projection (helps with high-dimensional input)
        self.input_proj = nn.Sequential(
            nn.Linear(input_size, input_size // 2),
            nn.ReLU(),
            nn.Dropout(dropout * 0.5)
        )
        
        # LSTM
        self.lstm = nn.LSTM(
            input_size=input_size // 2,
            hidden_size=hidden_size,
            num_layers=num_layers,
            batch_first=True,
            dropout=dropout if num_layers > 1 else 0
        )
        
        # Output layers with residual-like connection
        self.fc = nn.Sequential(
            nn.Linear(hidden_size, hidden_size),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(hidden_size, hidden_size // 2),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(hidden_size // 2, 1)
        )
        
        # Loss function
        #self.loss_fn = nn.MSELoss()
        self.loss_fn = nn.HuberLoss(delta=1.0)
        # Initialize weights
        self._init_weights()
        
        if VERBOSE:
            total_params = sum(p.numel() for p in self.parameters())
            print(f"IMPROVED MODEL ARCHITECTURE")
            print("="*60)
            print(f"Input size: {input_size}")
            print(f"Input projection: {input_size} -> {input_size // 2}")
            print(f"Hidden size: {hidden_size}")
            print(f"Layers: {num_layers}")
            print(f"Dropout: {dropout}")
            print(f"Total parameters: {total_params:,}")
            print("="*60)
    
    def _init_weights(self):
        """Better weight initialization"""
        for name, param in self.named_parameters():
            if 'weight' in name:
                if 'lstm' in name:
                    nn.init.orthogonal_(param)
                else:
                    nn.init.kaiming_normal_(param, mode='fan_in', nonlinearity='relu')
            elif 'bias' in name:
                nn.init.constant_(param, 0)
    
    def forward(self, x):
        # x: (batch, seq_len=1, features)
        
        # Project input
        x_proj = self.input_proj(x)
        
        # LSTM
        lstm_out, (hidden, cell) = self.lstm(x_proj)
        
        # Use last hidden state
        last_hidden = hidden[-1]
        
        # Output
        output = self.fc(last_hidden)
        return output.squeeze(-1)
    
    def training_step(self, batch, batch_idx):
        x, y = batch
        y_pred = self(x)
        loss = self.loss_fn(y_pred, y)
        
        # Metrics
        mae = torch.abs(y_pred - y).mean()
        rmse = torch.sqrt(loss)
        
        # R² calculation
        ss_res = torch.sum((y - y_pred) ** 2)
        ss_tot = torch.sum((y - y.mean()) ** 2)
        r2 = 1 - ss_res / (ss_tot + 1e-8)
        
        self.log('train_loss', loss, on_step=False, on_epoch=True, prog_bar=True)
        self.log('train_mae', mae, on_step=False, on_epoch=True, prog_bar=True)
        self.log('train_rmse', rmse, on_step=False, on_epoch=True)
        self.log('train_r2', r2, on_step=False, on_epoch=True)
        
        return loss
    
    def validation_step(self, batch, batch_idx):
        x, y = batch
        y_pred = self(x)
        loss = self.loss_fn(y_pred, y)
        
        mae = torch.abs(y_pred - y).mean()
        rmse = torch.sqrt(loss)
        
        # R²
        ss_res = torch.sum((y - y_pred) ** 2)
        ss_tot = torch.sum((y - y.mean()) ** 2)
        r2 = 1 - ss_res / (ss_tot + 1e-8)
        
        self.log('val_loss', loss, on_step=False, on_epoch=True, prog_bar=True)
        self.log('val_mae', mae, on_step=False, on_epoch=True, prog_bar=True)
        self.log('val_rmse', rmse, on_step=False, on_epoch=True)
        self.log('val_r2', r2, on_step=False, on_epoch=True)
        
        return loss
    
    def test_step(self, batch, batch_idx):
        x, y = batch
        y_pred = self(x)
        loss = self.loss_fn(y_pred, y)
        
        mae = torch.abs(y_pred - y).mean()
        rmse = torch.sqrt(loss)
        
        self.log('test_loss', loss)
        self.log('test_mae', mae)
        self.log('test_rmse', rmse)
        
        return loss
    
    def configure_optimizers(self):
        optimizer = torch.optim.AdamW(
            self.parameters(), 
            lr=self.hparams.lr,
            weight_decay=0.01  # L2 regularization
        )
        
        scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(
            optimizer, 
            mode='min', 
            factor=0.5, 
            patience=10,
            min_lr=1e-6
        )
        
        return {
            'optimizer': optimizer,
            'lr_scheduler': {
                'scheduler': scheduler,
                'monitor': 'val_loss'
            }
        }


# Use this as SimpleLSTM
SimpleLSTM = ImprovedLSTM


if __name__ == "__main__":
    model = SimpleLSTM(input_size=50)
    x = torch.randn(32, 1, 50)
    y_pred = model(x)
    print(f"\nModel test passed!")
    print(f"   Input: {x.shape} -> Output: {y_pred.shape}")
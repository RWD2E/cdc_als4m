"""
trainer.py - Training logic
===========================
"""

import torch
import pytorch_lightning as pl
from pytorch_lightning.callbacks import EarlyStopping, ModelCheckpoint, LearningRateMonitor
from pytorch_lightning.loggers import TensorBoardLogger

from config import (
    MAX_EPOCHS, EARLY_STOPPING_PATIENCE, GRADIENT_CLIP_VAL,
    USE_GPU, BEST_MODEL_PATH, TENSORBOARD_DIR, MODELS_DIR,
    VERBOSE, RANDOM_SEED
)


def create_trainer():
    """Create PyTorch Lightning trainer with callbacks"""
    
    if VERBOSE:
        print("\n CONFIGURING TRAINER")
        print("="*60)
    
    # Callbacks
    early_stop = EarlyStopping(
        monitor='val_loss',
        patience=EARLY_STOPPING_PATIENCE,
        mode='min',
        verbose=True
    )
    
    checkpoint = ModelCheckpoint(
        dirpath=MODELS_DIR,
        filename='best_model_{epoch:02d}_{val_loss:.4f}',
        monitor='val_loss',
        mode='min',
        save_top_k=1,
        verbose=True
    )
    
    lr_monitor = LearningRateMonitor(logging_interval='epoch')
    
    # Logger
    logger = TensorBoardLogger(
        save_dir=TENSORBOARD_DIR,
        name='simple_lstm'
    )
    
    # Determine device
    if USE_GPU and torch.cuda.is_available():
        accelerator = 'gpu'
        devices = 1
        device_name = torch.cuda.get_device_name(0)
    else:
        accelerator = 'cpu'
        devices = 1
        device_name = 'CPU'
    
    # Create trainer
    trainer = pl.Trainer(
        max_epochs=MAX_EPOCHS,
        callbacks=[early_stop, checkpoint, lr_monitor],
        logger=logger,
        accelerator=accelerator,
        devices=devices,
        gradient_clip_val=GRADIENT_CLIP_VAL,
        deterministic=True,
        enable_progress_bar=True,
        log_every_n_steps=10
    )
    
    if VERBOSE:
        print(f"Max epochs: {MAX_EPOCHS}")
        print(f"Device: {device_name}")
        print(f"Early stopping patience: {EARLY_STOPPING_PATIENCE}")
        print(f" Trainer configured")
    
    return trainer, checkpoint


def train_model(model, train_loader, val_loader):
    """Train the model"""
    
    if VERBOSE:
        print("\n STARTING TRAINING")
        print("="*60)
        print(f"Training samples: {len(train_loader.dataset):,}")
        print(f"Validation samples: {len(val_loader.dataset):,}")
        print("="*60)
    
    # Set seed
    pl.seed_everything(RANDOM_SEED, workers=True)
    
    # Create trainer
    trainer, checkpoint = create_trainer()
    
    # Train
    trainer.fit(model, train_loader, val_loader)
    
    if VERBOSE:
        print("\n TRAINING COMPLETE")
        print("="*60)
        print(f"Best model: {checkpoint.best_model_path}")
        print(f"Best val loss: {checkpoint.best_model_score:.4f}")
        print(f"Epochs trained: {trainer.current_epoch}")
        print("="*60)
    
    return trainer, checkpoint


def test_model(trainer, model, test_loader):
    """Test the model"""
    
    if VERBOSE:
        print("\n TESTING MODEL")
        print("="*60)
    
    test_results = trainer.test(model, test_loader, verbose=False)
    
    if VERBOSE:
        print("Test Results:")
        for key, value in test_results[0].items():
            print(f"  {key}: {value:.4f}")
        print("="*60)
    
    return test_results[0]


def get_predictions(model, dataloader, device='cpu'):
    """Get model predictions"""
    
    model.eval()
    model = model.to(device)
    
    all_preds = []
    all_targets = []
    
    with torch.no_grad():
        for x, y in dataloader:
            x = x.to(device)
            preds = model(x)
            
            all_preds.append(preds.cpu().numpy())
            all_targets.append(y.numpy())
    
    import numpy as np
    predictions = np.concatenate(all_preds)
    targets = np.concatenate(all_targets)
    
    return predictions, targets


if __name__ == "__main__":
    from data_loader import load_data
    from data_preprocessing import preprocess_pipeline
    from dataset import create_dataloaders
    from model import SimpleLSTM
    
    print("Testing trainer...")
    df = load_data()
    data = preprocess_pipeline(df)
    train_loader, val_loader, test_loader = create_dataloaders(data)
    
    n_features = data['X_train'].shape[1]
    model = SimpleLSTM(input_size=n_features)
    
    print("\n Trainer test passed!")
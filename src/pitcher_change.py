import os
import torch
import pandas as pd
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
from sklearn.model_selection import train_test_split  # type: ignore
from torch.utils.data import DataLoader, Dataset
from typing import Self

# Set device
DEVICE: torch.device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')


class PitcherChangeDataset(Dataset):  # Custom Dataset Class
    def __init__(self: Self, df: pd.DataFrame) -> None:
        for col, dtype in zip(df.columns, df.dtypes):
            if dtype == bool:
                df[col] = df[col].astype(int)

        # Separate features and target
        data_X: pd.DataFrame = df.drop(columns=['pitcher_change'])
        data_y: pd.Series = df['pitcher_change']

        # Convert to tensors
        self.X_tensor: torch.Tensor = torch.tensor(data_X.values, dtype=torch.float32).to(DEVICE)
        self.y_tensor: torch.Tensor = torch.tensor(data_y.values, dtype=torch.float32).view(-1, 1).to(DEVICE)
        self.length: int = self.X_tensor.size(0)
        assert self.length == self.y_tensor.size(0)
        return None

    def __len__(self: Self) -> int:
        return self.length

    def __getitem__(self: Self, idx: int) -> tuple[torch.Tensor, torch.Tensor]:
        return self.X_tensor[idx], self.y_tensor[idx]


class ResidualBlock(nn.Module):
    """ Residual Block with BatchNorm and Dropout """

    def __init__(self: Self, in_features: int, hidden_features: int, dropout: float = 0.2) -> None:
        super(ResidualBlock, self).__init__()
        self.fc1: nn.Linear = nn.Linear(in_features, hidden_features)
        self.bn1: nn.BatchNorm1d = nn.BatchNorm1d(hidden_features)
        self.fc2: nn.Linear = nn.Linear(hidden_features, in_features)
        self.bn2: nn.BatchNorm1d = nn.BatchNorm1d(in_features)
        self.dropout: nn.Dropout = nn.Dropout(dropout)
        return None

    def forward(self: Self, x: torch.Tensor) -> torch.Tensor:
        identity: torch.Tensor = x
        out: torch.Tensor = F.relu(self.bn1(self.fc1(x)))
        out = self.dropout(out)
        out = self.bn2(self.fc2(out))
        out += identity  # Skip connection
        out = F.relu(out)
        return out


class SelfAttention(nn.Module):
    """ Self-Attention Mechanism for Dynamically Learning Important Features """

    def __init__(self: Self, input_dim: int) -> None:
        super(SelfAttention, self).__init__()
        self.query: nn.Linear = nn.Linear(input_dim, input_dim)
        self.key: nn.Linear = nn.Linear(input_dim, input_dim)
        self.value: nn.Linear = nn.Linear(input_dim, input_dim)
        self.scale: torch.Tensor = torch.sqrt(torch.FloatTensor([input_dim]))
        return None

    def forward(self: Self, x: torch.Tensor) -> torch.Tensor:
        Q: torch.Tensor = self.query(x)
        K: torch.Tensor = self.key(x)
        V: torch.Tensor = self.value(x)
        attn_scores: torch.Tensor = torch.matmul(Q, K.T) / self.scale  # Scaled dot-product
        attn_weights: torch.Tensor = F.softmax(attn_scores, dim=-1)
        out: torch.Tensor = torch.matmul(attn_weights, V)
        return out


class MixtureOfExperts(nn.Module):
    """ Mixture of Experts for Adaptation to Different Game Scenarios """

    def __init__(self: Self, input_dim: int, num_experts: int = 4) -> None:
        super(MixtureOfExperts, self).__init__()
        self.experts: nn.ModuleList = nn.ModuleList([nn.Linear(input_dim, 32) for _ in range(num_experts)])
        self.gate: nn.Linear = nn.Linear(input_dim, num_experts)
        return None

    def forward(self: Self, x: torch.Tensor) -> torch.Tensor:
        weights: torch.Tensor = F.softmax(self.gate(x), dim=-1)  # Gating function
        expert_outputs: torch.Tensor = torch.stack([expert(x) for expert in self.experts], dim=1)
        output: torch.Tensor = torch.sum(weights.unsqueeze(-1) * expert_outputs, dim=1)  # Weighted sum
        return output


class PitcherChangeModel(nn.Module):
    """ Advanced Model with Residual Connections, Attention, MoE, and Feature Transforms """

    def __init__(self: Self, input_size: int) -> None:
        super(PitcherChangeModel, self).__init__()

        # Initial feature transformation
        self.fc_input: nn.Linear = nn.Linear(input_size, 128)
        self.bn_input: nn.BatchNorm1d = nn.BatchNorm1d(128)
        self.dropout_input: nn.Dropout = nn.Dropout(0.1)

        # Residual Layers
        self.residual_blocks: nn.Sequential = nn.Sequential(
            ResidualBlock(128, 256, dropout=0.3),
            ResidualBlock(128, 256, dropout=0.3)
        )

        # Attention Mechanism
        self.attention: SelfAttention = SelfAttention(128)

        # Mixture of Experts
        self.moe: MixtureOfExperts = MixtureOfExperts(128, num_experts=4)

        # Fully Connected Output Layers
        self.fc_output: nn.Sequential = nn.Sequential(
            nn.Linear(128 + 32, 64),  # Includes MoE output
            nn.BatchNorm1d(64),
            nn.ReLU(),
            nn.Dropout(0.2),
            nn.Linear(64, 1),
            nn.Sigmoid()
        )

        return None

    def forward(self: Self, x: torch.Tensor) -> torch.Tensor:
        # Feature transformation
        x = F.relu(self.bn_input(self.fc_input(x)))
        x = self.dropout_input(x)

        # Residual blocks
        x = self.residual_blocks(x)

        # Attention
        attn_out: torch.Tensor = self.attention(x)

        # Mixture of Experts
        moe_out: torch.Tensor = self.moe(x)

        # Concatenate attention and MoE outputs with main features
        combined: torch.Tensor = torch.cat((attn_out, moe_out), dim=-1)

        # Output prediction
        out: torch.Tensor = self.fc_output(combined)
        return out


def train_model(df: pd.DataFrame, n_epochs: int = 20, batch_size: int = 32, lr: float = 0.001) -> None:
    # Split dataset into training and testing sets
    train_df: pd.DataFrame
    test_df: pd.DataFrame
    train_df, test_df = train_test_split(df, test_size=0.2, stratify=df['pitcher_change'], random_state=42)
    train_dataset: PitcherChangeDataset = PitcherChangeDataset(train_df)
    test_dataset: PitcherChangeDataset = PitcherChangeDataset(test_df)

    # Data loaders
    train_loader: DataLoader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
    test_loader: DataLoader = DataLoader(test_dataset, batch_size=batch_size, shuffle=False)

    # Model setup
    input_size: int = train_dataset.X_tensor.size(1)
    model: PitcherChangeModel = PitcherChangeModel(input_size=input_size).to(DEVICE)
    criterion: nn.BCELoss = nn.BCELoss()
    optimizer: optim.Adam = optim.Adam(model.parameters(), lr=lr)
    best_loss: float = float('inf')

    # Training loop
    for epoch in range(n_epochs):
        model.train()
        total_train_loss: float = 0.0

        for inputs, labels in train_loader:
            optimizer.zero_grad()
            outputs: torch.Tensor = model(inputs)
            loss: torch.Tensor = criterion(outputs, labels)
            loss.backward()
            optimizer.step()
            total_train_loss += loss.item()

        # Evaluate out-of-sample performance (raw probabilities)
        model.eval()
        total_test_loss: float = 0.0

        with torch.no_grad():
            for inputs, labels in test_loader:
                outputs = model(inputs)  # Raw probabilities
                loss = criterion(outputs, labels)
                total_test_loss += loss.item()

        avg_train_loss: float = total_train_loss / len(train_loader)
        avg_test_loss: float = total_test_loss / len(test_loader)

        # Save model if it has the best out-of-sample performance
        if avg_test_loss < best_loss:
            best_loss = avg_test_loss
            traced_model: torch.jit.ScriptModule = torch.jit.trace(model, torch.randn(1, input_size))
            traced_model.save(os.path.join('misc', 'torch', 'pitcher_change_model.pt'))
            print(f'Best model saved as "pitcher_change_model.pt" with Test Loss: {best_loss:.4f}')

        print(
            f'Epoch {epoch+1}/{n_epochs} - Train Loss: {avg_train_loss:.4f} - Test Loss: {avg_test_loss:.4f} - '
            f'Best Test Loss: {best_loss:.4f}'
        )

    return None


# Main Function
if __name__ == '__main__':
    # Load dataset
    os.chdir(os.path.dirname(os.path.dirname(__file__)))
    assert os.getcwd() == '/home/philip/Documents/R projects/baseball'
    df: pd.DataFrame = pd.read_csv(os.path.join('processed', 'pitcher_change.csv'))

    # Train the model
    train_model(df, n_epochs=256, batch_size=64)

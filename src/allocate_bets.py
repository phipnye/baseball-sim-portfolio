import os
import numpy as np
import pandas as pd
from scipy.optimize import minimize, OptimizeResult  # type: ignore
from typing import Any


def american_to_decimal(odds: int) -> float:
    return (odds / 100) + 1 if odds > 0 else (100 / abs(odds)) + 1


def bayesian_bet_allocation(result_files: list[str], away_odds: dict[str, int], home_odds: dict[str, int],
                            over_unders: list[float], B: int, game_date: str) -> pd.DataFrame:
    n_games: int = len(result_files)

    if not (len(home_odds) == len(away_odds) == len(over_unders) == n_games):
        raise ValueError('Mismatch in number of games and odds lists.')

    # Make sure result files match up with odds
    matchups: list[tuple[str, str]] = []
    for res_idx, res_file in enumerate(result_files):
        away_team: str = list(away_odds.keys())[res_idx]
        home_team: str = list(home_odds.keys())[res_idx]
        assert (
            os.path.basename(res_file)
            == f'{away_team.replace(" ", "").lower()}_{home_team.replace(" ", "").lower()}_{game_date}.csv'
        )

        matchups.append((away_team, home_team))

    # Read in CSV files with simulated game scores
    simulated_scores: list[pd.DataFrame] = [pd.read_csv(f) for f in result_files]
    S: int = simulated_scores[0].shape[0]

    # Convert American odds to decimal odds
    away_dec_odds: list[float] = [american_to_decimal(odds) for odds in away_odds.values()]
    home_dec_odds: list[float] = [american_to_decimal(odds) for odds in home_odds.values()]
    over_odds: float = american_to_decimal(-110)

    payoff_matrix: np.ndarray = np.zeros((S, 3 * n_games))

    for i, df in enumerate(simulated_scores):
        total_score: pd.Series = df['away'] + df['home']

        # Compute payoffs per $1 stake using decimal odds
        payoff_matrix[:, 3 * i] = np.where(df['away'] > df['home'], away_dec_odds[i] - 1, -1)
        payoff_matrix[:, 3 * i + 1] = np.where(df['home'] > df['away'], home_dec_odds[i] - 1, -1)
        payoff_matrix[:, 3 * i + 2] = np.where(total_score > over_unders[i], over_odds - 1, -1)

    # Compute empirical EV per bet
    ev_per_bet: np.ndarray = payoff_matrix.mean(axis=0)

    # Only allow bets with positive EV
    positive_ev_indices: np.ndarray = np.where(ev_per_bet > 0)[0]

    if len(positive_ev_indices) == 0:
        raise Exception('No bets with positive EV.')

    # Restrict payoff matrix and EV to positive EV bets only
    payoff_matrix_pos_ev: np.ndarray = payoff_matrix[:, positive_ev_indices]

    # Optimization: Minimize variance of total returns
    cov_matrix: np.ndarray = np.cov(payoff_matrix_pos_ev, rowvar=False)

    def portfolio_variance(w: np.ndarray) -> float:
        return w @ cov_matrix @ w

    # Constraints
    constraints: list[dict[str, Any]] = [
        {'type': 'eq', 'fun': lambda w: np.sum(w) - B}  # Full bankroll allocation
    ]

    bounds: list[tuple[int, int]] = [(0, B) for _ in range(len(positive_ev_indices))]
    initial_w: np.ndarray = np.full(len(positive_ev_indices), B / len(positive_ev_indices))

    result: OptimizeResult = minimize(
        portfolio_variance,
        initial_w,
        method='SLSQP',
        bounds=bounds,
        constraints=constraints
    )

    # Reconstruct full bet allocation vector
    w_full: np.ndarray = np.zeros(3 * n_games)

    if not result.success:
        raise Exception('Unsuccessful variance minimization.')

    w_full[positive_ev_indices] = result.x

    # Extract the details of the placed bets
    bet_details: list[dict[str, Any]] = []

    for idx in positive_ev_indices:
        game_idx: int = idx // 3
        away_team, home_team = matchups[game_idx]

        # Determine the specific bet type and decimal odds
        if idx % 3 == 0:
            bet_description: str = f'{away_team} ML ({away_odds[away_team]})'
            decimal_odds: float = away_dec_odds[game_idx]
        elif idx % 3 == 1:
            bet_description = f'{home_team} ML ({home_odds[home_team]})'
            decimal_odds = home_dec_odds[game_idx]
        else:
            bet_description = f'Over {over_unders[game_idx]} Total Runs'
            decimal_odds = over_odds

        bet_amount: float = float(w_full[idx])
        expected_value: float = float(ev_per_bet[idx]) * bet_amount  # Expected return on the bet
        payout_if_hits: float = bet_amount * decimal_odds  # Potential payout if bet wins

        bet_details.append({
            'Game': f'{away_team} vs {home_team}',
            'Bet': bet_description,
            'Amount': bet_amount,
            'Expected Return': expected_value,
            'Payout if Bet Hits': payout_if_hits
        })

    bet_df: pd.DataFrame = pd.DataFrame(bet_details)
    total_row: pd.DataFrame = pd.DataFrame({
        'Game': ['Total'],
        'Bet': [''],
        'Amount': [bet_df['Amount'].sum()],
        'Expected Return': [bet_df['Expected Return'].sum()],
        'Payout if Bet Hits': [bet_df['Payout if Bet Hits'].sum()]
    })
    bet_df = pd.concat([bet_df, total_row], axis=0, ignore_index=True)

    # Format currencies
    bet_df[['Amount', 'Expected Return', 'Payout if Bet Hits']] = (
        bet_df[['Amount', 'Expected Return', 'Payout if Bet Hits']].map(lambda amt: f'${amt:,.2f}')
    )

    return bet_df


if __name__ == '__main__':
    # Change working directory
    os.chdir(os.path.dirname(os.path.dirname(__file__)))
    assert os.getcwd() == '/home/philip/Documents/R projects/baseball'

    # Import data
    game_date: str = '20250328'
    result_files: list[str] = [
        os.path.join('results', f) for f in sorted(os.listdir('results')) if f'_{game_date}.csv' in f
    ]

    # Optimize betting strategy
    bet_strat: pd.DataFrame = bayesian_bet_allocation(
        result_files,
        away_odds={'Pirates': -150, 'Tigers': +180},
        home_odds={'Marlins': +125, 'Dodgers': -230},
        over_unders=[5.5, 7.5],
        B=5000,
        game_date=game_date
    )
    print(bet_strat)
    bet_strat.to_clipboard()

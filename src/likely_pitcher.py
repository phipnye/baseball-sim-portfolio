import datetime
import requests
import numpy as np
import pandas as pd
from typing import Any

MLB_API_URL: str = 'https://statsapi.mlb.com/api/v1'


def get_team_id(team_name: str) -> int:
    """ Get the MLB team ID from the given team name. """
    teams_url: str = f'{MLB_API_URL}/teams?sportId=1'
    response: dict[str, Any] = requests.get(teams_url).json()

    for team in response.get('teams', []):
        if team_name.lower() in team['name'].lower() or team_name.lower() in team['abbreviation'].lower():
            return team['id']

    raise ValueError(f'Team "{team_name}" not found.')


def get_bullpen_usage(team_id: int, game_date: str, days: int = 60) -> pd.DataFrame:
    game_datetime: datetime.datetime = datetime.datetime.strptime(game_date, '%Y-%m-%d')
    end_date: str = (game_datetime - datetime.timedelta(days=1)).strftime('%Y-%m-%d')  # Exclude game day
    start_date: str = (game_datetime - datetime.timedelta(days=days)).strftime('%Y-%m-%d')

    schedule_url: str = f'{MLB_API_URL}/schedule?sportId=1&startDate={start_date}&endDate={end_date}'
    response: dict[str, Any] = requests.get(schedule_url).json()

    bullpen_usage: list[dict[str, Any]] = []

    for date in response.get('dates', []):
        for game in date.get('games', []):
            for team_role in ['home', 'away']:
                team: dict[str, Any] = game['teams'][team_role]
                if team['team']['id'] == team_id:
                    boxscore_url: str = f'{MLB_API_URL}/game/{game["gamePk"]}/boxscore'
                    boxscore_response: requests.Response = requests.get(boxscore_url)

                    if boxscore_response.status_code != 200:
                        print(f'Warning: No data for game {game["gamePk"]} on {date["date"]}')
                        continue

                    boxscore: dict[str, Any] = boxscore_response.json()
                    team_box: dict[str, Any] = boxscore.get('teams', {}).get(team_role, {})

                    # Extract relief pitchers (ignore starters)
                    for pitcher_id in team_box.get('pitchers', []):
                        player_data: dict[str, Any] = team_box['players'].get(f'ID{pitcher_id}', {})
                        if 'stats' in player_data and 'pitching' in player_data['stats']:
                            stats: dict[str, Any] = player_data['stats']['pitching']
                            innings_pitched: float = float(stats.get('inningsPitched', 0))
                            pitch_count: int = int(stats.get('numberOfPitches', 0))

                            if innings_pitched > 0:
                                bullpen_usage.append({
                                    'Reliever': player_data['person']['fullName'],
                                    'Game Date': date['date'],
                                    'Innings': innings_pitched,
                                    'Pitch Count': pitch_count
                                })

    return pd.DataFrame(bullpen_usage)


def compute_bullpen_probabilities(bullpen_df: pd.DataFrame, game_date: str, decay_factor: float = 0.1) -> pd.DataFrame:
    """
    Computes the probability of each reliever being used based on:
    - Past usage trends (time-decayed pitch count)
    - Rest days since last outing
    - Frequency of appearances
    - Fatigue & recovery

    Decay Factor: Controls how fast past pitch counts lose influence.
    """
    if bullpen_df.empty:
        return pd.DataFrame(columns=['Reliever', 'Probability'])

    # Convert Game Date to datetime
    bullpen_df['Game Date'] = pd.to_datetime(bullpen_df['Game Date'])
    game_datetime: pd.Timestamp = pd.Timestamp(datetime.datetime.strptime(game_date, '%Y-%m-%d'))

    reliever_probs: dict[str, float] = {}

    for reliever, group in bullpen_df.groupby('Reliever'):
        total_games: int = group.shape[0]

        if total_games < 2:
            continue  # Ignore pitchers with too little data

        # Compute time-decayed pitch count using exponential decay
        group = group.copy()
        group['Days Ago'] = (game_datetime - group['Game Date']).dt.days  # type: ignore
        group['Decay Weight'] = np.exp(-decay_factor * group['Days Ago'])
        group['Weighted Pitches'] = group['Pitch Count'] * group['Decay Weight']

        if (last_pitched := group['Days Ago'].min()) > 14:
            print(f'Warning: {reliever} has not pitched in {last_pitched} days.')
            continue

        # Total decayed pitch count (recent outings matter more)
        time_decayed_pitch_count: float = float(group['Weighted Pitches'].sum())

        # Frequency of use
        freq_of_use: float = total_games / bullpen_df['Game Date'].nunique()

        # Fatigue penalty based on decayed pitch count
        fatigue_penalty: float = 1.0 - min(time_decayed_pitch_count / 80, 1.0)  # Scales with past workload

        # Compute probability based on frequency & fatigue
        probability: float = freq_of_use * fatigue_penalty

        reliever_probs[str(reliever)] = probability

    # Normalize probabilities to sum to 1
    total_prob: float = sum(reliever_probs.values())

    if total_prob > 0:
        for reliever in reliever_probs:
            reliever_probs[reliever] = round(reliever_probs[reliever] / total_prob, 3)

    # Return sorted DataFrame
    return pd.DataFrame(
        sorted(reliever_probs.items(), key=lambda x: x[1], reverse=True),  # type: ignore
        columns=['Reliever', 'Probability']
    )


def get_likely_pitchers(team_name: str, game_date: str) -> pd.DataFrame:
    team_id: int = get_team_id(team_name)
    bullpen_df: pd.DataFrame = get_bullpen_usage(team_id, game_date, days=30)
    likely_pitchers: pd.DataFrame = compute_bullpen_probabilities(bullpen_df, game_date)
    return likely_pitchers


if __name__ == '__main__':
    # Example Usage
    team: str = 'Reds'
    date: str = '2024-08-13'
    likely_pitchers: pd.DataFrame = get_likely_pitchers(team, date)
    print(likely_pitchers)

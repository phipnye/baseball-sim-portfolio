import os
import time
import pandas as pd
import urllib.request
from bs4 import BeautifulSoup
from googlesearch import search, SearchResult  # type: ignore
from tqdm import tqdm
from typing import Optional


def google_search(query: str) -> Optional[list[str]]:
    """Perform a Google search and return a list of URLs from the first page of results."""
    try:
        search_results: list[SearchResult] = search(query, sleep_interval=5)
        baseball_ref_results: list[str] = [url for url in search_results if 'baseball-reference.com' in url]

    except Exception:
        return None

    return baseball_ref_results


def get_player_name_from_page(url: str) -> str:
    """Fetch the player's name from a Baseball Reference page."""
    headers: dict[str, str] = {'User-Agent': 'Mozilla/5.0'}

    try:
        req: urllib.request.Request = urllib.request.Request(url, headers=headers)
        with urllib.request.urlopen(req, timeout=5) as response:
            html: str = response.read().decode('utf-8')

        soup: BeautifulSoup = BeautifulSoup(html, 'html.parser')
        h1_tag = soup.find('h1')

        return h1_tag.find('span').text.strip() if h1_tag and h1_tag.find('span') else 'Not found'  # type: ignore

    except Exception:
        return 'Not found'


def open_baseball_reference(player_name: str) -> Optional[str]:
    """Find the Baseball Reference page for the player and extract their name."""
    query: str = f'{player_name} pitcher'

    urls: Optional[list[str]] = google_search(query)

    if urls is None:
        return None
    elif len(urls):
        return get_player_name_from_page(urls[0])

    return 'Not found'


def main(df: pd.DataFrame) -> pd.DataFrame:
    """Process the dataframe, correcting pitcher names if missing."""
    df_updated: pd.DataFrame = df.copy()
    prog_bar: tqdm = tqdm(df.iterrows(), total=df.shape[0])

    for idx, row in prog_bar:
        pitcher: str = row['pitcher']
        bat_hand: str = row['bat_hand']

        prog_bar.set_description(pitcher)

        if not pd.isna(bat_hand):
            continue

        corrected_pitcher: Optional[str]
        if 'corrected_pitcher' in df_updated.columns and \
                (~pd.isna(df_updated.loc[df_updated['pitcher'] == pitcher, 'corrected_pitcher'])).any():
            corrected_pitcher = df_updated.loc[df_updated['pitcher'] == pitcher, 'corrected_pitcher'].dropna().values[0]
        else:
            corrected_pitcher = open_baseball_reference(pitcher)
            time.sleep(3)  # Avoid triggering Google's rate limits

            if corrected_pitcher is None:
                print('Terminating early due to failed Google seach.')
                break

        df_updated.at[idx, 'corrected_pitcher'] = corrected_pitcher

    return df_updated


if __name__ == '__main__':
    os.chdir(os.path.dirname(os.path.dirname(__file__)))
    assert os.getcwd() == '/home/philip/Documents/R projects/baseball'
    df: pd.DataFrame = pd.read_csv(os.path.join('raw', 'switch_hitter_hands.csv'))
    df_updated: pd.DataFrame = main(df)
    df_updated.to_csv(os.path.join('raw', 'switch_hitter_hands.csv'), index=False)

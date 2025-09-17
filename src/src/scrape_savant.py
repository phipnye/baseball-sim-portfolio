import os
import re
import torch
import pandas as pd
from bs4 import BeautifulSoup
from multiprocessing import Pool
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.remote.webelement import WebElement
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from sentence_transformers import SentenceTransformer, util
from time import sleep
from typing import Optional
from tqdm import tqdm
from unidecode import unidecode

SERVICE: Service = Service('/usr/bin/chromedriver')
SBERT: SentenceTransformer = SentenceTransformer('all-MiniLM-L6-v2')  # Lightweight but effective pre-trained model


def find_best_match_sbert(name: str, name_list: list[str], threshold: float = 0.9) -> Optional[str]:
    """
    Finds the best match for a given name in a list of names using SBERT embeddings.

    Args:
    - name (str): The input name to search for.
    - name_list (list of str): List of candidate names.
    - threshold (float): Minimum confidence score (0 to 1) to accept a match.

    Returns:
    - (str): The best-matching name or None if no match is found.
    """

    # Convert strings to unicode
    name = unidecode(name)
    name_list_unicode: list[str] = list(map(unidecode, name_list))

    # Encode both the target name and the list of names
    embeddings: torch.Tensor = SBERT.encode([name] + name_list_unicode, convert_to_tensor=True)

    # Compute cosine similarity between the target name and all list names
    similarities: torch.Tensor = util.pytorch_cos_sim(embeddings[0], embeddings[1:])[0]

    # Get the best match
    best_idx: int = torch.argmax(similarities).item()  # type: ignore
    best_score: float = similarities[best_idx].item()

    # Return the best match if above the threshold, else return None
    return name_list[best_idx] if best_score >= threshold else None


def partition_dataframe(df: pd.DataFrame, miss_col: str, sort_cols: str | list[str], n: int) -> list[pd.DataFrame]:
    # Sort the data by the sort column first
    df = df.sort_values(by=sort_cols, inplace=False)

    # Separate rows with and without missing values
    df_missing: pd.DataFrame = df[df[miss_col].isna()]
    df_non_missing: pd.DataFrame = df[~df[miss_col].isna()]

    # Determine how many missing values should go into each partition
    missing_per_partition: int = len(df_missing) // n
    remainder: int = len(df_missing) % n  # Handle extra missing values

    # Distribute missing and non-missing rows across partitions
    partitions: list[pd.DataFrame] = []
    start_missing: int = 0
    start_non_missing: int = 0

    for i in range(n):
        extra: int = 1 if i < remainder else 0  # Distribute remainder
        end_missing: int = start_missing + missing_per_partition + extra
        missing_part: pd.DataFrame = df_missing.iloc[start_missing:end_missing]

        # Approximate size of the partition
        size: int = len(df) // n + (1 if i < len(df) % n else 0)
        end_non_missing: int = start_non_missing + (size - len(missing_part))
        non_missing_part: pd.DataFrame = df_non_missing.iloc[start_non_missing:end_non_missing]

        # Combine and shuffle
        partition: pd.DataFrame = pd.concat([missing_part, non_missing_part]).reset_index(drop=True)
        partitions.append(partition)

        start_missing = end_missing
        start_non_missing = end_non_missing

    return partitions


def scrape(args: tuple[int, str, pd.DataFrame]) -> tuple[list[str], list[str], pd.DataFrame]:
    core_num: int
    base_url: str
    split_df: pd.DataFrame
    core_num, base_url, split_df = args

    sleep(5 * (core_num + 1))  # Allow five seconds between each initialization of webdriver

    driver: webdriver.Chrome = webdriver.Chrome(service=SERVICE)
    wait: WebDriverWait = WebDriverWait(driver, 5)
    bat_hand_df: pd.DataFrame = split_df.copy()
    prog_bar: tqdm = tqdm(bat_hand_df.iterrows(), total=bat_hand_df.shape[0])
    not_found_batters: list[str] = []
    not_found_pitchers: list[str] = []

    for idx, row in prog_bar:
        try:
            batter: str = row['batter']
            pitcher: str = row['pitcher']
            prog_bar.set_description(f'Batter: {batter} - Pitcher: {pitcher}')

            if 'bat_hand' in bat_hand_df.columns and not pd.isna(bat_hand_df.at[idx, 'bat_hand']):
                continue

            if batter in not_found_batters or pitcher in not_found_pitchers:
                continue

            driver.get(base_url)

            # Wait for search bar to load and interact with it
            search_bar: WebElement = wait.until(EC.element_to_be_clickable((By.ID, 'search-complete')))
            search_bar.click()
            sleep(1.5)
            search_bar.send_keys(batter)
            sleep(1.5)
            search_bar.send_keys(Keys.RETURN)
            sleep(18)

            # Wait for the page to load by checking if URL has changed
            if driver.current_url == base_url:
                not_found_batters.append(batter)
                continue

            # Click the correct "YEAR (...)" dropdown arrow
            dropdown_containers: list[WebElement] = wait.until(
                EC.visibility_of_all_elements_located((By.CLASS_NAME, 'form-option'))
            )

            for container in dropdown_containers:
                try:
                    label: WebElement = container.find_element(By.CLASS_NAME, 'ddl-label')
                    if 'YEAR (' in label.text.upper():
                        years_dropdown_arrow: WebElement = container.find_element(By.CLASS_NAME, 'dropdown-arrow')
                        wait.until(EC.element_to_be_clickable(years_dropdown_arrow)).click()
                        sleep(1.5)
                        break
                except Exception:
                    continue

            # Wait for year options to load
            year_options: list[WebElement] = wait.until(EC.visibility_of_all_elements_located(
                (By.XPATH, "//div[@class='multipick']//input[@type='checkbox']")
            ))

            # Select all year options
            year_opt_ind: int = 0
            while not all(year_opt.is_selected() for year_opt in year_options):
                if not year_options[year_opt_ind].is_selected():
                    year_options[year_opt_ind].click()
                    sleep(0.5)
                    year_options = wait.until(EC.presence_of_all_elements_located(
                        (By.XPATH, "//div[@class='multipick']//input[@type='checkbox']")
                    ))  # Refresh the list
                else:
                    year_opt_ind += 1

            # Click the correct "SEASON TYPE" dropdown arrow
            dropdown_containers = wait.until(
                EC.visibility_of_all_elements_located((By.CLASS_NAME, 'form-option'))
            )

            for container in dropdown_containers:
                try:
                    label = container.find_element(By.CLASS_NAME, 'ddl-label')
                    if 'SEASON TYPE (' in label.text.upper():
                        ssn_type_dropdown: WebElement = container.find_element(By.CLASS_NAME, 'dropdown-arrow')
                        wait.until(EC.element_to_be_clickable(ssn_type_dropdown)).click()
                        sleep(1.5)
                        break
                except Exception:
                    continue

            # Wait for season type options to load
            season_type_options: list[WebElement] = wait.until(EC.visibility_of_all_elements_located(
                (By.XPATH, "//div[@class='multipick']//input[@type='checkbox']")
            ))

            # Select all season type options
            season_type_opt_ind: int = 0
            while not all(season_type_opt.is_selected() for season_type_opt in season_type_options):
                if not season_type_options[season_type_opt_ind].is_selected():
                    season_type_options[season_type_opt_ind].click()
                    sleep(0.5)
                    season_type_options = wait.until(EC.presence_of_all_elements_located(
                        (By.XPATH, "//div[@class='multipick']//input[@type='checkbox']")
                    ))  # Refresh the list
                else:
                    season_type_opt_ind += 1

            # Click the "FACING PLAYER" dropdown arrow
            dropdown_containers = wait.until(EC.visibility_of_all_elements_located((By.CLASS_NAME, 'form-option')))
            for container in dropdown_containers:
                try:
                    label = container.find_element(By.CLASS_NAME, 'ddl-label')
                    if 'FACING PLAYER' in label.text.upper():
                        pitcher_dropdown_arrow: WebElement = container.find_element(By.CLASS_NAME, 'dropdown-arrow')
                        pitcher_dropdown_arrow.click()
                        sleep(1.5)  # Allow dropdown to open
                        break
                except Exception:
                    continue

            # Select the desired pitcher
            pitcher_name: str = row['pitcher_last'] + (' Jr.' if re.search(r'\sJr\.?$', pitcher) else '')
            pitcher_name += (', ' + pitcher.removesuffix(pitcher_name)).rstrip()

            try:
                pitcher_label_element: WebElement = wait.until(EC.visibility_of_element_located(
                    (By.XPATH, f"//div[@class='multipick']//label[text()='{pitcher_name}']")
                ))
                pitcher_checkbox_element: WebElement = pitcher_label_element.find_element(
                    By.XPATH, './preceding-sibling::input'
                )

                if not pitcher_checkbox_element.is_selected():
                    pitcher_checkbox_element.click()
                    sleep(1.5)  # Allow selection to register

            except Exception:
                dropdown_containers = wait.until(EC.visibility_of_all_elements_located((By.CLASS_NAME, 'form-option')))
                pitcher_dropdown_html: Optional[str] = None

                for container in dropdown_containers:
                    try:
                        label = container.find_element(By.CLASS_NAME, 'ddl-label')
                        if 'FACING PLAYER' in label.text.upper():
                            # Obtain html for the pitcher dropdown
                            pitcher_dropdown_html = container.get_attribute('outerHTML')
                            break

                    except Exception:
                        continue

                # Extract all player names from the dropdown
                if pitcher_dropdown_html is not None:
                    pitchers_soup: BeautifulSoup = BeautifulSoup(pitcher_dropdown_html, 'html.parser')  # type: ignore
                    pitcher_opts: list[str] = [label.text.strip() for label in pitchers_soup.select('.multipick label')]
                    pitcher_opts = [
                        player for player in pitcher_opts if unidecode(row['pitcher_last']) in unidecode(player)
                    ]

                    if len(pitcher_opts) == 1:
                        pitcher_name = pitcher_opts[0]

                        try:
                            pitcher_label_element = wait.until(EC.visibility_of_element_located(
                                (By.XPATH, f"//div[@class='multipick']//label[text()='{pitcher_name}']")
                            ))
                            pitcher_checkbox_element = pitcher_label_element.find_element(
                                By.XPATH, './preceding-sibling::input'
                            )

                            if not pitcher_checkbox_element.is_selected():
                                pitcher_checkbox_element.click()
                                sleep(1.5)  # Allow selection to register

                            bat_hand_df.at[idx, 'corrected_pitcher_2'] = ' '.join(reversed(pitcher_name.split(', ')))

                        except Exception:
                            not_found_pitchers.append(pitcher)
                            continue

                    elif len(pitcher_opts) > 1:
                        sbert_match: Optional[str] = find_best_match_sbert(pitcher_name, pitcher_opts)

                        if sbert_match is not None:
                            pitcher_name = sbert_match

                            try:
                                pitcher_label_element = wait.until(EC.visibility_of_element_located(
                                    (By.XPATH, f"//div[@class='multipick']//label[text()='{pitcher_name}']")
                                ))
                                pitcher_checkbox_element = pitcher_label_element.find_element(
                                    By.XPATH, './preceding-sibling::input'
                                )

                                if not pitcher_checkbox_element.is_selected():
                                    pitcher_checkbox_element.click()
                                    sleep(1.5)  # Allow selection to register

                                bat_hand_df.at[idx, 'corrected_pitcher_2'] = pitcher_name

                            except Exception:
                                not_found_pitchers.append(pitcher)
                                continue

                    else:
                        not_found_pitchers.append(pitcher)
                        continue

                else:
                    not_found_pitchers.append(pitcher)
                    continue

            # Capture the stance the batter takes more frequently
            n_pa: dict[str, int] = {}
            for batter_stance in ('Left', 'Right'):
                dropdown_containers = wait.until(EC.visibility_of_all_elements_located((By.CLASS_NAME, 'form-option')))

                for container in dropdown_containers:
                    try:
                        label = container.find_element(By.CLASS_NAME, 'ddl-label')
                        if 'BATTER STANDS' in label.text.upper():
                            option_element: WebElement = container.find_element(
                                By.XPATH,
                                f".//select/option[@value='{batter_stance[0]}']"
                            )
                            option_element.click()
                            sleep(1.5)
                            break
                    except Exception:
                        continue

                soup: BeautifulSoup = BeautifulSoup(driver.page_source, 'html.parser')
                n_pa_element = soup.find('div', class_='stats').find_all('div', class_='stats-value')[1]  # type: ignore
                n_pa[batter_stance] = int(n_pa_element.text)

            if n_pa['Left'] == n_pa['Right']:
                bat_hand_df.at[idx, 'bat_hand'] = 'Left/Right'
            elif n_pa['Left'] > n_pa['Right']:
                bat_hand_df.at[idx, 'bat_hand'] = 'Left'
            else:
                bat_hand_df.at[idx, 'bat_hand'] = 'Right'

        except Exception:
            continue

    return not_found_batters, not_found_pitchers, bat_hand_df


def main(base_url: str, switch_hitters: pd.DataFrame, n_cores: int = 5) -> pd.DataFrame:
    # Split the process across multiple processors (evenly distributing the number of missing 'bat_hand' per dataframe)
    split_dfs: list[pd.DataFrame] = partition_dataframe(
        switch_hitters,
        'bat_hand',
        sort_cols=['pitcher', 'batter'],  # sort by pitcher then batter, want to capitalize on players not found
        n=n_cores
    )

    # Group the arguments with the base url
    arg_pairs: list[tuple[int, str, pd.DataFrame]] = [(i, base_url, split_dfs[i]) for i in range(n_cores)]

    with Pool(n_cores) as pool:
        scraping_results: list[tuple[list[str], list[str], pd.DataFrame]] = pool.map(scrape, arg_pairs)

    not_found_batters: list[str] = []
    not_found_pitchers: list[str] = []
    bat_hand_dfs: list[pd.DataFrame] = []

    for (nf_batters, nf_pitchers, bh_df) in scraping_results:
        not_found_batters.extend(nf_batters)
        not_found_pitchers.extend(nf_pitchers)
        bat_hand_dfs.append(bh_df)

    if len(not_found_batters):
        not_found_batters_str: str = '\n'.join(sorted(set(not_found_batters)))
        print(f'WARNING: The following batters were not found: {not_found_batters_str}')

    if len(not_found_pitchers):
        not_found_pitchers_str: str = '\n'.join(sorted(set(not_found_pitchers)))
        print(f'WARNING: The following pitchers were not found: {not_found_pitchers_str}')

    bat_hand_df: pd.DataFrame = pd.concat(bat_hand_dfs, ignore_index=True)
    return bat_hand_df


if __name__ == '__main__':
    os.chdir(os.path.dirname(os.path.dirname(__file__)))
    assert os.getcwd() == '/home/philip/Documents/R projects/baseball', f'Wrong WD: {os.getcwd()}'

    # Read in data
    switch_hitters: pd.DataFrame = pd.read_csv(os.path.join('raw', 'switch_hitter_hands.csv'))
    switch_hitters.drop_duplicates(['batter_id', 'pitcher_id'], inplace=True, ignore_index=True)
    base_url: str = 'https://baseballsavant.mlb.com/illustrator'

    # Determine which hand the batter used most frequently against a given pitcher
    bat_hand_df: pd.DataFrame = main(base_url, switch_hitters, n_cores=4)
    bat_hand_df.to_csv(os.path.join('raw', 'switch_hitter_hands.csv'), index=False)

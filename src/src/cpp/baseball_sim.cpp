#include <Rcpp.h>
#include <torch/script.h>
using namespace Rcpp;

// ----------------------------------------------------------------------------
// Helper functions
// ----------------------------------------------------------------------------
// Single-Sample Dirichlet
NumericVector rdirichlet_one(const NumericVector &alpha) {
  const int K = alpha.size();
  NumericVector out(K);
  double sum_term = 0.0;

  for (int j = 0; j < K; ++j) {
    double cur = R::rgamma(alpha[j], 1.0);
    out[j] = cur;
    sum_term += cur;
  }

  out = out / sum_term;
  return out;
}

std::string remove_prefix(const std::string &str, const std::string &prefix) {
  return str.substr(prefix.length());
}

std::string remove_after(const std::string &str, char delimiter) {
  size_t pos = str.find(delimiter);
  return (pos != std::string::npos) ? str.substr(0, pos) : str;
}

int sample_index(const NumericVector &probs) {
  IntegerVector indices = seq_along(probs) - 1;
  IntegerVector idx = sample(indices, 1, false, probs);
  return idx[0];
}

// Split a string based on a character delimited
std::vector<std::string> strsplit(const std::string &str, const char delimiter) {
  int delim_count = std::count(str.begin(), str.end(), delimiter);
  std::vector<std::string> tokens;
  tokens.reserve(delim_count + 1);  // Pre-allocate space

  size_t start = 0, end;
  while ((end = str.find(delimiter, start)) != std::string::npos) {
    tokens.push_back(str.substr(start, end - start));
    start = end + 1;
  }

  tokens.push_back(str.substr(start));  // Last token
  return tokens;
}

// -----------------------------------------------------------------------------
// Determine Base Conditional
// -----------------------------------------------------------------------------
std::string determine_base_conditional(const bool (&bases)[3]) {
  if (!bases[0] && !bases[1] && !bases[2]) return "pno";

  std::ostringstream base_cond;
  for (int i = 0; i < 3; ++i) {
    if (bases[i]) base_cond << "p" << (i + 1);
  }

  return base_cond.str();
}

// -----------------------------------------------------------------------------
// Get the Row Index for the Given Base Condition + Batter Index + Pitcher Index
// -----------------------------------------------------------------------------
int get_batter_row(const DataFrame &POSTERIOR, const std::string &base_cond, const int batter_idx,
                   const int pitcher_idx) {
  CharacterVector bc = POSTERIOR["base_cond"];
  IntegerVector bo = POSTERIOR["batter_order"];
  IntegerVector po = POSTERIOR["pitcher_order"];

  for (int i = 0; i < POSTERIOR.nrow(); ++i) {
    if (bc[i] == base_cond && bo[i] == batter_idx && po[i] == pitcher_idx) return i;
  }

  stop("Batter index out of range or no matching rows found for base_cond.");
}

// -----------------------------------------------------------------------------
// Filter out zero/NA alphas for the batter
// -----------------------------------------------------------------------------
NumericVector get_event_alphas(const DataFrame &POSTERIOR, const int batter_row_idx,
                               const std::vector<std::string> &alpha_cols) {
  int n_alphas = alpha_cols.size();
  std::vector<double> event_alphas_vec;
  std::vector<std::string> event_names_vec;
  event_alphas_vec.reserve(n_alphas);
  event_names_vec.reserve(n_alphas);

  for (int i = 0; i < n_alphas; ++i) {
    NumericVector col = POSTERIOR[alpha_cols[i]];
    double value = col[batter_row_idx];
    if (value != 0.0 && !NumericVector::is_na(value)) {
      event_alphas_vec.push_back(value);
      event_names_vec.push_back(alpha_cols[i]);
    }
  }

  NumericVector event_alphas(event_alphas_vec.begin(), event_alphas_vec.end());
  event_alphas.names() = event_names_vec;
  return event_alphas;
}

// -----------------------------------------------------------------------------
// Check for columns that match the event and base conditional
// -----------------------------------------------------------------------------
bool matches_pattern(const std::string &col_name, const std::string &event, const std::string &base_cond) {
  std::vector<std::string> col_split = strsplit(col_name, '_');
  return col_split.size() == 3 && (col_split[1] == event && col_split[2] == base_cond);
}

// -----------------------------------------------------------------------------
// Process runner advancement, update bases/scores/outs
// -----------------------------------------------------------------------------
bool process_run_adv(const std::string &run_adv, bool (&bases)[3], int (&scores)[2], float (&runs_allowed)[2],
                     int &n_outs, const int team_idx) {
  std::string clean_adv = remove_after(remove_prefix(run_adv, "alpha."), '_');
  size_t adv_pos = clean_adv.find("adv");
  size_t out_pos = clean_adv.find("out");

  std::string advs = (adv_pos != std::string::npos) ? clean_adv.substr(adv_pos) : "";
  std::string outs = (out_pos != std::string::npos) ? clean_adv.substr(out_pos) : "";

  if (advs.empty() && outs.empty()) {
    stop("Both advancements and outs are missing.");
  }

  // Process advancements
  if (!advs.empty()) {
    std::string adv_details = advs.substr(3);
    std::vector<std::string> all_advs = strsplit(adv_details, '.');

    std::vector<std::vector<std::string>> on_base(3);
    std::vector<std::vector<std::string>> off_base(3);

    for (const std::string &adv : all_advs) {
      if (adv.size() < 2) continue;
      char from_base = adv[0];
      char to_base = adv[1];
      if (!std::isdigit(from_base) || !std::isdigit(to_base)) continue;

      if (to_base == '4') {
        ++scores[team_idx];
        ++runs_allowed[team_idx];
      } else {
        int b = to_base - '1';
        on_base[b].push_back(adv);
      }
      if (from_base != '0') {
        int b = from_base - '1';
        off_base[b].push_back(adv);
      }
    }
    // Check conflicts
    for (int i = 0; i < 3; ++i) {
      if (on_base[i].size() > 1 || off_base[i].size() > 1) {
        stop("More than one runner advancing to/from the same base.");
      }
    }
    // Update bases
    for (int i = 0; i < 3; ++i) {
      bases[i] = (!on_base[i].empty() || (off_base[i].empty() && bases[i]));
    }
  }

  // Process outs
  if (!outs.empty()) {
    std::string out_details = outs.substr(3);
    std::vector<std::string> all_outs = strsplit(out_details, '.');
    n_outs += all_outs.size();
  }

  // Return whether to advance the batter
  return (clean_adv.find("adv0") != std::string::npos || clean_adv.find("out0") != std::string::npos);
}

// -----------------------------------------------------------------------------
// Sample runner advancement
// -----------------------------------------------------------------------------
std::string sample_run_adv(const DataFrame &POSTERIOR, const DataFrame &RUN_ADV, const std::string &event,
                           const std::string &base_cond, const int batter_row_idx) {
  // Collect relevant columns in RUN_ADV
  std::vector<std::string> run_adv_names = RUN_ADV.names();
  std::vector<std::string> adv_cols;
  adv_cols.reserve(run_adv_names.size());

  for (int i = 0; i < (int)run_adv_names.size(); ++i) {
    const std::string &col_name = run_adv_names[i];
    if (matches_pattern(col_name, event, base_cond)) {
      adv_cols.push_back(col_name);
    }
  }

  // Retrieve some fields from POSTERIOR
  LogicalVector at_home_vec = POSTERIOR["at_home"];
  CharacterVector bat_hand_vec = POSTERIOR["bat_hand"];
  CharacterVector pit_hand_vec = POSTERIOR["pit_hand"];

  bool ah = at_home_vec[batter_row_idx];
  std::string bh = as<std::string>(bat_hand_vec[batter_row_idx]);
  std::string ph = as<std::string>(pit_hand_vec[batter_row_idx]);

  if (adv_cols.empty()) {
    std::string msg = "No columns match: event=" + event + ", base_cond=" + base_cond +
                      ", home=" + std::to_string(ah) + ", bh=" + bh + ", ph=" + ph;
    stop(msg);
  }

  // Identify matching row in RUN_ADV
  LogicalVector run_at_home = RUN_ADV["at_home"];
  CharacterVector run_bh_vec = RUN_ADV["bat_hand"];
  CharacterVector run_ph_vec = RUN_ADV["pit_hand"];

  int row_idx = -1;
  for (int i = 0; i < RUN_ADV.nrow(); ++i) {
    if (run_at_home[i] == ah && run_bh_vec[i] == bh && run_ph_vec[i] == ph) {
      row_idx = i;
      break;
    }
  }
  if (row_idx < 0) {
    stop("No row found in RUN_ADV matching the given conditions.");
  }

  // Build alpha vector for adv columns
  NumericVector run_adv_alphas(adv_cols.size());
  for (int i = 0; i < adv_cols.size(); ++i) {
    NumericVector col_data = RUN_ADV[adv_cols[i]];
    run_adv_alphas[i] = col_data[row_idx];
  }

  // Dirichlet sample
  NumericVector run_adv_probs = rdirichlet_one(run_adv_alphas);

  // Sample one column by discrete distribution
  int chosen_idx = sample_index(run_adv_probs);
  return adv_cols[chosen_idx];
}

// -----------------------------------------------------------------------------
// Predict pitcher changes
// -----------------------------------------------------------------------------

bool lefty_batter_next(const DataFrame &POSTERIOR, const std::string &base_cond, const int batter_idx,
                       const int pitcher_idx) {
  int next_batter_idx;

  if (batter_idx == 8) {
    next_batter_idx = batter_idx - 8;
  } else {
    next_batter_idx = batter_idx + 1;
  }

  // Match on base conditional and batter/pitcher indices
  CharacterVector bc = POSTERIOR["base_cond"];
  IntegerVector bo = POSTERIOR["batter_order"];
  IntegerVector po = POSTERIOR["pitcher_order"];
  int i = 0;
  while (bc[i] != base_cond || bo[i] != next_batter_idx || po[i] != pitcher_idx) {
    ++i;
  }

  CharacterVector bat_hand = POSTERIOR["bat_hand"];
  return bat_hand[i] == "L";
}

bool is_lefty_pitcher(const DataFrame &POSTERIOR, const int batter_row_idx) {
  CharacterVector pit_hand = POSTERIOR["pit_hand"];
  return pit_hand[batter_row_idx] == "L";
}

bool int_in_vec(const int value, const IntegerVector &vec) {
  // Use unordered_set for fast lookup
  std::unordered_set<int> values_set(vec.begin(), vec.end());

  // Check if value exists in the set
  return values_set.find(value) != values_set.end();
}

// Determine whether a given pitcher is a closer
float is_closer(const DataFrame &POSTERIOR, const int pitcher_idx) {
  IntegerVector pitcher_indices = POSTERIOR["pitcher_order"];
  CharacterVector pitcher_types = POSTERIOR["pitcher_type"];

  std::string pitcher_type;
  for (int i = 0; i < POSTERIOR.nrow(); ++i) {
    if (pitcher_indices[i] == pitcher_idx) {
      pitcher_type = pitcher_types[i];
      break;
    }
  }

  return static_cast<float>(pitcher_type == "closer");
}

int pitcher_available(const DataFrame &POSTERIOR, const int pitcher_idx, const int inning,
                      const IntegerVector &used_pitcher_indices) {
  IntegerVector pitcher_indices = POSTERIOR["pitcher_order"];
  CharacterVector pitcher_types = POSTERIOR["pitcher_type"];

  IntegerVector all_pitcher_indices = seq_len(max(pitcher_indices));
  IntegerVector next_pitcher_indices = setdiff(all_pitcher_indices, used_pitcher_indices);
  std::string look_type = inning < 9 || inning > 10 ? "reliever" : "closer";

  for (int i = 0; i < POSTERIOR.nrow(); ++i) {
    int curr_idx = pitcher_indices[i];
    if (int_in_vec(curr_idx, next_pitcher_indices) && pitcher_types[i] == look_type) {
      return curr_idx;
    }
  }

  return -1;
}

// Sample the number of pitches for a given play event
int sample_pitches(const DataFrame &PIT_DIST, const DataFrame &POSTERIOR, int batter_row_idx,
                   const std::string &event) {
  // Retrieve relevant columns from POSTERIOR
  LogicalVector at_home_vec = POSTERIOR["at_home"];
  CharacterVector bat_hand_vec = POSTERIOR["bat_hand"];
  CharacterVector pit_hand_vec = POSTERIOR["pit_hand"];

  bool at_home = at_home_vec[batter_row_idx];
  std::string bat_hand = as<std::string>(bat_hand_vec[batter_row_idx]);
  std::string pit_hand = as<std::string>(pit_hand_vec[batter_row_idx]);

  // Extract columns from PIT_DIST
  LogicalVector pit_at_home = PIT_DIST["at_home"];
  CharacterVector pit_bat_hand = PIT_DIST["bat_hand"];
  CharacterVector pit_pit_hand = PIT_DIST["pit_hand"];
  CharacterVector pit_event = PIT_DIST["event"];

  // Find matching row
  int row_idx = -1;
  for (int i = 0; i < PIT_DIST.nrows(); ++i) {
    if (pit_at_home[i] == at_home && pit_bat_hand[i] == bat_hand && pit_pit_hand[i] == pit_hand &&
        pit_event[i] == event) {
      row_idx = i;
      break;
    }
  }

  // Error handling
  if (row_idx < 0) {
    stop("No matching row found in PIT_DIST for event: " + event);
  }

  // Extract pitch probabilities (columns 1-17)
  NumericVector pitch_probs(17);
  for (int i = 0; i < 17; ++i) {
    NumericVector col = PIT_DIST[i + 4];  // Offset for first 4 columns
    pitch_probs[i] = col[row_idx];
  }

  // Ensure probabilities sum to 1 (prevent floating point issues)
  pitch_probs = pitch_probs / sum(pitch_probs);

  // Sample a pitch count (1-17) using the probability distribution
  int sampled_cnt = sample_index(pitch_probs);
  return sampled_cnt + 1;
}

bool pitcher_change(const int inning, const bool at_home, const float new_inning, const float total_pitches,
                    const float runs_allowed, const int n_outs, const bool (&bases)[3],
                    const int (&scores)[2], const float starting_pitcher, const float closing_pitcher,
                    const bool lefty_pitcher, const float era, const float whip, const float strikeout_rate,
                    const float walk_rate, const bool left_batter_incoming,
                    torch::jit::script::Module &model) {
  // Prepare input values
  float input_values[20] = {
      static_cast<float>(inning),
      static_cast<float>(at_home),
      new_inning,
      total_pitches,
      runs_allowed,
      static_cast<float>(n_outs),
      static_cast<float>(bases[0]),
      static_cast<float>(bases[1]),
      static_cast<float>(bases[2]),
      static_cast<float>(scores[0]),
      static_cast<float>(scores[1]),
      starting_pitcher,
      static_cast<float>(starting_pitcher == 0.0f && closing_pitcher == 0.0f),  // relief pitcher
      closing_pitcher,
      static_cast<float>(lefty_pitcher),
      era,
      whip,
      strikeout_rate,
      walk_rate,
      static_cast<float>(left_batter_incoming)};

  // Convert array to a Torch tensor
  at::Tensor input_tensor = torch::from_blob(input_values, {1, 20}, at::TensorOptions().dtype(torch::kFloat));

  // Run inference with the preloaded model
  at::Tensor output_tensor = model.forward({input_tensor}).toTensor();

  // Extract probability
  float prob = output_tensor.item<float>();

  // Generate a random number and compare
  double rand_num = R::runif(0, 1);
  return rand_num < prob;
}

// -----------------------------------------------------------------------------
// 11) MAIN FUNCTION
// -----------------------------------------------------------------------------

// [[Rcpp::export]]
DataFrame sim_game_cpp(const ListOf<DataFrame> &POSTERIORs, const DataFrame &RUN_ADV,
                       const DataFrame &PIT_DIST, const std::string &model_path,
                       const std::vector<std::string> &alpha_cols, const int n_sim) {
  // Load the PyTorch model
  torch::jit::script::Module model;
  try {
    model = torch::jit::load(model_path);
    model.eval();
  } catch (const c10::Error &e) {
    stop("Error loading the PyTorch model: " + std::string(e.what()));
  }

  // Prepare result vectors
  IntegerVector away_scores(n_sim), home_scores(n_sim);

  // Function for setting seed
  Function set_seed("set.seed");

  // Game-state variables
  std::string teams[2] = {"away", "home"};

  // Event sets for incrementing pitcher predictors
  const std::unordered_set<std::string> WHIP_EVENTS = {
      "single",        "double",       "triple",       "hr",           "doubleplay.single",
      "walk",          "caught2.walk", "caught3.walk", "caught4.walk", "passedball.walk",
      "walk.wildpitch"};
  const std::unordered_set<std::string> STRIKEOUT_EVENTS = {
      "caught2.doubleplay.strikeout", "caught2.strikeout",
      "caught3.doubleplay.strikeout", "caught3.strikeout",
      "caught4.doubleplay.strikeout", "caught4.strikeout",
      "doubleplay.strikeout",         "erroradv.strikeout",
      "passedball.strikeout",         "strikeout",
      "strikeout.wildpitch"};
  const std::unordered_set<std::string> WALK_EVENTS = {"caught2.walk",    "caught3.walk", "caught4.walk",
                                                       "passedball.walk", "walk",         "walk.wildpitch"};

  for (int sim_i = 0; sim_i < n_sim; ++sim_i) {
    // Set random seed
    set_seed(sim_i + 1);

    // Game-state variables
    int batter_indices[2] = {0, 0};
    int pitcher_indices[2] = {0, 0};
    bool game_over = false;
    int inning = 1;
    int scores[2] = {0, 0};

    // Initialize variables that are needed for pitcher change determinations
    int batter_row_idx = 0;
    std::string base_cond = "pno";

    // Pitcher variables
    float closing_pitcher[2] = {0.0f, 0.0f};
    float n_pitches[2] = {0.0f, 0.0f};
    float n_plays[2] = {0.0f, 0.0f};
    float n_strikeouts[2] = {0.0f, 0.0f};
    float n_walks[2] = {0.0f, 0.0f};
    float runs_allowed[2] = {0.0f, 0.0f};
    float starting_pitcher[2] = {1.0f, 1.0f};
    std::vector<IntegerVector> used_pitcher_indices = {{0}, {0}};
    float whip_num[2] = {0.0f, 0.0f};

    while (inning < 10 || scores[0] == scores[1]) {
      for (const std::string &team : teams) {
        bool at_home = (team == "home");
        bool bases[3] = {false, false, false};
        float new_inning = 1.0f;
        int n_outs = 0;
        int team_idx = at_home ? 1 : 0;
        const DataFrame &POSTERIOR = POSTERIORs[team_idx];
        game_over = (inning > 8 && at_home && scores[0] < scores[1]);

        while (n_outs < 3 && !game_over) {
          // Determine if a pitcher change is needed
          if (n_plays[team_idx] > 0.0f) {
            // Determine the next pitcher index if a change is to occur
            int next_pitcher_idx = pitcher_available(POSTERIOR, pitcher_indices[team_idx], inning,
                                                     used_pitcher_indices[team_idx]);
            if (next_pitcher_idx != -1) {
              bool lefty_pitcher = is_lefty_pitcher(POSTERIOR, batter_row_idx);
              float era = (runs_allowed[team_idx] / inning) * 9;
              float whip = whip_num[team_idx] / inning;
              float strikeout_rate = n_strikeouts[team_idx] / n_plays[team_idx];
              float walk_rate = n_walks[team_idx] / n_plays[team_idx];
              bool left_batter_incoming = lefty_batter_next(POSTERIOR, base_cond, batter_indices[team_idx],
                                                            pitcher_indices[team_idx]);
              bool next_pitcher = pitcher_change(
                  inning, at_home, new_inning, n_pitches[team_idx], runs_allowed[team_idx], n_outs, bases,
                  scores, starting_pitcher[team_idx], closing_pitcher[team_idx], lefty_pitcher, era, whip,
                  strikeout_rate, walk_rate, left_batter_incoming, model);

              if (next_pitcher) {
                used_pitcher_indices[team_idx].push_back(next_pitcher_idx);
                pitcher_indices[team_idx] = next_pitcher_idx;
                n_strikeouts[team_idx] = 0.0f;
                n_walks[team_idx] = 0.0f;
                n_pitches[team_idx] = 0.0f;
                runs_allowed[team_idx] = 0.0f;
                whip_num[team_idx] = 0.0f;
                starting_pitcher[team_idx] = 0.0f;
                closing_pitcher[team_idx] = is_closer(POSTERIOR, next_pitcher_idx);
              }
            }
          }

          // Determine the batter that's up and the base conditional
          std::string base_cond = determine_base_conditional(bases);
          int batter_row_idx =
              get_batter_row(POSTERIOR, base_cond, batter_indices[team_idx], pitcher_indices[team_idx]);
          NumericVector event_alphas = get_event_alphas(POSTERIOR, batter_row_idx, alpha_cols);
          NumericVector event_posteriors = rdirichlet_one(event_alphas);
          int chosen_evt_idx = sample_index(event_posteriors);
          std::vector<std::string> evt_names = event_alphas.names();
          std::string event_full = evt_names[chosen_evt_idx];
          std::string event_name = remove_prefix(event_full, "alpha.");
          std::string run_adv = sample_run_adv(POSTERIOR, RUN_ADV, event_name, base_cond, batter_row_idx);

          // Update runner advancements
          bool next_batter = process_run_adv(run_adv, bases, scores, runs_allowed, n_outs, team_idx);
          if (next_batter) batter_indices[team_idx] = (batter_indices[team_idx] + 1) % 9;

          // Update pitcher variables
          int play_pitches = sample_pitches(PIT_DIST, POSTERIOR, batter_row_idx, event_name);
          n_pitches[team_idx] += play_pitches;
          ++n_plays[team_idx];
          new_inning = 0.0f;
          game_over = (inning > 8 && at_home && scores[0] < scores[1]);

          // Increment pitcher predictors upon specific events
          if (WHIP_EVENTS.count(event_name)) ++whip_num[team_idx];
          if (STRIKEOUT_EVENTS.count(event_name)) ++n_strikeouts[team_idx];
          if (WALK_EVENTS.count(event_name)) ++n_walks[team_idx];
        }
      }
      ++inning;
    }
    away_scores[sim_i] = scores[0];
    home_scores[sim_i] = scores[1];
  }

  DataFrame ALL_SCORES = DataFrame::create(_["away"] = away_scores, _["home"] = home_scores);
  return ALL_SCORES;
}

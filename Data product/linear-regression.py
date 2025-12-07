# ============================================
# Bike sharing hourly demand prediction - Linear Regression + User Input + Validation + Tips
# Bike sharing hourly demand prediction
# ============================================

import os
from math import sqrt
from datetime import datetime

import pandas as pd
import numpy as np

from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import OneHotEncoder, StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score


# ================================
# 1. 读取数据 / Load the dataset
# ================================

script_dir = os.path.dirname(os.path.abspath(__file__))
csv_path = os.path.join(script_dir, "bikehour.csv")

print("Script directory:", script_dir)
print("CSV path:", csv_path)

df = pd.read_csv(csv_path)

print("\nData preview:")
print(df.head())
print("Columns:", df.columns.tolist())

# ================================
# 2. Select features & target
# ================================
feature_cols = [
    "hr",
    "weekday",
    "workingday",
    "holiday",
    "mnth",
    "season",
    "yr",
    "weathersit",
    "temp",
    "atemp",
    "hum",
    "windspeed",
]
target_col = "cnt"

X = df[feature_cols].copy()
y = df[target_col].copy()

print("\nFeature columns:", feature_cols)

# ================================
# 3. Train-test split by year
# ================================
train_mask = df["yr"] == 0   # 2011
test_mask = df["yr"] == 1    # 2012

X_train = X[train_mask]
y_train = y[train_mask]
X_test = X[test_mask]
y_test = y[test_mask]

print("\nTrain shape:", X_train.shape)
print("Test shape:", X_test.shape)

# ================================
# 4. Preprocessing
# ================================
categorical_features = [
    "season",
    "yr",
    "mnth",
    "hr",
    "weekday",
    "workingday",
    "holiday",
    "weathersit",
]
numeric_features = ["temp", "atemp", "hum", "windspeed"]

categorical_transformer = OneHotEncoder(handle_unknown="ignore")
numeric_transformer = StandardScaler()

preprocessor = ColumnTransformer(
    transformers=[
        ("cat", categorical_transformer, categorical_features),
        ("num", numeric_transformer, numeric_features),
    ]
)

# ================================
# 5. Build pipeline & train
# ================================
# Linear Regression Model
model_lr = Pipeline(
    steps=[
        ("preprocess", preprocessor),
        ("regressor", LinearRegression()),
    ]
)

print("\nTraining Linear Regression model...")
model_lr.fit(X_train, y_train)
print("Training LR done.")

# Random Forest Model (Best Model)
model_rf = Pipeline(
    steps=[
        ("preprocess", preprocessor),
        ("regressor", RandomForestRegressor(n_estimators=100, random_state=42)),
    ]
)

print("\nTraining Random Forest model...")
model_rf.fit(X_train, y_train)
print("Training RF done.")

# ================================
# 6. Evaluation
# ================================
# Evaluate LR
y_pred_lr = model_lr.predict(X_test)
mae_lr = mean_absolute_error(y_test, y_pred_lr)
rmse_lr = sqrt(mean_squared_error(y_test, y_pred_lr))
r2_lr = r2_score(y_test, y_pred_lr)

# Evaluate RF
y_pred_rf = model_rf.predict(X_test)
mae_rf = mean_absolute_error(y_test, y_pred_rf)
rmse_rf = sqrt(mean_squared_error(y_test, y_pred_rf))
r2_rf = r2_score(y_test, y_pred_rf)

print("\n=== Test performance (Linear Regression) ===")
print(f"MAE: {mae_lr:.2f}, RMSE: {rmse_lr:.2f}, R^2: {r2_lr:.3f}")

print("\n=== Test performance (Random Forest) ===")
print(f"MAE: {mae_rf:.2f}, RMSE: {rmse_rf:.2f}, R^2: {r2_rf:.3f}")


# ================================
# 7. Date & season helpers
# ================================
def month_to_season(mnth: int) -> int:
    """
    Input month (1-12), return season code (1-4)
    1: spring, 2: summer, 3: fall, 4: winter
    """
    if mnth in (3, 4, 5):
        return 1  # spring
    elif mnth in (6, 7, 8):
        return 2  # summer
    elif mnth in (9, 10, 11):
        return 3  # fall
    else:
        return 4  # winter


def extract_date_features(date_str: str):
    """
    Extract date-related features matching the dataset format from date string (YYYY-MM-DD).
    """
    dt = datetime.strptime(date_str, "%Y-%m-%d")

    # Original data: yr=0 means 2011, yr=1 means 2012
    # We map other years to 1 ("next year") for simplicity.
    yr = 0 if dt.year <= 2011 else 1

    mnth = dt.month
    season = month_to_season(mnth)

    # Python: Monday=0, Sunday=6
    # Dataset: 0=Sunday,...,6=Saturday
    py_weekday = dt.weekday()
    weekday = (py_weekday + 1) % 7

    workingday = 1 if weekday in (1, 2, 3, 4, 5) else 0
    holiday = 0  # Simplified: do not distinguish statutory holidays

    return {
        "yr": yr,
        "mnth": mnth,
        "season": season,
        "weekday": weekday,
        "workingday": workingday,
        "holiday": holiday,
    }


# ================================
# 8. Input validation helpers
# ================================
def input_date_with_validation(prompt: str, default: str) -> str:
    """
    Ask user for a date, validate format and existence.
    """
    while True:
        s = input(f"{prompt} (YYYY-MM-DD, 默认 {default}): ").strip()
        if s == "":
            s = default
        try:
            _ = datetime.strptime(s, "%Y-%m-%d")
            return s
        except ValueError:
            print("❌ Invalid or non-existing date, please try again (e.g. 2012-07-01).")


def input_int_in_range(prompt: str, default: int, min_v: int, max_v: int) -> int:
    """
    Ask user for an int within [min_v, max_v].
    """
    while True:
        s = input(f"{prompt} (默认 {default}，范围 {min_v}-{max_v}): ").strip()
        if s == "":
            return default
        try:
            v = int(s)
        except ValueError:
            print("❌ Please enter an integer.")
            continue
        if not (min_v <= v <= max_v):
            print(f"❌ Value out of reasonable range [{min_v}, {max_v}], please try again.")
            continue
        return v


def input_float_in_range(prompt: str, default: float, min_v: float, max_v: float) -> float:
    """
    Ask user for a float within [min_v, max_v].
    """
    while True:
        s = input(f"{prompt} (默认 {default}，范围 {min_v}~{max_v}): ").strip()
        if s == "":
            return default
        try:
            v = float(s)
        except ValueError:
            print("❌ Please enter a number.")
            continue
        if not (min_v <= v <= max_v):
            print(f"❌ Value out of reasonable range [{min_v}, {max_v}], please try again.")
            continue
        return v


# ================================
# 9. Predict one hour
# ================================
def predict_one_hour_from_user(
    date_str: str,
    hour: int,
    temp_c: float,
    weathersit: int = 1,
    hum: float = 0.5,

    windspeed: float = 0.2,
    model_type: str = "rf",
) -> float:
    """
    Predict using user-friendly format:
    - date_str: 'YYYY-MM-DD'
    - hour: 0-23
    - temp_c: Celsius temperature (automatically converted to temp=temp_c/41)
    - weathersit: 1=Good, 2=Average, 3=Bad
    - hum: Normalized humidity (0-1)
    - windspeed: Normalized windspeed (0-1)
    """
    date_feats = extract_date_features(date_str)

    # Celsius -> normalized temp
    temp_norm = temp_c / 41.0
    atemp_norm = temp_norm  # Simplified: feeling temp = temp

    data = pd.DataFrame(
        [
            {
                "hr": hour,
                "weekday": date_feats["weekday"],
                "workingday": date_feats["workingday"],
                "holiday": date_feats["holiday"],
                "mnth": date_feats["mnth"],
                "season": date_feats["season"],
                "yr": date_feats["yr"],
                "weathersit": weathersit,
                "temp": temp_norm,
                "atemp": atemp_norm,
                "hum": hum,
                "windspeed": windspeed,
            }
        ]
    )

    if model_type == "lr":
        pred = model_lr.predict(data)[0]
    else:
        pred = model_rf.predict(data)[0]
        
    return float(pred)


# ================================
# 10. Predict next n hours
# ================================
def predict_next_n_hours_from_user(
    date_str: str,
    start_hour: int,
    n: int,
    temp_c: float,
    weathersit: int = 1,
    hum: float = 0.5,
    windspeed: float = 0.2,
):
    """
    Returns (hours, preds):
    - hours: list of hours corresponding to each prediction
    - preds: predicted cnt for each hour
    """
    hours = []
    preds = []

    for i in range(1, n + 1):
        hr = (start_hour + i) % 24  # wrap to next day if > 23
        y_hat = predict_one_hour_from_user(
            date_str=date_str,
            hour=hr,
            temp_c=temp_c,
            weathersit=weathersit,
            hum=hum,
            windspeed=windspeed,
        )
        hours.append(hr)
        preds.append(y_hat)

    return hours, preds


# ================================
# 11. Trend interpretation
# ================================
def summarize_trend(date_str: str, start_hour: int, hours, preds):
    """
    Give a human-readable summary about demand trend.
    """
    if not preds:
        return

    first = preds[0]
    last = preds[-1]
    avg = sum(preds) / len(preds)

    max_val = max(preds)
    min_val = min(preds)
    max_idx = preds.index(max_val)
    min_idx = preds.index(min_val)
    max_hour = hours[max_idx]
    min_hour = hours[min_idx]

    # avoid division by zero
    base = first if first > 1e-6 else avg if avg > 1e-6 else 1.0
    change_ratio = (last - first) / base

    if abs(change_ratio) < 0.05:
        trend_label = "Overall stable"
        advice = "Demand is expected to be stable in the next few hours."
    elif change_ratio >= 0.2:
        trend_label = "Significant increase"
        advice = (
            f"Expected to peak around {max_hour:02d}:00. "
            "Suggest renting early if needed to avoid shortage."
        )
    elif change_ratio > 0.05:
        trend_label = "Slight increase"
        advice = (
            f"Expected to be slightly busier around {max_hour:02d}:00. "
            "Consider leaving early if time-sensitive."
        )
    elif change_ratio <= -0.2:
        trend_label = "Significant decrease"
        advice = (
            f"Current demand is higher than future prediction. Expected to ease around {min_hour:02d}:00. "
            "Consider waiting if you want to avoid the crowd."
        )
    else:
        trend_label = "Slight decrease"
        advice = (
            f"Demand will slightly decrease. Easier to rent after {min_hour:02d}:00."
        )

    print("\n=== Usage suggestion ===")
    print(f"Trend after {start_hour}:00 on {date_str}: {trend_label}")
    print(advice)
    print(
        f"(Max prediction approx {max_val:.1f}, Min approx {min_val:.1f}, Average approx {avg:.1f} rides/hour)"
    )


# ================================
# 13. Interface for R Shiny
# ================================
def get_predictions(date_str: str, weather_cat: int, temp_c: float, start_hour: int, n_hours: int, hum: float, windspeed: float, model_type: str = "rf"):
    """
    Generate n_hours predictions for R Shiny app starting from start_hour.
    Returns a Pandas DataFrame with columns: Hour, Predicted_Demand, Lower_CI, Upper_CI
    """
    hours = []
    preds = []
    
    # 90% Confidence Interval z-score approx 1.645
    z_score = 1.645
    
    # Ensure model and rmse are available (they are global variables in this script)
    # rmse is calculated in the evaluation section above
    current_rmse = rmse_lr if model_type == "lr" else rmse_rf
    
    for i in range(n_hours):
        # Calculate actual hour (0-23)
        hr = (start_hour + i) % 24
        
        pred = predict_one_hour_from_user(
            date_str=date_str,
            hour=hr,
            temp_c=temp_c,
            weathersit=int(weather_cat),
            hum=hum,
            windspeed=windspeed,
            model_type=model_type
        )
        hours.append(hr)
        preds.append(pred)
        
    # Create DataFrame
    results = pd.DataFrame({
        'Hour': hours,
        'Predicted_Demand': preds
    })
    
    # Add Confidence Intervals
    # Prediction Interval = y_hat +/- z * RMSE
    # Note: This is a simplified PI assuming constant variance (homoscedasticity)
    results['Lower_CI'] = results['Predicted_Demand'] - z_score * current_rmse
    results['Upper_CI'] = results['Predicted_Demand'] + z_score * current_rmse
    
    # Clip negative values to 0 for demand
    results['Predicted_Demand'] = results['Predicted_Demand'].clip(lower=0)
    results['Lower_CI'] = results['Lower_CI'].clip(lower=0)
    results['Upper_CI'] = results['Upper_CI'].clip(lower=0)
    
    return results


# ================================
# 12. CLI interaction
# ================================
if __name__ == "__main__":
    pass
    # The interactive CLI is disabled for the web app.
    # To run in CLI mode, uncomment the lines below.
    
    # print("\n=================================")
    # print("Bike demand prediction - user input")
    # print("=================================\n")

    # # 1) Date (auto validation)
    # date_str = input_date_with_validation("Please enter date", "2012-07-01")

    # # 2) Start hour 0-23
    # start_hour = input_int_in_range("Please enter start hour (0-23)", 14, 0, 23)

    # # 3) Number of hours to predict 1-24
    # n = input_int_in_range("Please enter number of hours n", 5, 1, 24)

    # # 4) Celsius temperature, simple limit [-20, 45]
    # temp_c = input_float_in_range("Please enter current temp (Celsius)", 25.0, -20.0, 45.0)

    # # 5) Weather category weathersit
    # print("\nWeather category weathersit: 1=Good(Clear/Few Clouds), 2=Average(Mist/Cloudy), 3=Bad(Light Rain/Snow)")
    # weathersit = input_int_in_range("Please enter weathersit", 1, 1, 3)

    # # 6) Humidity and Windspeed (0-1 normalized)
    # hum = input_float_in_range("Please enter humidity hum (0-1, suggest 0.3~0.9)", 0.6, 0.0, 1.0)
    # windspeed = input_float_in_range("Please enter windspeed (0-1, suggest 0.0~0.6)", 0.3, 0.0, 1.0)

    # # Predict next n hours
    # hours, preds = predict_next_n_hours_from_user(
    #     date_str=date_str,
    #     start_hour=start_hour,
    #     n=n,
    #     temp_c=temp_c,
    #     weathersit=weathersit,
    #     hum=hum,
    #     windspeed=windspeed,
    # )

    # print(f"\nPredictions for {date_str}, next {n} hours after {start_hour}:00:\n")
    # for h, p in zip(hours, preds):
    #     print(f"Hour {h:02d}: predicted cnt = {p:.1f}")

    # # Trend interpretation + Usage suggestion
    # summarize_trend(date_str, start_hour, hours, preds)

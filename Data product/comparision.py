# ============================================
# 共享单车小时需求预测 - 多模型对比
# Bike sharing hourly demand prediction - model comparison
# ============================================

import os
from math import sqrt

import numpy as np
import pandas as pd

from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import OneHotEncoder, StandardScaler, PolynomialFeatures
from sklearn.pipeline import Pipeline
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score


# ================================
# 1. 读取数据 / Load dataset
# ================================
script_dir = os.path.dirname(os.path.abspath(__file__))
csv_path = os.path.join(script_dir, "bikehour.csv")

print("脚本所在目录 / Script directory:", script_dir)
print("CSV 路径 / CSV path:", csv_path)

df = pd.read_csv(csv_path)

print("\nData preview / 数据预览：")
print(df.head())
print("Columns / 列名：", df.columns.tolist())

# ================================
# 2. 选择特征和目标 / Select features & target
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

print("\nFeature columns / 特征列：", feature_cols)

# ================================
# 3. 按年份划分训练/测试集 / Train-test split by year
# ================================
# 原数据: yr=0 -> 2011 (train), yr=1 -> 2012 (test)
train_mask = df["yr"] == 0
test_mask = df["yr"] == 1

X_train = X[train_mask]
y_train = y[train_mask]
X_test = X[test_mask]
y_test = y[test_mask]

print("\nTrain shape / 训练集尺寸:", X_train.shape)
print("Test shape / 测试集尺寸:", X_test.shape)

print("\n=== Test set stats / 测试集简单统计 ===")
print("Mean cnt / 平均每小时租赁量:", y_test.mean())
print("Median cnt / 中位数:", y_test.median())
print("Max cnt / 最大值:", y_test.max())
print("Min cnt / 最小值:", y_test.min())

# ================================
# 4. 定义特征类型 / Categorical & numeric features
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

# 基础预处理：类别 -> OneHot, 数值 -> StandardScaler
# Basic preprocessor: OneHot for categorical, StandardScaler for numeric
categorical_transformer = OneHotEncoder(handle_unknown="ignore")
numeric_transformer = StandardScaler()

preprocessor_basic = ColumnTransformer(
    transformers=[
        ("cat", categorical_transformer, categorical_features),
        ("num", numeric_transformer, numeric_features),
    ]
)

# 多项式预处理：数值特征先做 PolynomialFeatures，再做 StandardScaler
# For polynomial regression, we add polynomial features to numeric variables.
numeric_poly_pipeline = Pipeline(
    steps=[
        ("poly", PolynomialFeatures(degree=2, include_bias=False)),
        ("scaler", StandardScaler()),
    ]
)

preprocessor_poly = ColumnTransformer(
    transformers=[
        ("cat", categorical_transformer, categorical_features),
        ("num", numeric_poly_pipeline, numeric_features),
    ]
)

# ================================
# 5. 通用评估函数 / Generic evaluation helper
# ================================
def evaluate_predictions(y_true, y_pred, model_name):
    """计算并打印 MAE / RMSE / R²，返回一个结果字典。
    Compute and print MAE / RMSE / R², return result dict.
    """
    mae = mean_absolute_error(y_true, y_pred)
    mse = mean_squared_error(y_true, y_pred)
    rmse = sqrt(mse)
    r2 = r2_score(y_true, y_pred)

    print(f"\n>> {model_name} performance / 模型表现")
    print(f"MAE : {mae:.2f}")
    print(f"RMSE: {rmse:.2f}")
    print(f"R^2 : {r2:.3f}")

    return {
        "model": model_name,
        "MAE": mae,
        "RMSE": rmse,
        "R2": r2,
    }


def train_and_evaluate_pipeline(pipeline, model_name):
    """训练带预处理的 pipeline 并在测试集上评估。
    Train a pipeline (with preprocessing) and evaluate on test set.
    """
    print(f"\n=== Training {model_name} ===")
    pipeline.fit(X_train, y_train)
    print("Training done.")

    y_pred = pipeline.predict(X_test)
    result = evaluate_predictions(y_test, y_pred, model_name)
    return pipeline, result


results = []  # 用来保存所有模型的结果 / store results of all models

# ================================
# 6. 基线模型：始终预测训练集平均值
#    Baseline: always predict train mean
# ================================
print("\n=== Baseline model: Global mean / 基线模型：全局均值 ===")
mean_train_cnt = y_train.mean()
y_pred_baseline = np.full_like(y_test, fill_value=mean_train_cnt, dtype=float)
res_baseline = evaluate_predictions(y_test, y_pred_baseline, "GlobalMeanBaseline")
results.append(res_baseline)

# ================================
# 7. 线性回归 / Linear Regression
# ================================
lr_pipeline = Pipeline(
    steps=[
        ("preprocess", preprocessor_basic),
        ("regressor", LinearRegression()),
    ]
)
lr_model, res_lr = train_and_evaluate_pipeline(lr_pipeline, "LinearRegression")
results.append(res_lr)

# ================================
# 8. 非线性回归：多项式特征 + 线性回归
#    Nonlinear regression: Polynomial features + LinearRegression
# ================================
poly_lr_pipeline = Pipeline(
    steps=[
        ("preprocess", preprocessor_poly),
        ("regressor", LinearRegression()),
    ]
)
poly_lr_model, res_poly = train_and_evaluate_pipeline(
    poly_lr_pipeline, "PolynomialLinearRegression"
)
results.append(res_poly)

# ================================
# 9. 随机森林回归 / Random Forest Regressor
# ================================
rf_pipeline = Pipeline(
    steps=[
        ("preprocess", preprocessor_basic),
        (
            "regressor",
            RandomForestRegressor(
                n_estimators=300,
                max_depth=None,
                min_samples_leaf=1,
                random_state=42,
                n_jobs=-1,
            ),
        ),
    ]
)
rf_model, res_rf = train_and_evaluate_pipeline(
    rf_pipeline, "RandomForestRegressor"
)
results.append(res_rf)

# ================================
# 10. 梯度提升树 / Gradient Boosting Regressor
# ================================
gbr_pipeline = Pipeline(
    steps=[
        ("preprocess", preprocessor_basic),
        (
            "regressor",
            GradientBoostingRegressor(
                n_estimators=300,
                learning_rate=0.05,
                max_depth=3,
                random_state=42,
            ),
        ),
    ]
)
gbr_model, res_gbr = train_and_evaluate_pipeline(
    gbr_pipeline, "GradientBoostingRegressor"
)
results.append(res_gbr)

# ================================
# 11. 模型对比表 / Comparison table
# ================================
print("\n===============================")
print("Model comparison / 模型对比")
print("===============================")
print(f"{'Model':28s}\t{'MAE':>10s}\t{'RMSE':>10s}\t{'R^2':>10s}")
for r in results:
    print(
        f"{r['model']:28s}\t"
        f"{r['MAE']:10.2f}\t"
        f"{r['RMSE']:10.2f}\t"
        f"{r['R2']:10.3f}"
    )

# 找出 MAE 最小的模型 / pick best model by MAE
best_by_mae = min(results, key=lambda d: d["MAE"])
print("\nBest model by MAE / 按 MAE 最好的模型:")
print(
    f"{best_by_mae['model']}  (MAE={best_by_mae['MAE']:.2f}, "
    f"RMSE={best_by_mae['RMSE']:.2f}, R^2={best_by_mae['R2']:.3f})"
)

# 如果你后续要做用户交互预测，可以在这里选择一个模型作为最终模型：
# If you want to use a model for user-facing prediction, choose one here:
final_model = gbr_model  # 例如选择 GBDT 作为最终模型 / e.g. choose GBDT
# 然后把之前的 predict_one_hour_from_user 等函数里的 model 换成 final_model 即可。

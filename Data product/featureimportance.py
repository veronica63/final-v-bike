# ============================================
# rf_feature_importance.py
# 随机森林模型训练 + 特征重要性分析
# （独立脚本，可直接运行）
# ============================================

import os
from math import sqrt

import numpy as np
import pandas as pd

from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import OneHotEncoder, StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.ensemble import RandomForestRegressor
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
# 4. 特征预处理 / Preprocessing
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
# 5. 随机森林模型 / RandomForest model
# ================================
rf_pipeline = Pipeline(
    steps=[
        ("preprocess", preprocessor),
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

print("\n=== Training RandomForestRegressor ===")
rf_pipeline.fit(X_train, y_train)
print("Training done.")

# ================================
# 6. 测试集评估 / Evaluation
# ================================
y_pred = rf_pipeline.predict(X_test)

mae = mean_absolute_error(y_test, y_pred)
mse = mean_squared_error(y_test, y_pred)
rmse = sqrt(mse)
r2 = r2_score(y_test, y_pred)

print("\n=== RandomForest performance / 模型表现 ===")
print(f"MAE : {mae:.2f}")
print(f"RMSE: {rmse:.2f}")
print(f"R^2 : {r2:.3f}")

# ================================
# 7. 提取特征重要性（编码后粒度）
#    Feature importance at encoded level (after OneHot)
# ================================
pre = rf_pipeline.named_steps["preprocess"]
rf = rf_pipeline.named_steps["regressor"]

# OneHot 之后的类别特征列名
ohe = pre.named_transformers_["cat"]
cat_feature_names = ohe.get_feature_names_out(categorical_features)  # array[str]

# 数值特征列名（StandardScaler 不改变名字）
num_feature_names = np.array(numeric_features)

# 编码后的所有特征名（顺序与 feature_importances_ 对齐）
all_feature_names = np.concatenate([cat_feature_names, num_feature_names])

importances = rf.feature_importances_

feat_imp_detail = (
    pd.DataFrame(
        {
            "encoded_feature": all_feature_names,
            "importance": importances,
        }
    )
    .sort_values("importance", ascending=False)
    .reset_index(drop=True)
)

print("\n=== RandomForest feature importance (encoded level) / 详细特征重要性 ===")
print(feat_imp_detail.head(30))  # 只看前 30 行，防止太长

# 如需保存到文件，可以取消下一行注释：
# detail_path = os.path.join(script_dir, "rf_feature_importance_detail.csv")
# feat_imp_detail.to_csv(detail_path, index=False)
# print(f"\n详细重要性已保存到: {detail_path}")

# ================================
# 8. 按原始特征汇总的重要性
#    Aggregated importance by original feature
# ================================
agg_importance = {}

# 1) 类别特征：把对应的 one-hot 列重要性求和
cat_importances = importances[: len(cat_feature_names)]
for orig_feat in categorical_features:
    mask = np.char.startswith(cat_feature_names.astype(str), orig_feat + "_")
    agg_importance[orig_feat] = cat_importances[mask].sum()

# 2) 数值特征：每个特征对应一列
start_idx = len(cat_feature_names)
for i, orig_feat in enumerate(numeric_features):
    agg_importance[orig_feat] = importances[start_idx + i]

feat_imp_agg = (
    pd.DataFrame(
        {
            "feature": list(agg_importance.keys()),
            "importance": list(agg_importance.values()),
        }
    )
    .sort_values("importance", ascending=False)
    .reset_index(drop=True)
)

print("\n=== RandomForest feature importance (aggregated) / 按原始特征汇总 ===")
print(feat_imp_agg)

# 如需保存汇总结果到文件，可以取消下一行注释：
# agg_path = os.path.join(script_dir, "rf_feature_importance_aggregated.csv")
# feat_imp_agg.to_csv(agg_path, index=False)
# print(f"\n汇总重要性已保存到: {agg_path}")

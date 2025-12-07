# ================================
# 0. 导入库 / Import libraries
# ================================
import os
import pandas as pd
import numpy as np
# 你后面的 sklearn import 不动


from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import OneHotEncoder, StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score

# ================================
# 1. 读取数据 / Load the dataset
# ================================
# 请把路径改成你自己的文件路径
# Please change the path to your own csv file location
df = pd.read_csv("bikehour.csv")

print("Data preview / 数据预览：")
print(df.head())
print("Columns / 列名：", df.columns.tolist())

# ================================
# 2. 选择特征和目标 / Select features and target
# ================================
# 我们先用一组合适的基础特征：
# - 时间相关: hr, weekday, workingday, holiday, mnth, season, yr
# - 天气相关: weathersit, temp, atemp, hum, windspeed
# 目标变量是 cnt（该小时的租赁次数）
#
# We start with a basic feature set:
# - Time-related: hr, weekday, workingday, holiday, mnth, season, yr
# - Weather-related: weathersit, temp, atemp, hum, windspeed
# Target variable is cnt (number of rentals in that hour)

feature_cols = [
    "hr",          # 小时 / hour of day (0-23)
    "weekday",     # 周几 / day of week (0=Sunday)
    "workingday",  # 是否工作日 / is working day (1) or not (0)
    "holiday",     # 是否节假日 / is holiday (1) or not (0)
    "mnth",        # 月份 / month (1-12)
    "season",      # 季节 / season (1-4)
    "yr",          # 年份 (0=2011, 1=2012) / year indicator
    "weathersit",  # 天气类别 / weather situation category
    "temp",        # 归一化气温 / normalized temperature
    "atemp",       # 归一化体感温度 / normalized feeling temperature
    "hum",         # 归一化湿度 / normalized humidity
    "windspeed"    # 归一化风速 / normalized wind speed
]

target_col = "cnt"   # 目标：小时租赁总量 / target: hourly total rental count

X = df[feature_cols].copy()
y = df[target_col].copy()

# ================================
# 3. 按年份划分训练集和测试集 / Split train & test by year
# ================================
# 使用 2011 年 (yr=0) 做训练集，2012 年 (yr=1) 做测试集
# This simulates "train on past, test on future".
train_mask = (df["yr"] == 0)  # 2011
test_mask = (df["yr"] == 1)   # 2012

X_train = X[train_mask]
y_train = y[train_mask]

X_test = X[test_mask]
y_test = y[test_mask]

print("Train shape / 训练集尺寸:", X_train.shape)
print("Test shape / 测试集尺寸:", X_test.shape)

# ================================
# 4. 定义类别特征和数值特征 / Define categorical & numeric features
# ================================
# 类别特征我们做 One-Hot 编码
# Categorical features will be one-hot encoded.
categorical_features = [
    "season", "yr", "mnth", "hr",
    "weekday", "workingday", "holiday", "weathersit"
]

# 数值特征我们做标准化（减均值除标准差）
# Numeric features will be standardized (zero mean, unit variance).
numeric_features = ["temp", "atemp", "hum", "windspeed"]

# ================================
# 5. 构建预处理器 / Build the preprocessor
# ================================
# OneHotEncoder: 把类别变量变成多列 0/1
# StandardScaler: 把数值变量缩放到均值0、方差1
#
# OneHotEncoder turns categorical variables into 0/1 dummy columns.
# StandardScaler normalizes numeric variables to mean 0 and std 1.

categorical_transformer = OneHotEncoder(handle_unknown="ignore")
numeric_transformer = StandardScaler()

preprocessor = ColumnTransformer(
    transformers=[
        ("cat", categorical_transformer, categorical_features),
        ("num", numeric_transformer, numeric_features),
    ]
)

# ================================
# 6. 搭建包含预处理+线性回归的 Pipeline
#    Build a Pipeline: preprocessing + Linear Regression
# ================================
# Pipeline 会先做预处理，再把结果送给线性回归模型
# The pipeline first applies preprocessing, then fits the linear regression model.

model = Pipeline(steps=[
    ("preprocess", preprocessor),
    ("regressor", LinearRegression())
])

# ================================
# 7. 训练模型 / Train the model
# ================================
print("Training Linear Regression model... / 开始训练线性回归模型...")
model.fit(X_train, y_train)
print("Training done. / 训练完成。")

# ================================
# 8. 在测试集上评估模型 / Evaluate on test set
# ================================
y_pred = model.predict(X_test)

from math import sqrt  # 文件顶部可以加上 / add this near the top

# 评价指标 / Evaluation metrics
mae = mean_absolute_error(y_test, y_pred)

# 某些旧版本的 sklearn 不支持 squared 参数，所以我们手动开平方
# Some older sklearn versions don't support the 'squared' argument,
# so we compute RMSE manually from MSE.
mse = mean_squared_error(y_test, y_pred)
rmse = sqrt(mse)

r2 = r2_score(y_test, y_pred)

print("\n=== Test performance / 测试集表现 ===")
print(f"MAE (Mean Absolute Error / 平均绝对误差): {mae:.2f}")
print(f"RMSE (Root Mean Squared Error / 均方根误差): {rmse:.2f}")
print(f"R^2 (Coefficient of Determination / 决定系数): {r2:.3f}")

# ================================
# 9. 查看部分预测结果 / Inspect some predictions
# ================================
results = pd.DataFrame({
    "y_true": y_test.values,
    "y_pred": y_pred
})
print("\nSample predictions / 部分预测结果示例：")
print(results.head(10))

# ================================
# 10. 写一个小函数：给定一行特征，预测一个小时的租赁量
#     Define a helper function: predict cnt for one hour given features
# ================================
def predict_one_hour(
    hr,
    weekday,
    workingday,
    holiday,
    mnth,
    season,
    yr,
    weathersit,
    temp,
    atemp,
    hum,
    windspeed
):
    """
    输入一组特征，输出线性回归模型预测的该小时租赁量。
    Input one feature set, output predicted cnt for that hour.
    """
    # 构造与训练时相同结构的 DataFrame（单行）
    # Build a one-row DataFrame with the same structure
    data = pd.DataFrame([{
        "hr": hr,
        "weekday": weekday,
        "workingday": workingday,
        "holiday": holiday,
        "mnth": mnth,
        "season": season,
        "yr": yr,
        "weathersit": weathersit,
        "temp": temp,
        "atemp": atemp,
        "hum": hum,
        "windspeed": windspeed
    }])

    pred = model.predict(data)[0]
    return pred

# 示例：预测“2012年某个夏天的工作日晚上18点，晴天，适中气温”的用车量
# Example: predict bike demand for a summer weekday at 18:00 in 2012 with clear weather.
example_pred = predict_one_hour(
    hr=18,
    weekday=3,        # e.g. Wednesday / 周三
    workingday=1,     # working day / 工作日
    holiday=0,        # not a holiday / 非假日
    mnth=7,           # July / 7月
    season=3,         # e.g. season 3 (check your dataset doc) / 例如：3=夏季
    yr=1,             # 2012
    weathersit=1,     # good weather / 晴好
    temp=0.6,         # 这些是归一化后的数值，你可以用真实数据里的典型值
    atemp=0.6,
    hum=0.5,
    windspeed=0.2
)

print("\nExample prediction / 示例预测结果：", example_pred)

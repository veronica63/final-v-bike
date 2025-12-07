# ============================================
# 共享单车小时需求预测 - 随机森林版 + 用户交互
# Bike sharing hourly demand prediction - RandomForest + CLI
# ============================================

import os
from math import sqrt
from datetime import datetime

import pandas as pd
import numpy as np

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
model = Pipeline(
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

print("\nTraining RandomForestRegressor model... / 正在训练随机森林模型...")
model.fit(X_train, y_train)
print("Training done. / 训练完成。")

# ================================
# 6. 测试集评估 / Evaluation on test set
# ================================
y_pred = model.predict(X_test)

mae = mean_absolute_error(y_test, y_pred)
mse = mean_squared_error(y_test, y_pred)
rmse = sqrt(mse)
r2 = r2_score(y_test, y_pred)

print("\n=== Test performance / 测试集表现（RandomForest） ===")
print(f"MAE (平均绝对误差): {mae:.2f}")
print(f"RMSE (均方根误差): {rmse:.2f}")
print(f"R^2 (决定系数): {r2:.3f}")


# ================================
# 7. 日期 & 季节辅助函数 / Date & season helpers
# ================================
def month_to_season(mnth: int) -> int:
    """
    输入月份(1-12)，返回 season 编码(1-4)
    1: spring, 2: summer, 3: fall, 4: winter
    """
    if mnth in (3, 4, 5):
        return 1  # spring / 春
    elif mnth in (6, 7, 8):
        return 2  # summer / 夏
    elif mnth in (9, 10, 11):
        return 3  # fall / 秋
    else:
        return 4  # winter / 冬


def extract_date_features(date_str: str):
    """
    从日期字符串 (YYYY-MM-DD) 提取与数据集匹配的日期特征。
    Extract date-related features matching the dataset format.
    """
    dt = datetime.strptime(date_str, "%Y-%m-%d")

    # 原数据: yr=0 表示 2011, yr=1 表示 2012
    yr = 0 if dt.year <= 2011 else 1

    mnth = dt.month
    season = month_to_season(mnth)

    # Python: Monday=0, Sunday=6
    # Dataset: 0=Sunday,...,6=Saturday
    py_weekday = dt.weekday()
    weekday = (py_weekday + 1) % 7

    workingday = 1 if weekday in (1, 2, 3, 4, 5) else 0
    holiday = 0  # 简化: 不区分法定节假日 / simplified

    return {
        "yr": yr,
        "mnth": mnth,
        "season": season,
        "weekday": weekday,
        "workingday": workingday,
        "holiday": holiday,
    }


# ================================
# 8. 输入校验工具 / Input validation helpers
# ================================
def input_date_with_validation(prompt: str, default: str) -> str:
    """
    让用户输入日期，自动校验格式和是否存在。
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
            print("❌ 日期格式不正确或日期不存在，请重新输入，例如 2012-07-01。")
            print("   Invalid or non-existing date, please try again (e.g. 2012-07-01).")


def input_int_in_range(prompt: str, default: int, min_v: int, max_v: int) -> int:
    """
    让用户输入整数并限制在 [min_v, max_v] 范围内。
    Ask user for an int within [min_v, max_v].
    """
    while True:
        s = input(f"{prompt} (默认 {default}，范围 {min_v}-{max_v}): ").strip()
        if s == "":
            return default
        try:
            v = int(s)
        except ValueError:
            print("❌ 请输入整数数字 / Please enter an integer.")
            continue
        if not (min_v <= v <= max_v):
            print(f"❌ 数值超出合理范围 [{min_v}, {max_v}]，请重试。")
            continue
        return v


def input_float_in_range(prompt: str, default: float, min_v: float, max_v: float) -> float:
    """
    让用户输入浮点数并限制在 [min_v, max_v] 范围内。
    Ask user for a float within [min_v, max_v].
    """
    while True:
        s = input(f"{prompt} (默认 {default}，范围 {min_v}~{max_v}): ").strip()
        if s == "":
            return default
        try:
            v = float(s)
        except ValueError:
            print("❌ 请输入数字 / Please enter a number.")
            continue
        if not (min_v <= v <= max_v):
            print(f"❌ 数值超出合理范围 [{min_v}, {max_v}]，请重试。")
            continue
        return v


# ================================
# 9. 单小时预测函数 / Predict one hour
# ================================
def predict_one_hour_from_user(
    date_str: str,
    hour: int,
    temp_c: float,
    weathersit: int = 1,
    hum: float = 0.5,
    windspeed: float = 0.2,
) -> float:
    """
    使用用户友好格式进行预测：
    - date_str: 'YYYY-MM-DD'
    - hour: 0-23
    - temp_c: 摄氏温度（自动转换为 temp=temp_c/41）
    - weathersit: 1=好, 2=一般, 3=差
    - hum: 湿度归一化(0-1)
    - windspeed: 风速归一化(0-1)
    """
    date_feats = extract_date_features(date_str)

    # 摄氏度 -> 归一化温度 / Celsius -> normalized temp
    temp_norm = temp_c / 41.0
    atemp_norm = temp_norm  # 简化: 体感温度 = 温度 / simplified

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

    # 随机森林基本不会给负数，但保险起见我们仍然裁成 >=0
    raw_pred = model.predict(data)[0]
    pred = max(raw_pred, 0.0)  # ensure non-negative
    return float(pred)


# ================================
# 10. 预测未来 n 小时 / Predict next n hours
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
    返回 (hours, preds)：
    - hours: 每个预测对应的小时列表
    - preds: 每个小时的预测 cnt
    """
    hours = []
    preds = []

    for i in range(1, n + 1):
        hr = (start_hour + i) % 24  # 超过 23 点就滚到第二天 / wrap to next day
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
# 11. 趋势解读 / Trend interpretation
# ================================
def summarize_trend(date_str: str, start_hour: int, hours, preds):
    """
    根据预测结果给用户一句友好的提示：
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

    base = first if first > 1e-6 else avg if avg > 1e-6 else 1.0
    change_ratio = (last - first) / base

    if abs(change_ratio) < 0.05:
        trend_label = "整体较为稳定"
        advice = "未来几小时用车需求整体比较稳定，可以按平时习惯安排出行。"
    elif change_ratio >= 0.2:
        trend_label = "用车量将明显增加"
        advice = (
            f"预计在 {max_hour:02d}:00 左右达到高峰，建议如需用车尽量提前借车，"
            "避免高峰时段车辆紧张。"
        )
    elif change_ratio > 0.05:
        trend_label = "用车量会略有增加"
        advice = (
            f"预计在 {max_hour:02d}:00 附近会稍微忙一些，如对时间敏感可适当提前出发。"
        )
    elif change_ratio <= -0.2:
        trend_label = "用车量将明显减少"
        advice = (
            f"当前时段比未来预测更繁忙，预计到 {min_hour:02d}:00 附近会明显缓和，"
            "如果想错峰出行可以稍晚一点使用。"
        )
    else:
        trend_label = "用车量会略有下降"
        advice = (
            f"未来几小时需求会略微下降，{min_hour:02d}:00 后会更容易借到车。"
        )

    print("\n=== 使用建议 / Usage suggestion ===")
    print(f"{date_str} 从 {start_hour}:00 之后的趋势：{trend_label}")
    print(advice)
    print(
        f"(预测区间内最高预测值约为 {max_val:.1f}，最低约为 {min_val:.1f}，平均约 {avg:.1f} 次/小时)"
    )


# ================================
# 12. 命令行交互 / CLI interaction
# ================================
if __name__ == "__main__":
    print("\n=================================")
    print("共享单车需求预测 - 用户输入模式（随机森林）")
    print("Bike demand prediction - user input (RandomForest)")
    print("=================================\n")

    # 1) 日期
    date_str = input_date_with_validation("请输入日期", "2012-07-01")

    # 2) 起始小时 0-23
    start_hour = input_int_in_range("请输入起始小时 (0-23)", 14, 0, 23)

    # 3) 预测的小时数 1-24
    n = input_int_in_range("请输入要预测的小时数 n", 5, 1, 24)

    # 4) 摄氏温度，简单限制 [-20, 45]
    temp_c = input_float_in_range("请输入当前温度 (摄氏度)", 25.0, -20.0, 45.0)

    # 5) 天气类别 weathersit
    print("\n天气类别 weathersit: 1=好(晴/少云), 2=一般(雾/阴), 3=差(小雨/小雪)")
    weathersit = input_int_in_range("请输入 weathersit", 1, 1, 3)

    # 6) 湿度和风速（0-1 归一化）
    hum = input_float_in_range("请输入湿度 hum (0-1, 建议 0.3~0.9)", 0.6, 0.0, 1.0)
    windspeed = input_float_in_range(
        "请输入风速 windspeed (0-1, 建议 0.0~0.6)", 0.3, 0.0, 1.0
    )

    # 预测未来 n 小时
    hours, preds = predict_next_n_hours_from_user(
        date_str=date_str,
        start_hour=start_hour,
        n=n,
        temp_c=temp_c,
        weathersit=weathersit,
        hum=hum,
        windspeed=windspeed,
    )

    print(f"\n{date_str} 从 {start_hour}:00 之后未来 {n} 小时预测结果：")
    print(f"Predictions for {date_str}, next {n} hours after {start_hour}:00:\n")
    for h, p in zip(hours, preds):
        print(f"Hour {h:02d}: predicted cnt = {p:.1f}")

    # 趋势解读 + 使用建议
    summarize_trend(date_str, start_hour, hours, preds)

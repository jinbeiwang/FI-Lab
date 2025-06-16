#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
生存分析SHAP解释性分析 - Python版本
适用于XGBoost和随机生存森林的SHAP值计算与可视化
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import shap
import xgboost as xgb
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, LabelEncoder
from sksurv.metrics import concordance_index_censored
import warnings
import os
from pathlib import Path

# 设置图形样式和参数
plt.style.use('seaborn-v0_8-whitegrid')
sns.set_palette("husl")
warnings.filterwarnings('ignore')

# 设置matplotlib中文字体和参数
plt.rcParams['font.family'] = ['DejaVu Sans', 'SimHei', 'Arial Unicode MS']
plt.rcParams['axes.unicode_minus'] = False
plt.rcParams['figure.dpi'] = 300
plt.rcParams['savefig.dpi'] = 300
plt.rcParams['figure.facecolor'] = 'white'
plt.rcParams['axes.facecolor'] = 'white'

# 创建输出目录
output_dir = Path("Feature_selection")
output_dir.mkdir(exist_ok=True)


def load_and_preprocess_data():
    """
    加载和预处理生存数据
    """
    print("🔄 正在加载数据...")

    # 读取生存数据
    try:
        tte_data = pd.read_csv('tte.csv')
        print(f"✅ 生存数据加载成功: {tte_data.shape}")
    except FileNotFoundError:
        print("❌ 未找到tte.csv文件，请检查文件路径")
        return None, None

    # 读取特征元数据
    try:
        metadata = pd.read_excel('fi_lab_metadata.xlsx')
        print(f"✅ 元数据加载成功: {metadata.shape}")

        # 筛选需要的特征变量 (xvar==1)
        feature_vars = metadata[metadata['xvar'] == 1]['variable_name'].tolist()
        print(f"📊 识别到 {len(feature_vars)} 个特征变量")
    except FileNotFoundError:
        print("❌ 未找到fi_lab_metadata.xlsx文件，请检查文件路径")
        return None, None

    return tte_data, feature_vars


def prepare_survival_data(tte_data, feature_vars, endpoint='ICU28D'):
    """
    准备生存分析数据
    """
    print(f"🔧 正在准备 {endpoint} 生存数据...")

    # 筛选指定endpoint的数据
    survival_data = tte_data[tte_data['testcd'] == endpoint].copy()

    if survival_data.empty:
        print(f"❌ 未找到 {endpoint} 相关数据")
        return None, None, None

    # 检查必要列是否存在
    required_cols = ['aval', 'cnsr'] + feature_vars
    missing_cols = [col for col in required_cols if col not in survival_data.columns]

    if missing_cols:
        print(f"❌ 缺少必要列: {missing_cols}")
        available_features = [col for col in feature_vars if col in survival_data.columns]
        print(f"📋 可用特征: {len(available_features)} 个")
        feature_vars = available_features

    # 提取时间、事件和特征
    time = survival_data['aval'].values
    event = survival_data['cnsr'].values  # 1=event (与常规定义相反)

    # 特征数据
    X = survival_data[feature_vars].copy()

    # 处理缺失值
    X = X.fillna(X.median())

    # 处理分类变量
    le_dict = {}
    for col in X.columns:
        if X[col].dtype == 'object':
            le = LabelEncoder()
            X[col] = le.fit_transform(X[col].astype(str))
            le_dict[col] = le

    print(f"✅ 数据准备完成: {X.shape[0]} 样本, {X.shape[1]} 特征")
    print(f"📈 事件率: {event.mean():.2%}")

    return X, time, event


def train_xgboost_survival(X, time, event, test_size=0.2):
    """
    训练XGBoost生存模型
    """
    print("🚀 正在训练XGBoost生存模型...")

    # 创建生存标签 (时间越长，事件发生=0时，标签越大)
    # 使用负的对数时间作为回归目标，事件发生时权重更高
    y = np.where(event == 1, -np.log(time + 1), np.log(time + 1))

    # 数据分割
    X_train, X_test, y_train, y_test, time_train, time_test, event_train, event_test = \
        train_test_split(X, y, time, event, test_size=test_size, random_state=42, stratify=event)

    # 计算样本权重
    sample_weight = np.where(event_train == 1, 2.0, 1.0)  # 给事件发生的样本更高权重

    # 训练XGBoost模型
    model = xgb.XGBRegressor(
        n_estimators=100,
        max_depth=5,
        learning_rate=0.1,
        subsample=0.8,
        colsample_bytree=0.8,
        random_state=42,
        tree_method='hist'
    )

    model.fit(X_train, y_train, sample_weight=sample_weight)

    # 计算C-index评估模型性能
    try:
        pred_train = model.predict(X_train)
        pred_test = model.predict(X_test)

        # 计算C-index（需要反转预测值，因为我们用的是负对数时间）
        c_index_train = concordance_index_censored(event_train == 1, time_train, -pred_train)[0]
        c_index_test = concordance_index_censored(event_test == 1, time_test, -pred_test)[0]

        print(f"📊 训练集 C-index: {c_index_train:.3f}")
        print(f"📊 测试集 C-index: {c_index_test:.3f}")
    except Exception as e:
        print(f"⚠️ C-index计算出错: {e}")

    return model, X_train, X_test, y_train, y_test


def calculate_shap_values(model, X_train, X_test):
    """
    计算SHAP值
    """
    print("🔍 正在计算SHAP值...")

    # 使用TreeExplainer
    explainer = shap.TreeExplainer(model)

    # 计算测试集的SHAP值
    shap_values = explainer.shap_values(X_test)

    print(f"✅ SHAP值计算完成: {shap_values.shape}")

    return explainer, shap_values


def save_plot_multiple_formats(plt_obj, filename_base, output_dir):
    """
    保存图片为多种格式
    """
    formats = ['jpg', 'png', 'svg', 'tif']
    for fmt in formats:
        filepath = output_dir / f"{filename_base}.{fmt}"
        if hasattr(plt_obj, 'savefig'):
            plt_obj.savefig(filepath, format=fmt, bbox_inches='tight', dpi=300, facecolor='white')
        else:
            plt.savefig(filepath, format=fmt, bbox_inches='tight', dpi=300, facecolor='white')
    print(f"💾 图片已保存: {filename_base}")


def create_shap_visualizations(explainer, shap_values, X_test, endpoint, output_dir):
    """
    创建SHAP可视化图形
    """
    print("🎨 正在创建SHAP可视化图形...")

    # 1. 全局特征重要性条形图
    plt.figure(figsize=(12, 8))
    shap.summary_plot(shap_values, X_test, plot_type="bar", show=False)
    plt.title(f'SHAP Feature Importance - {endpoint}', fontsize=16, fontweight='bold', pad=20)
    plt.xlabel('Mean |SHAP Value|', fontsize=14)
    plt.ylabel('Features', fontsize=14)
    plt.tick_params(axis='both', which='major', labelsize=12)
    plt.tight_layout()
    save_plot_multiple_formats(plt, f'shap_feature_importance_{endpoint}', output_dir)
    plt.close()

    # 2. SHAP汇总图（蜂群图）
    plt.figure(figsize=(12, 10))
    shap.summary_plot(shap_values, X_test, show=False, alpha=0.7)
    plt.title(f'SHAP Summary Plot - {endpoint}', fontsize=16, fontweight='bold', pad=20)
    plt.xlabel('SHAP Value (Impact on Model Output)', fontsize=14)
    plt.ylabel('Features', fontsize=14)
    plt.tick_params(axis='both', which='major', labelsize=12)

    # 添加颜色条标签
    cbar = plt.colorbar(plt.cm.ScalarMappable(cmap=plt.cm.coolwarm), ax=plt.gca())
    cbar.set_label('Feature Value', fontsize=12)
    plt.tight_layout()
    save_plot_multiple_formats(plt, f'shap_summary_plot_{endpoint}', output_dir)
    plt.close()

    # 3. 前5个重要特征的依赖图
    feature_importance = np.abs(shap_values).mean(0)
    top_features_idx = np.argsort(feature_importance)[-5:][::-1]
    top_features = X_test.columns[top_features_idx]

    for i, feature in enumerate(top_features):
        plt.figure(figsize=(10, 6))
        shap.dependence_plot(feature, shap_values, X_test, show=False, alpha=0.7)
        plt.title(f'SHAP Dependence Plot - {feature} ({endpoint})', fontsize=14, fontweight='bold', pad=15)
        plt.xlabel(f'{feature}', fontsize=12)
        plt.ylabel(f'SHAP Value for {feature}', fontsize=12)
        plt.tick_params(axis='both', which='major', labelsize=10)
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        save_plot_multiple_formats(plt, f'shap_dependence_{feature}_{endpoint}', output_dir)
        plt.close()

    # 4. 局部解释 - 瀑布图（前3个样本）
    for i in range(min(3, len(X_test))):
        plt.figure(figsize=(12, 8))

        # 创建解释对象
        explanation = shap.Explanation(
            values=shap_values[i],
            base_values=explainer.expected_value,
            data=X_test.iloc[i].values,
            feature_names=X_test.columns.tolist()
        )

        shap.plots.waterfall(explanation, show=False)
        plt.title(f'SHAP Waterfall Plot - Sample {i + 1} ({endpoint})', fontsize=14, fontweight='bold', pad=15)
        plt.tick_params(axis='both', which='major', labelsize=10)
        plt.tight_layout()
        save_plot_multiple_formats(plt, f'shap_waterfall_sample_{i + 1}_{endpoint}', output_dir)
        plt.close()

    # 5. 决策图（前3个样本）
    for i in range(min(3, len(X_test))):
        plt.figure(figsize=(10, 12))
        shap.decision_plot(explainer.expected_value, shap_values[i], X_test.iloc[i],
                           feature_names=X_test.columns.tolist(), show=False)
        plt.title(f'SHAP Decision Plot - Sample {i + 1} ({endpoint})', fontsize=14, fontweight='bold', pad=15)
        plt.tick_params(axis='both', which='major', labelsize=10)
        plt.tight_layout()
        save_plot_multiple_formats(plt, f'shap_decision_sample_{i + 1}_{endpoint}', output_dir)
        plt.close()

    # 6. 特征相互作用热力图
    if len(X_test.columns) <= 20:  # 只对少量特征计算相互作用
        plt.figure(figsize=(12, 10))

        # 计算SHAP相互作用值（计算量大，谨慎使用）
        try:
            interaction_values = explainer.shap_interaction_values(X_test.iloc[:100])  # 只用前100个样本
            interaction_mean = np.abs(interaction_values).mean(0)

            # 创建热力图
            sns.heatmap(interaction_mean,
                        xticklabels=X_test.columns,
                        yticklabels=X_test.columns,
                        annot=False,
                        cmap='coolwarm',
                        center=0,
                        square=True)
            plt.title(f'SHAP Interaction Values Heatmap - {endpoint}', fontsize=14, fontweight='bold', pad=15)
            plt.xlabel('Features', fontsize=12)
            plt.ylabel('Features', fontsize=12)
            plt.xticks(rotation=45, ha='right')
            plt.yticks(rotation=0)
            plt.tight_layout()
            save_plot_multiple_formats(plt, f'shap_interaction_heatmap_{endpoint}', output_dir)
        except Exception as e:
            print(f"⚠️ 相互作用值计算失败: {e}")

        plt.close()


def generate_shap_report(shap_values, X_test, endpoint, output_dir):
    """
    生成SHAP分析报告
    """
    print("📋 正在生成SHAP分析报告...")

    # 计算特征重要性
    feature_importance = np.abs(shap_values).mean(0)
    feature_stats = pd.DataFrame({
        'Feature': X_test.columns,
        'Mean_Abs_SHAP': feature_importance,
        'Mean_SHAP': shap_values.mean(0),
        'Std_SHAP': shap_values.std(0),
        'Min_SHAP': shap_values.min(0),
        'Max_SHAP': shap_values.max(0)
    }).sort_values('Mean_Abs_SHAP', ascending=False)

    # 保存报告
    report_path = output_dir / f'shap_analysis_report_{endpoint}.csv'
    feature_stats.to_csv(report_path, index=False)

    print(f"📊 SHAP分析报告已保存: {report_path}")
    print("\n📈 前10个最重要特征:")
    print(feature_stats.head(10)[['Feature', 'Mean_Abs_SHAP', 'Mean_SHAP']].to_string(index=False))

    return feature_stats


def main():
    """
    主函数
    """
    print("🏥 生存分析SHAP解释性分析开始\n" + "=" * 50)

    # 1. 加载数据
    tte_data, feature_vars = load_and_preprocess_data()
    if tte_data is None:
        return

    # 2. 分析两个endpoint
    endpoints = ['ICU28D', 'HOSPXXD']

    for endpoint in endpoints:
        print(f"\n{'=' * 20} 分析 {endpoint} {'=' * 20}")

        # 准备数据
        X, time, event = prepare_survival_data(tte_data, feature_vars, endpoint)
        if X is None:
            print(f"⚠️ 跳过 {endpoint} 分析")
            continue

        # 训练模型
        model, X_train, X_test, y_train, y_test = train_xgboost_survival(X, time, event)

        # 计算SHAP值
        explainer, shap_values = calculate_shap_values(model, X_train, X_test)

        # 创建可视化
        create_shap_visualizations(explainer, shap_values, X_test, endpoint, output_dir)

        # 生成报告
        feature_stats = generate_shap_report(shap_values, X_test, endpoint, output_dir)

    print(f"\n🎉 分析完成！所有结果已保存到 {output_dir} 目录")
    print("📁 输出文件包括:")
    print("   - SHAP特征重要性图")
    print("   - SHAP汇总图")
    print("   - 特征依赖图")
    print("   - 局部解释图（瀑布图、决策图）")
    print("   - 特征相互作用热力图")
    print("   - 分析报告CSV文件")


if __name__ == "__main__":
    main()

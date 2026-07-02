#!/bin/bash

# ============================================================================
# 配置信息
# ============================================================================
# 官方 HTTPS 仓库地址
REPO_URL="https://github.com/slren95/RSV_burden_by_country_LMIC.git"
TARGET_DIR="/srv/shiny-server/rsvburdenlmic"
TEMP_DIR="/tmp/rsv_repo_download"

# 确保脚本遇到任何错误时立即停止执行，防止误删或错误覆盖
set -e

echo "🚀 开始从官方仓库更新 Shiny 应用程序..."

# 1. 清理可能残余的旧临时目录
if [ -d "$TEMP_DIR" ]; then
    echo "🧹 清理上一次的临时下载目录..."
    rm -rf "$TEMP_DIR"
fi

# 2. 克隆仓库到临时目录
echo "📦 正在从 GitHub 下载最新代码..."
git clone "$REPO_URL" "$TEMP_DIR"

# 3. 检查下载的项目里是否存在 shiny 文件夹
if [ ! -d "$TEMP_DIR/shiny" ]; then
    echo "❌ 错误: 仓库中未找到 'shiny' 文件夹，请检查目录结构！"
    rm -rf "$TEMP_DIR"
    exit 1
fi

# 4. 删除 shiny-server 下已有的旧版本程序（如果存在）
if [ -d "$TARGET_DIR" ]; then
    echo "🗑️ 发现已有旧版本，正在移除: $TARGET_DIR"
    sudo rm -rf "$TARGET_DIR"
fi

# 🌟 确保父级目录 /srv/shiny-server/ 存在，即使 TARGET_DIR 首次创建也不会报错
sudo mkdir -p /srv/shiny-server/

# 5. 将 shiny 文件夹移动到目标位置并重命名
echo "🚚 正在部署 shiny 文件夹至: $TARGET_DIR"
sudo mv "$TEMP_DIR/shiny" "$TARGET_DIR"

# 6. 清理临时下载的其余所有仓库文件
echo "🧼 正在清理其余多余仓库文件..."
rm -rf "$TEMP_DIR"

# 7. 修正文件权限（确保 shiny 用户组有权运行，避免 404/500 报错）
echo "🔒 正在优化 shiny-server 目录权限..."
sudo chown -R shiny:shiny "$TARGET_DIR"
sudo chmod -R 755 "$TARGET_DIR"

echo "✨ 部署成功！"
echo "🌐 你的应用已就绪，请通过 http://你的服务器IP:3838/rsvburdenlmic/ 访问。"
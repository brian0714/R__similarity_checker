/**
 * 計算 n-gram 的頻率
 * @param {string[]} documents - 所有文件的文字數組
 * @param {number} n - n-gram 的長度
 * @returns {object} - 包含 n-gram 的頻率字典
 */
export function calculateNGramFrequency(documents, n) {
    const nGramDict = {}; // 用於存儲 n-gram 和其頻率的字典

    documents.forEach(doc => {
        // 將文字以空格切分
        const words = doc.split(" ");
        if (words.length < n) return; // 如果詞數不足 n，跳過此文件

        // 提取 n-gram
        for (let i = 0; i <= words.length - n; i++) {
            const nGram = words.slice(i, i + n).join(" ");
            nGramDict[nGram] = (nGramDict[nGram] || 0) + 1;
        }
    });

    return nGramDict;
}

/**
 * 過濾並打印符合條件的 n-gram
 * @param {object} nGramDict - n-gram 頻率字典
 * @param {number} documentCount - 文件數量
 */
function printFrequentNGrams(nGramDict, documentCount) {
    console.log("Frequent n-grams with frequency >= document count:");
    for (const [key, value] of Object.entries(nGramDict)) {
        if (value >= documentCount) {
            console.log(`"${key}": ${value}`);
        }
    }
}

// 主程序
// 定義讀取的 cluster 和 document 資料
// const clusterData = [
//     "I like green apple.",
//     "I like green kiwi."
// ]; // 測試用的文字資料
// function main() {
//     const n = 3; // 設置 n-gram 的長度
//     const documentCount = clusterData.length; // 文件總數

//     // 計算 n-gram 頻率
//     const nGramDict = calculateNGramFrequency(clusterData, n);

//     // 過濾並打印符合條件的 n-gram
//     printFrequentNGrams(nGramDict, documentCount);
// }

// // 執行主程序
// main();

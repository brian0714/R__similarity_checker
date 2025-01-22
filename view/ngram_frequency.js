/**
 * 計算 n-gram 的頻率（根據出現在不同文件中的次數計算）
 * @param {string[]} documents - 所有文件的文字數組
 * @param {number} n - n-gram 的長度
 * @returns {object} - 包含 n-gram 的頻率字典
 */
export function calculateNGramFrequency(documents, n) {
    const nGramDict = {}; // 用於存儲 n-gram 和其在不同文件中出現的次數

    documents.forEach(doc => {
        const words = doc.split(" ");
        if (words.length < n) return; // 如果詞數不足 n，跳過此文件

        const nGramSet = new Set(); // 用於記錄該文件中的 n-grams

        // 提取 n-gram
        for (let i = 0; i <= words.length - n; i++) {
            const nGram = words.slice(i, i + n).join(" ");
            nGramSet.add(nGram); // 加入該文件的 n-gram 集合
        }

        // 將 n-gram 集合更新到全局字典中
        nGramSet.forEach(nGram => {
            nGramDict[nGram] = (nGramDict[nGram] || 0) + 1;
        });
    });

    return nGramDict;
}

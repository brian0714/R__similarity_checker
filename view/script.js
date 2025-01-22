// 引入 ngram_frequency.js 中的函數
import { calculateNGramFrequency } from './ngram_frequency.js';

const homeButton = document.getElementById('home-button');
const menuButton = document.getElementById('menu-button');
const sidebar = document.getElementById('sidebar');
const clusterList = document.getElementById('cluster-list');
const content = document.getElementById('content');
const jsonPath = "data/json/CREATIVE_clusters_202501230455.json";
const csvPath = "../data/text_data/extracted_behavior_pattern_data.csv";

// 全局變量
window.currentClusterIndex = null; // 保存目前的 Cluster 索引
window.currentClusterData = null; // 保存目前的 Cluster 資料
window.lastNGramValue = null; // 保存上一次選擇的 n-gram 值
window.lastNGramDict = {}; // 保存上一次的 n-gram 結果

// 為標題按鈕添加點擊事件，回到首頁
homeButton.addEventListener('click', showHome);

// Toggle Sidebar
menuButton.addEventListener('click', () => {
    sidebar.classList.toggle('active');
});

// Fetch JSON and CSV Data
Promise.all([fetch(jsonPath), fetch(csvPath)])
    .then(async ([jsonResponse, csvResponse]) => {
        if (!jsonResponse.ok || !csvResponse.ok) {
            throw new Error(`HTTP error! status: JSON ${jsonResponse.status}, CSV ${csvResponse.status}`);
        }
        const jsonData = await jsonResponse.json();
        const csvText = await csvResponse.text();

        // 使用 PapaParse 解析 CSV 數據
        const csvData = Papa.parse(csvText, {
            header: true,
            skipEmptyLines: true,
        }).data;

        console.log("Parsed JSON Data:", jsonData);
        console.log("Parsed CSV Data (First 5 Rows):", csvData.slice(0, 5));

        const clustersWithDocuments = transformClusters(jsonData, csvData);
        populateSidebar(clustersWithDocuments);
    })
    .catch(error => {
        console.error('Error loading data:', error);
        content.innerHTML = `<section><h2>Error</h2><p>Failed to load the document clusters.</p></section>`;
    });

// Transform Clusters with CSV Data
function transformClusters(jsonData, csvData) {
    return {
        clusters: jsonData,
        csvData: csvData
    };
}

// Populate Sidebar
function populateSidebar(data) {
    const { clusters, csvData } = data;
    clusters.forEach((cluster, index) => {
        const li = document.createElement('li');
        const link = document.createElement('a');
        link.href = `#cluster_${index + 1}`;
        link.textContent = `Cluster ${index + 1}`;
        link.addEventListener('click', () => showCluster(cluster, index + 1, csvData));
        li.appendChild(link);
        clusterList.appendChild(li);
    });
}

// Show Cluster Content
function showCluster(cluster, clusterIndex, csvData) {
    currentClusterIndex = clusterIndex;
    currentClusterData = { cluster, csvData };
    window.currentClusterIndex = currentClusterIndex;
    window.currentClusterData = currentClusterData;

    // 重置 n-gram 狀態
    window.lastNGramValue = null;
    window.lastNGramDict = {};

    const documents = cluster.map(id => {
        const row = csvData.find(row => String(row.user_id).trim() === String(id).trim());
        return row ? escapeHTML(row.final_submission) : null;
    }).filter(Boolean);

    content.innerHTML = `
        <section>
            <h2>Cluster ${clusterIndex}</h2>
            <hr>
            <div>
                <label for="ngram-select">Select Gram:</label>
                <select id="ngram-select">
                    <option value="" selected disabled>Select n-gram</option>
                    <option value="2">Gram = 2</option>
                    <option value="3">Gram = 3</option>
                </select>
            </div>
            <hr>
            <div id="documents-container">
                ${cluster.map((id) => {
                    const row = csvData.find(row => String(row.user_id).trim() === String(id).trim());
                    const doc = row ? row.final_submission : `Document for ID: ${id} (No match!)`;
                    return `<button class="doc-button" data-id="${id}" data-doc="${escapeHTML(doc)}">Document ${id}</button>`;
                }).join('')}
            </div>
            <hr>
            <button class="back-button" onclick="showHome()">Back to Home</button>
        </section>
    `;

    // Attach event listener for select dropdown
    const nGramSelect = document.getElementById('ngram-select');
    nGramSelect.addEventListener('change', () => {
        const selectedValue = nGramSelect.value;

        if (!selectedValue) {
            alert("Please select a gram value.");
            return;
        }

        const n = parseInt(selectedValue, 10);
        window.lastNGramValue = n; // 保存 n 的值
        window.lastNGramDict = calculateNGramFrequency(documents, n); // 保存 n-gram 結果
        console.log(`N-Gram Frequencies for Gram = ${n}:`, window.lastNGramDict);
    });

    // Attach event listeners to document buttons
    document.querySelectorAll('.doc-button').forEach(button => {
        button.addEventListener('click', () => {
            const doc = button.getAttribute('data-doc');
            const id = button.getAttribute('data-id');
            showDocument(doc, id);
        });
    });
}

// Highlight text in document
function highlightText(text, nGramDict, threshold) {
    const words = text.split(" ");
    const n = Math.max(...Object.keys(nGramDict).map(gram => gram.split(" ").length));
    const highlightedText = [];

    for (let i = 0; i < words.length; i++) {
        let matchFound = false;

        for (let j = n; j > 0; j--) {
            if (i + j <= words.length) {
                const nGram = words.slice(i, i + j).join(" ");
                if (nGramDict[nGram] >= threshold) {
                    highlightedText.push(`<span class="highlight">${nGram}</span>`);
                    i += j - 1;
                    matchFound = true;
                    break;
                }
            }
        }

        if (!matchFound) {
            highlightedText.push(words[i]);
        }
    }

    return highlightedText.join(" ");
}

// Show Document Content with highlighting
function showDocument(doc, id) {
    const threshold = 2;
    const highlightedContent = highlightText(doc, window.lastNGramDict, threshold);

    content.innerHTML = `
        <section>
            <h2>Document ${id}</h2>
            <hr>
            <p>${highlightedContent}</p>
            <hr>
            <button class="back-button" onclick="showCluster(window.currentClusterData.cluster, window.currentClusterIndex, window.currentClusterData.csvData)">Back to Cluster ${window.currentClusterIndex}</button>
        </section>
    `;
}

// Show Home Page
function showHome() {
    currentClusterIndex = null;
    currentClusterData = null;

    content.innerHTML = `
        <section>
            <h2>Welcome</h2>
            <p>This website displays clustered documents based on NLP analysis. Select a cluster from the sidebar to view its documents.</p>
        </section>
    `;
}

// HTML Escape Function
function escapeHTML(str) {
    if (!str) return '';
    return str
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;')
        .replace(/\n/g, '<br>');
}

// 暴露全局函數
window.showHome = showHome;
window.showCluster = showCluster;

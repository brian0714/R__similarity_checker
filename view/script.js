// 引入 ngram_frequency.js 中的函數
import { calculateNGramFrequency } from './ngram_frequency.js';

const homeButton = document.getElementById('home-button');
const menuButton = document.getElementById('menu-button');
const sidebar = document.getElementById('sidebar');
const clusterList = document.getElementById('cluster-list');
const content = document.getElementById('content');
const jsonPath = "data/json/CREATIVE_clusters_202501230455.json";
const csvPath = "../data/text_data/extracted_behavior_pattern_data.csv";

let currentClusterIndex = null; // 保存目前的 Cluster 索引
let currentClusterData = null; // 保存目前的 Cluster 資料

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

        // Use PapaParse to parse CSV data
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
    currentClusterIndex = clusterIndex; // 保存目前 Cluster 索引
    currentClusterData = { cluster, csvData }; // 保存目前 Cluster 資料

    // 收集每個 document 的文字內容
    const documents = cluster.map(id => {
        const row = csvData.find(row => String(row.user_id).trim() === String(id).trim());
        return row ? escapeHTML(row.final_submission) : null; // 只收集非空的內容
    }).filter(Boolean); // 過濾掉 null 或 undefined 的值

    // 計算 n-gram 頻率
    const n = 3; // 設置 n-gram 的長度
    const nGramDict = calculateNGramFrequency(documents, n);
    console.log("N-Gram Frequencies:", nGramDict);

    content.innerHTML = `
        <section>
            <h2>Cluster ${clusterIndex}</h2>
            <hr>
            <div>
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

    // Attach event listeners to buttons
    document.querySelectorAll('.doc-button').forEach(button => {
        button.addEventListener('click', () => {
            const doc = button.getAttribute('data-doc');
            const id = button.getAttribute('data-id');
            showDocument(doc, id);
        });
    });
}

// Show Document Content
function showDocument(doc, id) {
    content.innerHTML = `
        <section>
            <h2>Document ${id}</h2>
            <hr>
            <p>${doc}</p>
            <hr>
            <button class="back-button" onclick="showCluster(currentClusterData.cluster, currentClusterIndex, currentClusterData.csvData)">Back to Cluster</button>
        </section>
    `;
}

// Show Home Page
function showHome() {
    currentClusterIndex = null; // 清空目前 Cluster 狀態
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

// 引入 ngram_frequency.js 中的函數
import { calculateNGramFrequency } from './ngram_frequency.js';

const homeButton = document.getElementById('home-button');
const menuButton = document.getElementById('menu-button');
const sidebar = document.getElementById('sidebar');
const creativeDataButton = document.getElementById('creative-data-button');
const practicalDataButton = document.getElementById('practical-data-button');
const creativeClusterList = document.getElementById('creative-cluster-list');
const practicalClusterList = document.getElementById('practical-cluster-list');
const content = document.getElementById('content');

// JSON Paths
const creativeJsonPath = "data/json/CREATIVE_clusters_202501230455.json";
const practicalJsonPath = "data/json/PRACTICAL_clusters_202501230447.json";
const csvPath = "../data/text_data/extracted_behavior_pattern_data.csv";

// 全局變量
window.currentDataset = null; // 保存當前數據集 ("creative" 或 "practical")
window.currentClusterIndex = null; // 保存目前的 Cluster 索引
window.currentClusterData = null; // 保存目前的 Cluster 資料
window.clusterStates = {}; // 保存每個 Cluster 的 n-gram 狀態
window.csvData = []; // 保存解析後的 CSV 數據

// 初始化
homeButton.addEventListener('click', showHome);
menuButton.addEventListener('click', () => sidebar.classList.toggle('active'));
creativeDataButton.addEventListener('click', () => toggleClusterList('creative'));
practicalDataButton.addEventListener('click', () => toggleClusterList('practical'));

// Fetch CSV Data
fetch(csvPath)
    .then(response => {
        if (!response.ok) throw new Error(`HTTP error! status: ${response.status}`);
        return response.text();
    })
    .then(csvText => {
        window.csvData = Papa.parse(csvText, {
            header: true,
            skipEmptyLines: true,
        }).data;
        console.log("Parsed CSV Data (First 5 Rows):", window.csvData.slice(0, 5));
    })
    .catch(error => console.error("Error loading CSV data:", error));

// 展開/收起 Cluster List
function toggleClusterList(dataset) {
    const list = dataset === 'creative' ? creativeClusterList : practicalClusterList;
    const jsonPath = dataset === 'creative' ? creativeJsonPath : practicalJsonPath;

    if (list.classList.contains('hidden')) {
        fetchClusterData(jsonPath, list, dataset);
        list.classList.remove('hidden');
    } else {
        list.classList.add('hidden');
    }
}

// Fetch Cluster Data
function fetchClusterData(jsonPath, listElement, dataset) {
    fetch(jsonPath)
        .then(response => {
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            return response.json();
        })
        .then(jsonData => {
            populateClusterList(jsonData, listElement, dataset);
        })
        .catch(error => {
            console.error('Error loading cluster data:', error);
        });
}

// Populate Cluster List
function populateClusterList(jsonData, listElement, dataset) {
    listElement.innerHTML = ""; // 清空原有的列表
    jsonData.forEach((cluster, index) => {
        const li = document.createElement('li');
        const link = document.createElement('a');
        link.href = `#${dataset}_data/#cluster_${index + 1}`;
        link.textContent = `Cluster ${index + 1}`;
        link.addEventListener('click', () => showCluster(cluster, index + 1, dataset));
        li.appendChild(link);
        listElement.appendChild(li);

        // 初始化 cluster 狀態
        const clusterKey = `${dataset}_cluster_${index + 1}`;
        if (!window.clusterStates[clusterKey]) {
            window.clusterStates[clusterKey] = {
                nGramValue: null,
                nGramDict: {}
            };
        }
    });
}

// Show Cluster Content
function showCluster(cluster, clusterIndex, dataset) {
    currentClusterIndex = clusterIndex;
    currentClusterData = { cluster, dataset };
    window.currentDataset = dataset;

    const clusterKey = `${dataset}_cluster_${clusterIndex}`;
    const clusterState = window.clusterStates[clusterKey];

    const documents = cluster.map(id => {
        const row = window.csvData.find(row => String(row.user_id).trim() === String(id).trim());
        return row ? escapeHTML(row.final_submission) : null;
    }).filter(Boolean);

    content.innerHTML = `
        <section>
            <h2>${dataset === 'creative' ? 'Creative Data' : 'Practical Data'} - Cluster ${clusterIndex}</h2>
            <hr>
            <div>
                <label for="ngram-select">Select Gram:</label>
                <select id="ngram-select">
                    <option value="" selected disabled>Select n-gram</option>
                    <option value="2" ${clusterState.nGramValue === 2 ? "selected" : ""}>Gram = 2</option>
                    <option value="3" ${clusterState.nGramValue === 3 ? "selected" : ""}>Gram = 3</option>
                </select>
            </div>
            <hr>
            <div id="documents-container">
                ${cluster.map((id) => {
                    const row = window.csvData.find(row => String(row.user_id).trim() === String(id).trim());
                    const doc = row ? row.final_submission : `Document for ID: ${id} (No match!)`;
                    return `<button class="doc-button" data-id="${id}" data-doc="${escapeHTML(doc)}">Document ${id}</button>`;
                }).join('')}
            </div>
            <hr>
            <button class="back-button" onclick="showHome()">Back to Home</button>
        </section>
    `;

    // Attach event listener for n-gram dropdown
    const nGramSelect = document.getElementById('ngram-select');
    nGramSelect.addEventListener('change', () => {
        const selectedValue = nGramSelect.value;

        if (!selectedValue) {
            alert('Please select a gram value.');
            return;
        }

        const n = parseInt(selectedValue, 10);
        clusterState.nGramValue = n; // 保存 n 的值
        clusterState.nGramDict = calculateNGramFrequency(documents, n);
        console.log(`N-Gram Frequencies for Gram = ${n}:`, clusterState.nGramDict);
    });

    // Attach event listeners to document buttons
    document.querySelectorAll('.doc-button').forEach(button => {
        button.addEventListener('click', () => {
            const id = button.getAttribute('data-id');
            const doc = button.getAttribute('data-doc');
            showDocument(doc, id, clusterKey);
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
function showDocument(doc, id, clusterKey) {
    const clusterState = window.clusterStates[clusterKey];
    if (!clusterState.nGramValue || !Object.keys(clusterState.nGramDict).length) {
        alert("Please calculate n-grams first.");
        return;
    }

    const threshold = 2;
    const highlightedContent = highlightText(doc, clusterState.nGramDict, threshold);

    content.innerHTML = `
        <section>
            <h2>Document ${id}</h2>
            <hr>
            <p>${highlightedContent}</p>
            <hr>
            <button class="back-button" onclick="showCluster(window.currentClusterData.cluster, window.currentClusterIndex, window.currentClusterData.dataset)">Back to Cluster ${window.currentClusterIndex}</button>
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
            <p>This website displays clustered documents based on NLP analysis. Select a dataset from the sidebar to explore its clusters and documents.</p>
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

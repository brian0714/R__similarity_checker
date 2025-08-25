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
// const creativeJsonPath = "data/json/CREATIVE_clusters_202501230455.json";
// const creativeJsonPath = "data/json/CREATIVE_levenshtein_clusters_202503270411.json202503270411.json";
// const creativeJsonPath = "data/json/CREATIVE_winnowing_by_char_clusters_202503270645.json202503270645.json";
const creativeJsonPath = "data/json/CREATIVE_HC_cosine_clusters_202503271437.json";

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
creativeDataButton.addEventListener('click', () => toggleDatasetView('creative'));
practicalDataButton.addEventListener('click', () => toggleDatasetView('practical'));

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

// 切換數據集視圖
function toggleDatasetView(dataset) {
    const list = dataset === 'creative' ? creativeClusterList : practicalClusterList;
    const jsonPath = dataset === 'creative' ? creativeJsonPath : practicalJsonPath;

    window.currentDataset = dataset;

    if (list.classList.contains('hidden')) {
        // 展開列表並顯示描述
        fetchClusterData(jsonPath, list, dataset);
        list.classList.remove('hidden');
    }
    showDatasetDescription(dataset);
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

    // 如果只有一份文件，禁用 n-gram 選擇功能
    const nGramSection = documents.length > 1 ? `
        <div>
            <label for="ngram-select">Select Gram:</label>
            <select id="ngram-select">
                <option value="" selected disabled>Select n-gram</option>
                <option value="2" ${clusterState.nGramValue === 2 ? "selected" : ""}>Gram = 2</option>
                <option value="3" ${clusterState.nGramValue === 3 ? "selected" : ""}>Gram = 3</option>
            </select>
        </div>` : `
        <p class="ngram-disabled">N-gram analysis is unavailable as this cluster contains only one document.</p>`;

    content.innerHTML = `
        <section>
            <h2>${dataset === 'creative' ? 'Creative Data' : 'Practical Data'} - Cluster ${clusterIndex}</h2>
            <hr>
            ${nGramSection}
            <hr>
            <div id="documents-container">
                ${cluster.map((id) => {
                    const row = window.csvData.find(row => String(row.user_id).trim() === String(id).trim());
                    const doc = row ? row.final_submission : `Document for ID: ${id} (No match!)`;
                    return `<button class="doc-button" data-id="${id}" data-doc="${escapeHTML(doc)}">Document ${id}</button>`;
                }).join('')}
            </div>
            <hr>
            <button class="back-button" onclick="showDatasetDescription('${dataset}')">Back to Dataset</button>
        </section>
    `;

    if (documents.length > 1) {
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
            console.log(`Cluster ${clusterIndex} N-Gram Frequencies for Gram = ${n}:`, clusterState.nGramDict);
        });
    }

    document.querySelectorAll('.doc-button').forEach(button => {
        button.addEventListener('click', () => {
            const id = button.getAttribute('data-id');
            const doc = button.getAttribute('data-doc');
            showDocument(doc, id, clusterKey);
        });
    });
}

// Show Dataset Description
function showDatasetDescription(dataset) {
    const description = dataset === 'creative' ? `
        <tr>
            <th>Creative Task Description</th>
            <td>You are now collaborating with a world-renowned design team. The team's goal is to design the experience for travelers, for a futuristic, innovative, high-tech airport in Tokyo due to be built in the year 2050. The team decided to draw upon your extensive travel experience and inclination for cutting-edge technology. They seek your help; their team propose an interesting experience for travelers using this airport. Your task is divided into two parts.<br><br>
            <strong>Part 1:</strong> Please write a story of an ideal experience a traveler might have in using this futuristic airport. The story should focus on what a traveler might see and experience in the airport, and how it will make them feel. This futuristic experience can be creative and does not need to be limited to today’s technologies; instead, focus on achieving your ideal experience. The story should be 100–200 words.<br><br>
            <strong>Part 2:</strong> Create three catchy marketing slogans that encapsulate the unique futuristic experiences mentioned in your story. Each of the three slogans should be within 2–10 words.
            </td>
        </tr>
    ` : `
        <tr>
            <th>Practical Task Description</th>
            <td>You are preparing a post for your Facebook fan page that will guide firsttime travelers successfully through the complicated process at a major airport in Japan to travel. The Facebook post will be divided into two parts.<br><br>
            <strong>Part 1:</strong> Create a concise, step-by-step guide that a traveler needs to follow from the moment they arrive at the airport until they board their plane. A traveler should be able to navigate the airport experience following only your guide. The guide should be 100–200 words.<br><br>
            <strong>Part 2:</strong> Highlight the top three bits of advice you would give travelers to prevent the most common mistakes they might make. Each of the three bits of advice should be no more than 1 sentence (three sentences total).
            </td>
        </tr>
    `;

    window.location.hash = `${dataset}_data`;

    if (dataset === 'creative') {
        creativeClusterList.classList.remove('hidden');
        practicalClusterList.classList.add('hidden');
    } else {
        practicalClusterList.classList.remove('hidden');
        creativeClusterList.classList.add('hidden');
    }

    content.innerHTML = `
        <section>
            <h2>${dataset === 'creative' ? 'Creative Data' : 'Practical Data'}</h2>
            <hr>
            <table class="description-table">
            <tr>
                <th>Common Background Scenario</th>
                <td>You are a prominent online influencer specializing in remote work and travel. You like to work remotely in different countries and experience different cultures. In addition, you also enjoy the innovative experience that cutting-edge technology brings you in your travels.</td>
            </tr>
            ${description}
            </table>
            <hr>
            <p> ⬅️ Select a cluster from the sidebar to view its documents.</p>
        </section>
    `;
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

// Get Cluster Document Count
function getClusterDocumentCount(clusterKey) {
    // 分解 clusterKey，例如 "creative_cluster_1"
    const [dataset, , clusterIndex] = clusterKey.split("_");
    const jsonPath = dataset === "creative" ? creativeJsonPath : practicalJsonPath;

    return new Promise((resolve, reject) => {
        // 從對應 JSON 文件中獲取資料
        fetch(jsonPath)
            .then(response => {
                if (!response.ok) {
                    throw new Error(`Failed to load JSON: ${response.status}`);
                }
                return response.json();
            })
            .then(jsonData => {
                // 尋找指定的 cluster，並返回 document 數量
                const clusterData = jsonData[parseInt(clusterIndex) - 1]; // Cluster 索引是從 1 開始的
                if (clusterData) {
                    resolve(clusterData.length); // 返回文件數量
                } else {
                    resolve(0); // 如果找不到對應的 cluster，返回 0
                }
            })
            .catch(error => {
                console.error("Error fetching cluster data:", error);
                reject(error);
            });
    });
}

// Show Document Content with highlighting
async function showDocument(doc, id, clusterKey) {
    const clusterState = window.clusterStates[clusterKey];
    let one_doc_in_cluster = false;

    const count = await getClusterDocumentCount(clusterKey);

    if (count == null) {
        console.log("Failed to retrieve document count.");
    } else if (count === 1) {
        one_doc_in_cluster = true;
    } else if (count > 1) {
        one_doc_in_cluster = false;
    }

    console.log("one_doc_in_cluster: ", one_doc_in_cluster);

    if ((!clusterState.nGramValue || !Object.keys(clusterState.nGramDict).length) && (!one_doc_in_cluster)) {
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
    window.currentDataset = null;

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
window.showDatasetDescription = showDatasetDescription;

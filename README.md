# R__Similarity_Checker

## Overview
R__Similarity_Checker is a tool designed to compare the similarity between text and code. It includes multiple functions for NLP processing, similarity computation, matrix visualization, and clustering. Additionally, it provides an interactive web interface for exploring clustering results.

## Project Structure

### 📂 `lib`
Contains core R functions for:
- NLP processing
- Similarity function computation
- CSV file reading and writing
- Clustering and visualization
- Matrix visualization

### 📂 `output`
Stores execution results such as:
- CSV, Excel, or JSON files
- Generated visualizations from clustering and matrix processing

### 📂 `view`
A front-end interface for visualizing clustering results from `cluster.json`. It includes:
- HTML, CSS, and JavaScript for rendering interactive charts
- A simple NLP-based interactive user experience (e.g., n-gram selection)
- To open the web; Type `python3 -m http.server 8000` in the command to run the html page on web browser

### 📂 `deploy_project`
Contains the final version of the web visualization for deployment.

---

## Usage Guide

### Step 1: Generate Similarity Matrix
Use `similarity_matrix_generator.R` to create a similarity matrix by selecting a similarity method. Optionally, apply an NLP method or use the default settings. The script outputs a similarity matrix file.

Example similarity matrix:

|     | id1  | id2  | id3  |
|-----|------|------|------|
| id1 | 1.00 | 0.95 | 0.65 |
| id2 | 0.95 | 1.00 | 0.50 |
| id3 | 0.65 | 0.50 | 1.00 |

---

### Step 2: Cluster Visualization
Use `matrix_visualization.R` to read the similarity matrix file.

(Perform hierarchical clustering currently)

1. Choose a **linkage method** (e.g., `average`).
2. Select a **clustering evaluation metric** for determine the optimal number k of clusters (e.g., `Silhouette scores` or `Elbow method`).
3. Generate a **dendrogram with clustering results**.

Note: Step 2 will be concluded in step 1 in the future.

---

### Step 3 (Optional): Generate Cluster JSON for Web Visualization
If you need a web-compatible clustering result, run `cluster_stat.R` to save clustering results as a JSON file. This JSON file helps the web interface display which documents belong to each cluster.

---

## Additional Notes
### Term-Document Matrix (TDM)
The `term_document_matrix_generator.R` script is under development. It may contribute to:
- Generating a **Term-Document Matrix (TDM)**
- Visualizing **word frequency relationships across documents**
- Creating **word clouds** or conducting **LDA topic modeling**

Example TDM:

|       | doc1 | doc2 | doc3 |
|-------|------|------|------|
| word1 |  3   |  1   |  0   |
| word2 |  0   |  2   |  5   |
| word3 |  1   |  0   |  4   |

---

## License
This project is licensed under the MIT License.

---

## Contact
For any inquiries or contributions, feel free to open an issue on GitHub.

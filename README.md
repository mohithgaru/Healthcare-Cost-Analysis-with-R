<div align="center">

<h1 style="font-weight:700; font-size:36px; margin-bottom:6px;
background: linear-gradient(90deg, #0EA5E9, #6366F1);
-webkit-background-clip:text; color:transparent;">
Healthcare Cost Analysis — R Project
</h1>

<p style="color:#808b9c; font-size:15px; margin-top:0;">
A complete statistical investigation of inpatient healthcare costs using R.
</p>

<hr style="width:55%; border:none; height:2px;
background: linear-gradient(90deg, #0EA5E9, #6366F1, #0EA5E9);
border-radius:4px;" />

</div>

## 🩺 Project Summary
This project analyzes **hospital cost drivers** using a real healthcare dataset (ages 0–17).  
It includes data cleaning, exploratory summaries, ANOVA testing, modeling, and insights.  
**All necessary files are attached below — no need to search inside folders.**

---

## 🔗 Quick Access to All Files (Replace With Your Links)

### 📄 **Final Report (PDF)**
- **Full Analysis Report:** `ADD_LINK_HERE`

### 📊 **Plots / Visual Outputs**
- **All Charts & Visualizations:** `ADD_LINK_HERE`

### 🧠 **R Code**
- **Scripts Folder:** `ADD_LINK_HERE`
- **Main Script (Run All):** `ADD_LINK_HERE`

### 📁 **Dataset**
- **hospitalcosts.csv:** `ADD_LINK_HERE`

---

## 📌 What This Project Proves (Proof of Work)
This README gives the **final results**, so viewers don’t have to open each dataset or script manually.

### ✔ Cleaned & validated the dataset  
→ Handled missing values, validated ranges, and removed inconsistencies.

### ✔ Identified the **highest-cost patient groups**  
- Age **0 (newborns)** = highest volume and total cost  
- Diagnosis **APRDRG 640** = highest total cost group  

### ✔ Verified malpractice claims (race-based cost differences)  
- ANOVA p-value: **0.94**  
- **Conclusion:** No race impacts hospital charges  

### ✔ Built predictive models  
- LOS prediction → weak relationship  
- **Cost prediction model → R² ≈ 0.96** (excellent)  
- Top predictors:  
  - **Length of Stay (LOS)**  
  - **Diagnosis (APRDRG)**  
  - **Age**

### ✔ Created clear visualizations  
- Cost distribution  
- LOS patterns  
- Diagnosis category impact  
- Regression residuals & diagnostics  

### ✔ Delivered a clean, structured analysis  
Perfect for academic submission, portfolio review, or employer showcase.

---

## 📈 Key Findings (Short + Clear)

- **Newborns** represent ~60% of hospitalizations + highest cost  
- **APRDRG 640** is the most expensive and most frequent diagnosis  
- **Race does NOT influence costs** (no malpractice evidence)  
- **Age, race, gender do NOT predict LOS**  
- **Cost is explained by clinical factors (LOS, diagnosis)**  
- **Model explains 96% of variance**  

---

## 🛠️ How to Run the Project

```r
# install required libraries
install.packages(c("dplyr", "ggplot2", "car"))

# run main script
source("scripts/main.R")

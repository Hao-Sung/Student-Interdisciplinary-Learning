# 學生跨領域學習行為分析

本專案使用學生修課紀錄，以及課程的相關資訊，探討學生跨領域學習的意向與模式，並進行基礎的分析。

架構上，`functionality資料夾` 涵蓋統計分析需要的所有函數與功能；`analysis.R` 則為分析實際運行的程式腳本；而 `figs資料夾` 儲存所有圖像產出。

⚠️⚠️⚠️ 值得注意的是，本專案所使用的資料，經過「加密」、「大量刪減」、「隨機抽樣」等處理流程，**成果僅具展示的意義，並非實際結果**。

## 如何開始

進入專案後，在控制台運行以下代碼：

```r
renv::init
renv::restore()
```

再執行 `analysis.R` 即可。

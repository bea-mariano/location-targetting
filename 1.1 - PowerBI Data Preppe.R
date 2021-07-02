# Author: Daniel Joseph Tan

# Load Packages ====

library(data.table)
library(dplyr)
library(plyr)
library(stringr)
library(lubridate)
library(tidyr)

#### Load Datasets ####

sales_reg <- fread(
  file = "./Data/OUTPUT_SALESREG.csv"
)
dim(sales_reg)
names(sales_reg)

cmdpsr <- fread(
  file = "./Data/OUTPUT_CMDPSR.csv"
)
dim(cmdpsr)
names(cmdpsr)

# Hardcoded Branch-Zone df
allBZ <- data.frame(
  Branch = c("CDO", "BUT", "OZA", "ZAM", "DAV", "TAG", "GEN", "CMB", "PAG", "CAL", "BAT", "NAG", "LEG", "MAS", "BAE", "MKT", "MRA", "MRB", "VAL", "BAN", "CAU", "TUG", "VIL", "BAC", "DUM", "ILO", "ROX", "MAN", "TSY", "TAC", "TGB", "BCR", "DAS", "SCT", "PAL", "TAL", "TAR", "PAM"),
  Zone = c("NMIN", "NMIN", "NMIN", "NMIN", "SMIN", "SMIN", "SMIN", "STAG", "STAG", "STAG", "STAG", "BICOL", "BICOL", "BICOL", "NGMA", "NGMA", "NGMA", "NGMA", "NGMA", "NLUZ", "NLUZ", "NLUZ", "NLUZ", "WVIS", "WVIS", "WVIS", "WVIS", "CEVIS", "CEVIS", "CEVIS", "CEVIS", "SGMA", "SGMA", "SGMA", "SGMA", "CLUZ", "CLUZ", "CLUZ")
)

#### Creating the Initial Topsheet ####

Topsheet_init <- cmdpsr %>% 
  select(
    "DIST_CD",
    "FSS_NAME",
    "PSR_CD",
    "PSR_NAME",
    "CUSTOMER_CD",
    "CUSTOMER_NAME",
    
    "No Contact Nos",
    "Same Cust. Name + Brgy. + Municipality",
    "Same Contact Person + Brgy. + Municipality",
    "Same Number + Brgy. + Municipality",
    "Same Geo Coordinates"
  ) %>% distinct() %>% 
  rename(c(
    "DIST_CD" = "BRANCH",
    "CUSTOMER_CD" = "CUST_CD",
    "CUSTOMER_NAME" = "CUST_NAME",
    
    "No Contact Nos" = "No Contact Number",
    "Same Cust. Name + Brgy. + Municipality" = "Same CustmrName +B+M",
    "Same Contact Person + Brgy. + Municipality" = "Same Contact Pn +B+M",
    "Same Number + Brgy. + Municipality" = "Same Number +B+M",
    "Same Geo Coordinates" = "Same Geo Coordinates"
  ))

# Creating the EXC & FRA Indicator Flag Datasets ====

flags_EXCFRA <- sales_reg %>% 
  select(
    "Cust_Code",
    "INVOICE_DATE",
    "INV_NO",
    
    "AMOUNT",
    "DIST_CD",
    "CUST_TYPE",
    
    "INV_TYPE",
    "CUST_NAME",
    
    "TXN_VS_PREVTXN_GPS_ISSUE", 
    "TXN_VS_CMD_GPS", 
    "SAME_DAY_TXN_GPS_DEV", 
    "SAME_GPS_MULT_CUST", 
    "COL_WITH_GPS_ISSUE", 
    "ONE_INVOICE_TRANS", 
    "UNUSUAL_INTERVAL_SALES", 
    "STANDARD_NUM_OF_VISIT_DEV", 
    "COLSALES_ON_DIFFDATE", 
    "OUT_OF_SEQUENCE_TRANS", 
    "Excessive.Sales.per.SKU", 
    "UNUSUAL_INCDEC_SALES", 
    "CASH_CREDIT_SAME_DAY_UNUTIL_CL", 
    "CUST_MORE_THAN_ONE_CASH_TRANS_SAME_DAY", 
    "CASH_TRANS_90DAYS_UNUTIL_CL", 
    "INVOICE_WITH_PARTIAL_PAYMENT"
  ) %>% distinct() %>% 
  rename(c(
    "DIST_CD" = "BRANCH",
    "Cust_Code" = "CUST_CD",
    "INV_NO" = "DOC_NO",
    "INV_TYPE" = "DOC_TYPE",
    "INVOICE_DATE" = "DOC_DT",
    "AMOUNT" = "DOC_AMT",
    
    "TXN_VS_PREVTXN_GPS_ISSUE" = "IND_TXN_VS_PREVTXN_GPS_ISSUE", 
    "TXN_VS_CMD_GPS" = "IND_TXN_VS_CMD_GPS", 
    "SAME_DAY_TXN_GPS_DEV" = "IND_SAME_DAY_TXN_GPS_DEV", 
    "SAME_GPS_MULT_CUST" = "IND_SAME_GPS_MULT_CUST", 
    "COL_WITH_GPS_ISSUE" = "IND_COL_WITH_GPS_ISSUE", 
    "ONE_INVOICE_TRANS" = "IND_ONE_INVOICE_TRANS", 
    "UNUSUAL_INTERVAL_SALES" = "IND_UNUSUAL_INTERVAL_SALES", 
    "STANDARD_NUM_OF_VISIT_DEV" = "IND_STANDARD_NUM_OF_VISIT_DEV", 
    "COLSALES_ON_DIFFDATE" = "IND_COLSALES_ON_DIFFDATE", 
    "OUT_OF_SEQUENCE_TRANS" = "IND_OUT_OF_SEQUENCE_TRANS", 
    "Excessive.Sales.per.SKU" = "IND_EXCESSIVE_SALES_PER_SKU", 
    "UNUSUAL_INCDEC_SALES" = "IND_UNUSUAL_INCDEC_SALES", 
    "CASH_CREDIT_SAME_DAY_UNUTIL_CL" = "IND_CASH_CREDIT_SAME_DAY_UNUTIL_CL", 
    "CUST_MORE_THAN_ONE_CASH_TRANS_SAME_DAY" = "IND_CUST_MORE_THAN_ONE_CASH_TRANS_SAME_DAY", 
    "CASH_TRANS_90DAYS_UNUTIL_CL" = "IND_CASH_TRANS_90DAYS_UNUTIL_CL", 
    "INVOICE_WITH_PARTIAL_PAYMENT" = "IND_INVOICE_WITH_PARTIAL_PAYMENT"
  ))

# Creating the totalFlags Column ====

flags_EXCFRA$totalFlags <- rowSums(flags_EXCFRA[,c(
  "IND_TXN_VS_PREVTXN_GPS_ISSUE", 
  "IND_TXN_VS_CMD_GPS", 
  "IND_SAME_DAY_TXN_GPS_DEV", 
  "IND_SAME_GPS_MULT_CUST", 
  "IND_COL_WITH_GPS_ISSUE", 
  "IND_ONE_INVOICE_TRANS", 
  "IND_UNUSUAL_INTERVAL_SALES", 
  "IND_STANDARD_NUM_OF_VISIT_DEV", 
  "IND_COLSALES_ON_DIFFDATE", 
  "IND_OUT_OF_SEQUENCE_TRANS", 
  "IND_EXCESSIVE_SALES_PER_SKU", 
  "IND_UNUSUAL_INCDEC_SALES", 
  "IND_CASH_CREDIT_SAME_DAY_UNUTIL_CL", 
  "IND_CUST_MORE_THAN_ONE_CASH_TRANS_SAME_DAY", 
  "IND_CASH_TRANS_90DAYS_UNUTIL_CL", 
  "IND_INVOICE_WITH_PARTIAL_PAYMENT"
)], na.rm = T)

flags_EXCFRA$reportDate <- as_datetime( # Get month end datetime
  paste0(
    str_pad(
      year(
        as_datetime(flags_EXCFRA$DOC_DT, "%m/%d/%Y %I:%M:%S %p", tz = "Asia/Kuala_Lumpur")
      ), 
      2, side = "left", pad = "0"
    ),
    "-",
    str_pad(
      month(
        as_datetime(flags_EXCFRA$DOC_DT, "%m/%d/%Y %I:%M:%S %p", tz = "Asia/Kuala_Lumpur")
      ), 
      2, side = "left", pad = "0"
    ),
    "-",
    str_pad(
      days_in_month(
        month(
          as_datetime(flags_EXCFRA$DOC_DT, "%m/%d/%Y %I:%M:%S %p", tz = "Asia/Kuala_Lumpur")
        )
      ), 
      2, side = "left", pad = "0"
    )
  ),
  tz = "Asia/Kuala_Lumpur"
) + days(1) - seconds(1)

flags_EXCFRA %>% ncol() -> colcount
flags_EXCFRA <- flags_EXCFRA[,c(..colcount, 1:(..colcount-1))] # reorder reportDate column
flags_EXCFRA$DCREATED_CD <- as_datetime(
  flags_EXCFRA$DOC_DT, "%m/%d/%Y %I:%M:%S %p", tz = "Asia/Kuala_Lumpur"
) %>% max(na.rm = T)
flags_EXCFRA %>% ncol() -> colcount
flags_EXCFRA <- flags_EXCFRA[,c(..colcount, 1:(..colcount-1))] # reorder DCREATED_CD column

# Split into PDupes, Tx and Indicator Tables ====
Topsheet_init %>% select(c(
  "BRANCH", 
  "FSS_NAME", 
  "PSR_CD", 
  "PSR_NAME", 
  "CUST_CD"
)) -> Topsheet

Topsheet_init %>% select(c(
  "CUST_CD", 
  
  "No Contact Number", 
  "Same CustmrName +B+M", 
  "Same Contact Pn +B+M", 
  "Same Number +B+M", 
  "Same Geo Coordinates"
)) -> TSpdupe_init

flags_EXCFRA %>% select(c(
  "DCREATED_CD", 
  "reportDate", 
  "CUST_CD", 
  "DOC_DT", 
  "DOC_NO", 
  "DOC_AMT", 
  "BRANCH", 
  "CUST_TYPE", 
  "DOC_TYPE", 
  "CUST_NAME", 
  "totalFlags" 
)) -> EXCFRA_TX

flags_EXCFRA %>% select(c(
  "DCREATED_CD", 
  "reportDate", 
  "DOC_NO", 
  
  "IND_TXN_VS_PREVTXN_GPS_ISSUE", 
  "IND_TXN_VS_CMD_GPS", 
  "IND_SAME_DAY_TXN_GPS_DEV", 
  "IND_SAME_GPS_MULT_CUST", 
  "IND_COL_WITH_GPS_ISSUE", 
  "IND_ONE_INVOICE_TRANS", 
  "IND_UNUSUAL_INTERVAL_SALES", 
  "IND_STANDARD_NUM_OF_VISIT_DEV", 
  "IND_COLSALES_ON_DIFFDATE", 
  "IND_OUT_OF_SEQUENCE_TRANS", 
  "IND_EXCESSIVE_SALES_PER_SKU", 
  "IND_UNUSUAL_INCDEC_SALES", 
  "IND_CASH_CREDIT_SAME_DAY_UNUTIL_CL", 
  "IND_CUST_MORE_THAN_ONE_CASH_TRANS_SAME_DAY", 
  "IND_CASH_TRANS_90DAYS_UNUTIL_CL", 
  "IND_INVOICE_WITH_PARTIAL_PAYMENT"    
)) -> EXCFRA_IND

#### Pivot and Finalize Datasets ####

TSpdupe_flag <- pivot_longer(
  TSpdupe_init, 
  c(
    "No Contact Number", 
    "Same CustmrName +B+M", 
    "Same Contact Pn +B+M", 
    "Same Number +B+M", 
    "Same Geo Coordinates"
  ), 
  names_to = "Indicator", values_to = "isFlagged"
)

TSpdupe_flag <- TSpdupe_flag[!is.na(TSpdupe_flag$isFlagged),] # Remove Indicator rows with NA values (not tested; N/A)
TSpdupe_flag <- TSpdupe_flag[TSpdupe_flag$isFlagged>0,] # Remove Indicator rows with no flags

EXCFRA_INDFlag <- pivot_longer(
  EXCFRA_IND, 
  c(
    "IND_TXN_VS_PREVTXN_GPS_ISSUE", 
    "IND_TXN_VS_CMD_GPS", 
    "IND_SAME_DAY_TXN_GPS_DEV", 
    "IND_SAME_GPS_MULT_CUST", 
    "IND_COL_WITH_GPS_ISSUE", 
    "IND_ONE_INVOICE_TRANS", 
    "IND_UNUSUAL_INTERVAL_SALES", 
    "IND_STANDARD_NUM_OF_VISIT_DEV", 
    "IND_COLSALES_ON_DIFFDATE", 
    "IND_OUT_OF_SEQUENCE_TRANS", 
    "IND_EXCESSIVE_SALES_PER_SKU", 
    "IND_UNUSUAL_INCDEC_SALES", 
    "IND_CASH_CREDIT_SAME_DAY_UNUTIL_CL", 
    "IND_CUST_MORE_THAN_ONE_CASH_TRANS_SAME_DAY", 
    "IND_CASH_TRANS_90DAYS_UNUTIL_CL", 
    "IND_INVOICE_WITH_PARTIAL_PAYMENT" 
  ), 
  names_to = "Indicator", values_to = "isFlagged"
)
EXCFRA_INDFlag <- EXCFRA_INDFlag[!is.na(EXCFRA_INDFlag$isFlagged),] # Remove Indicator rows with NA values (not tested; N/A)

fwrite(
  Topsheet,
  "./Output/Topsheet.csv"
)

fwrite(
  TSpdupe_flag,
  "./Output/Topsheet_PDupes.csv"
)

fwrite(
  EXCFRA_INDFlag,
  "./Output/EXCFRA_INDFlag.csv"
)

fwrite(
  EXCFRA_TX,
  "./Output/EXCFRA_TX.csv"
)

fwrite(
  allBZ,
  "./Output/BZones.csv"
)

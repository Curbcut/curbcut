WITH DataVals AS (
  SELECT "ID", "%1$s" AS val
  FROM "%2$s"."%3$s_%4$s"
  WHERE "%1$s" IS NOT NULL AND (%7$s = -1 OR "ID" = '%7$s')
),
ParentVals AS (
  SELECT "ID", "%5$s" AS parent_val
  FROM "%2$s"."%3$s_%6$s"
  WHERE "%5$s" IS NOT NULL AND (%7$s = -1 OR "ID" = '%7$s')
),
BothVals AS (
  SELECT d."ID", d.val, p.parent_val
  FROM DataVals d
  JOIN ParentVals p ON d."ID" = p."ID"
),
WeightedMeanCalc AS (
  SELECT
    SUM(val * parent_val) / NULLIF(SUM(parent_val), 0) AS val
  FROM BothVals
)

SELECT val
FROM WeightedMeanCalc;

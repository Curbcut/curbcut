WITH DataVals AS (
  SELECT "ID", "%1$s" AS val
  FROM %2$s."%3$s_%4$s"
  WHERE (%7$s = -1 OR "ID" = '%7$s')
),
ParentVals AS (
  SELECT "ID", "%5$s" AS parent_val
  FROM %2$s."%3$s_%6$s"
  WHERE (%7$s = -1 OR "ID" = '%7$s')
),
BothVals AS (
  SELECT d."ID", d.val, p.parent_val
  FROM DataVals d
  JOIN ParentVals p ON d."ID" = p."ID"
),
WeightedMeanCalc AS (
  SELECT
    SUM(val * parent_val) / NULLIF(SUM(parent_val), 0) AS val,
    SUM(parent_val) AS total_parent_val
  FROM BothVals
)

SELECT
  val,
  ROUND((val * total_parent_val) / 5) * 5 AS count
FROM WeightedMeanCalc;

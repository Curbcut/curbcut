SELECT SUM("%1$s") AS val
FROM %2$s."%3$s_%4$s"
WHERE "%5$s" IS NOT NULL
AND (%6$s = -1 OR "ID" = '%6$s');

WITH SelectedSums AS (
    SELECT SUM(%1$s) AS count
    FROM %2$s."%3$s_%4$s"
    WHERE "ID" IN (
        SELECT "ID"
        FROM %2$s."%3$s_%5$s"
        WHERE %6$s > %7$s
        AND (%8$s = -1 OR "ID" = '%8$s')
    )
), TotalSums AS (
    SELECT SUM(%1$s) AS val
    FROM %2$s."%3$s_%4$s"
)

SELECT count, count / val::float AS val
FROM SelectedSums, TotalSums;

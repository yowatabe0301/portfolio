-- A1: Analysis of Carrier On-Time Performance
-- Student Name: Yoshiyuki Watabe
-- Class Name: 2023 MBAN DD


-- SQL code 1


WITH flight_counts AS (
	SELECT year,
		quarter,
		month,
		operating_airline,
		COUNT(*) AS number_of_flights
	FROM "mban23_a1_assignment"."mybucket_ywatabe"
	GROUP BY year,
		quarter,
		month,
		operating_airline
)
SELECT year,
	quarter,
	month,
	operating_airline,
	number_of_flights,
	RANK() OVER (
		PARTITION BY year,
		quarter,
		month
		ORDER BY number_of_flights DESC
	) AS ranking
FROM flight_counts
ORDER BY year ASC,
	quarter ASC,
	month ASC,
	ranking ASC;


-- SQL code 2_1


SELECT dayofweek,
	COUNT(*) AS number_of_flights,
	ROUND(AVG(departuredelaygroups), 2) AS avg_dep_delay_group,
	ROUND(AVG(arrivaldelaygroups), 2) AS avg_arr_delay_group,
	ROUND(AVG(departuredelaygroups + arrivaldelaygroups), 2) avg_total_delay_group
FROM "mban23_a1_assignment"."mybucket_ywatabe"
GROUP BY dayofweek
ORDER BY avg_total_delay_group ASC;


-- SQL code 2_2


SELECT dayofmonth,
	COUNT(*) AS number_of_flights,
	ROUND(AVG(departuredelaygroups), 2) AS avg_dep_delay_group,
	ROUND(AVG(arrivaldelaygroups), 2) AS avg_arr_delay_group,
	ROUND(AVG(departuredelaygroups + arrivaldelaygroups), 2) avg_total_delay_group
FROM "mban23_a1_assignment"."mybucket_ywatabe"
GROUP BY dayofmonth
ORDER BY avg_total_delay_group ASC;


-- SQL code 3


SELECT flight_bin_range,
    COUNT(*) AS number_of_flights,
    ROUND(AVG(departuredelaygroups), 2) AS avg_dep_delay_group,
    ROUND(AVG(arrivaldelaygroups), 2) AS avg_arr_delay_group,
    ROUND(AVG(departuredelaygroups + arrivaldelaygroups), 2) avg_total_delay_group
FROM (
    SELECT CASE
        WHEN flights BETWEEN 0 AND 99 THEN '000-099'
        WHEN flights BETWEEN 100 AND 199 THEN '100-199'
        WHEN flights BETWEEN 200 AND 299 THEN '200-299'
        WHEN flights BETWEEN 300 AND 399 THEN '300-399'
        WHEN flights BETWEEN 400 AND 499 THEN '400-499'
        WHEN flights BETWEEN 500 AND 599 THEN '500-599'
        WHEN flights BETWEEN 600 AND 699 THEN '600-699'
        ELSE '700-799'
    END AS flight_bin_range,
    departuredelaygroups,
    arrivaldelaygroups
FROM mban23_a1_assignment.mybucket_ywatabe
WHERE departuredelaygroups IS NOT NULL
    AND arrivaldelaygroups IS NOT NULL
) AS binned_flights
GROUP BY flight_bin_range
ORDER BY flight_bin_range ASC;


-- SQL code 4


SELECT dayofweek,
	distance_bin_range,
	COUNT(*) AS number_of_flights
FROM (
		SELECT dayofweek,
			CASE
				WHEN distance BETWEEN 0 AND 99 THEN '000-099'
				WHEN distance BETWEEN 100 AND 199 THEN '100-199'
				WHEN distance BETWEEN 200 AND 299 THEN '200-299'
				WHEN distance BETWEEN 300 AND 399 THEN '300-399'
				WHEN distance BETWEEN 400 AND 499 THEN '400-499'
				WHEN distance BETWEEN 500 AND 599 THEN '500-599' ELSE '600-699'
			END AS distance_bin_range
		FROM mban23_a1_assignment.mybucket_ywatabe
	) AS binned_distance
GROUP BY dayofweek,
	distance_bin_range
ORDER BY dayofweek ASC,
	distance_bin_range ASC;


-- SQL code 5_0


SELECT ROW_NUMBER() OVER (
		ORDER BY origin ASC,
			destcityname ASC
	) AS route_number,
	origin,
	destcityname,
	COUNT(*) AS number_of_flights,
	SUM(
		CASE
			WHEN weatherdelay > 0 THEN 1 ELSE 0
		END
	) AS weather_delay_flights,
	ROUND(
		(
			SUM(
				CASE
					WHEN weatherdelay > 0 THEN 1 ELSE 0
				END
			) / COUNT(*)
		) * 100,
		2
	) AS weather_delay_percentage,
	ROUND(AVG(arrivaldelaygroups), 2) AS avg_arr_delay_group
FROM mban23_a1_assignment.mybucket_ywatabe
GROUP BY origin,
	destcityname;


-- SQL code 5_1


WITH route_delays AS (
	SELECT ROW_NUMBER() OVER (
			ORDER BY origin ASC,
				destcityname ASC
		) AS route_number,
		origin,
		destcityname,
		COUNT(*) AS number_of_flights,
		SUM(
			CASE
				WHEN weatherdelay > 0 THEN 1 ELSE 0
			END
		) AS weather_delay_flights,
		ROUND(
			(
				SUM(
					CASE
						WHEN weatherdelay > 0 THEN 1 ELSE 0
					END
				) / COUNT(*)
			) * 100,
			2
		) AS weather_delay_percentage,
		ROUND(AVG(arrivaldelaygroups), 2) AS avg_arr_delay_group
	FROM mban23_a1_assignment.mybucket_ywatabe
	GROUP BY origin,
		destcityname
),
delay_bins AS (
	SELECT route_number,
		CASE
			WHEN avg_arr_delay_group BETWEEN 0 AND 9 THEN '00-09'
			WHEN avg_arr_delay_group BETWEEN 10 AND 19 THEN '010-019'
			WHEN avg_arr_delay_group BETWEEN 20 AND 29 THEN '020-029'
			WHEN avg_arr_delay_group BETWEEN 30 AND 39 THEN '030-039'
			WHEN avg_arr_delay_group BETWEEN 40 AND 49 THEN '040-049' ELSE '50+'
		END AS route_delay_bin_range
	FROM route_delays
)
SELECT route_delay_bin_range,
	COUNT(route_number) AS number_of_routes
FROM delay_bins
GROUP BY route_delay_bin_range
ORDER BY route_delay_bin_range ASC;


-- SQL code 5_2


WITH route_delays AS (
	SELECT ROW_NUMBER() OVER (
			ORDER BY origin ASC,
				destcityname ASC
		) AS route_number,
		origin,
		destcityname,
		ROUND(AVG(arrivaldelaygroups), 2) AS avg_arr_delay_group
	FROM mban23_a1_assignment.mybucket_ywatabe
	GROUP BY origin,
		destcityname
),
delay_bins AS (
	SELECT route_number,
		CASE
			WHEN avg_arr_delay_group BETWEEN 0 AND 9 THEN '00-09'
			WHEN avg_arr_delay_group BETWEEN 10 AND 19 THEN '010-019'
			WHEN avg_arr_delay_group BETWEEN 20 AND 29 THEN '020-029'
			WHEN avg_arr_delay_group BETWEEN 30 AND 39 THEN '030-039'
			WHEN avg_arr_delay_group BETWEEN 40 AND 49 THEN '040-049' ELSE '50+'
		END AS route_delay_bin_range,
		origin,
		destcityname
	FROM route_delays
)
SELECT airport,
	COUNT(*) AS count
FROM (
		SELECT origin AS airport
		FROM delay_bins
		WHERE route_delay_bin_range = '50+'
		UNION ALL
		SELECT destcityname AS airport
		FROM delay_bins
		WHERE route_delay_bin_range = '50+'
	) AS combined
GROUP BY airport
ORDER BY count DESC,
	airport ASC;
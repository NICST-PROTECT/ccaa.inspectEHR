with summary as (
-- measurement
select 
m.measurement_concept_id  concept_id,
m.unit_concept_id,
count(*),
ROUND(MIN(m.value_as_number), 2) min,
ROUND(avg(m.value_as_number), 2) mean,
PERCENTILE_CONT(0.5) within group (order by m.value_as_number) median,
ROUND(MAX(m.value_as_number), 2) max,
MODE() within group (order by m.value_as_concept_id) most_frequent_category,
null most_frequent_string
from :OMOP_SCHEMA.measurement m
group by m.measurement_concept_id, m.unit_concept_id 
union 
-- observation
select 
o.observation_concept_id  concept_id,
o.unit_concept_id,
count(*),
ROUND(MIN(o.value_as_number), 2) min,
ROUND(avg(o.value_as_number), 2) mean,
PERCENTILE_CONT(0.5) within group (order by o.value_as_number) median,
ROUND(MAX(o.value_as_number), 2) max,
MODE() within group (order by o.value_as_concept_id) most_frequent_category,
MODE() within group (order by o.value_as_string) most_frequent_string
from :OMOP_SCHEMA.observation o
group by o.observation_concept_id, o.unit_concept_id )

-- getting concept names
select 
s.concept_id, 
c.concept_name,
s.unit_concept_id,
s.count,
s.min,
s.mean,
s.median,
most_frequent_category,
c1.concept_name most_frequent_category_name,
most_frequent_string
from summary s
inner join :OMOP_SCHEMA.concept c 
on c.concept_id = s.concept_id 
left join :OMOP_SCHEMA.concept c1
on c1.concept_id = s.most_frequent_category 
order by count desc
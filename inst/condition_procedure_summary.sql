with diagnoses as (
-- snomed condition
select 
condition_concept_id concept_id,
condition_source_value source_value,
count(*),
'condition_occurrence' table_name
from :OMOP_SCHEMA.condition_occurrence co
group by condition_concept_id , condition_source_value
having condition_concept_id != 0
-- snomed procedure 
union
select 
procedure_concept_id concept_id,
procedure_source_value source_value,
count(*),
'procedure_occurrence' table_name
from :OMOP_SCHEMA.procedure_occurrence po
group by procedure_concept_id , procedure_source_value
having procedure_concept_id != 0
union
-- apache condition
select 
condition_concept_id concept_id,
condition_source_value source_value,
count(*),
'condition_occurrence' table_name
from :OMOP_SCHEMA.condition_occurrence co
group by condition_concept_id, condition_source_value 
having condition_concept_id = 0
-- apache procedure 
union
select 
procedure_concept_id concept_id,
procedure_source_value source_value,
count(*),
'procedure_occurrence' table_name
from :OMOP_SCHEMA.procedure_occurrence po
group by procedure_concept_id, procedure_source_value 
having procedure_concept_id = 0
)

select 
d.concept_id,
d.source_value,
c.concept_name,
count,
table_name
from diagnoses d
left join :OMOP_SCHEMA.concept c
on c.concept_id = d.concept_id  
where count >20
order by count desc


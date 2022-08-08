with drugs as (
select 
drug_concept_id concept_id ,
count(*)
from :OMOP_SCHEMA.drug_exposure d 
group by drug_concept_id 
)

select 
d.concept_id,
c.concept_name,
count
from drugs d
left join :OMOP_SCHEMA.concept c
on c.concept_id = d.concept_id  
order by count desc


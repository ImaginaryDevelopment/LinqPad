<Query Kind="SQL">
</Query>

select p.project_id,qg.project_quota_id,count(distinct(
	--case when ui.invitation_source_id is null then 99 else --account for nulls as another distinct item
	ui.invitation_source_id 
	--end
	)) 
	from project.project p with(nolock)
	join project.quota_group qg with (nolock)
		on p.project_id=qg.project_id
	join [project].[User_Invitation] ui with(nolock) 
		on qg.project_quota_id = ui.project_quota_id
	group by p.project_id,qg.project_quota_id
	having count(distinct(
		--case when ui.invitation_source_id is null then 99 else 
		ui.invitation_source_id 
		--end
		)) >1
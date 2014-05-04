<Query Kind="SQL">
</Query>

--best fit: 65413
SELECT qg.project_id,t1.project_quota_id,ss.survey_status_name,
 	t1.prelim_survey_status_code as code,
	t1.invitation_source_id as sourceId,
	sc.user_invitation_source_name as sourceName
 , COUNT(1) as total
 
        FROM project.quota_group qg with(nolock)
		join [project].[user_invitation] AS [t1] with(nolock) on qg.project_quota_id = t1.project_quota_id
		join project.survey_status ss with(nolock) on t1.prelim_survey_status_code = ss.survey_status_code
		left join project.invitation_source_codeset sc with(nolock) on t1.invitation_source_id = sc.user_invitation_source_id
        WHERE [t1].[prelim_survey_status_code] IS NOT NULL 
			and t1.invitation_source_id is not null 
			and t1.prelim_survey_status_code !='U' --ignore no responses
		
		group by qg.project_id,t1.project_quota_id,t1.prelim_survey_status_code,ss.survey_status_name,t1.invitation_source_id,sc.user_invitation_source_name
        order by count(1) desc,qg.project_id 
		
		select * from project.survey_status
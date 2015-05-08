<Query Kind="SQL">
</Query>

select ui.prelim_survey_status_code, count(1) as total,ui.invitation_source_id as sourceId 
                    from [project].[User_Invitation] ui with(nolock) 
                    where ui.project_quota_id= 353234
                    group by ui.invitation_source_id, ui.prelim_survey_status_code
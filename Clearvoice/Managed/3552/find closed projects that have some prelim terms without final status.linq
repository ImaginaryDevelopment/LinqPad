<Query Kind="Expression">
</Query>

 
 from qg in Quota_groups.Where (q =>q.Is_closed==false&& q.Project.Project_status_code=='C'  && q.User_invitations.Any (u => u.Prelim_survey_status_code=='T' && u.Final_survey_status_dt.HasValue==false))
 let count=qg.User_invitations.Count (u => u.Prelim_survey_status_code=='T' && u.Final_survey_status_dt.HasValue==false)
 where count>1
 select new{qg,qg.Project, QgCount=qg.Project.Quota_groups.Count,UiCount=qg.User_invitations.Count (u => u.Prelim_survey_status_code=='T'&& u.Final_survey_status_dt.HasValue==false)}
 //User_invitations.Where (u => u.Prelim_survey_status_code=='T' && u.Quota_group.Is_closed==false).Select (u => u.Quota_group).Distinct()

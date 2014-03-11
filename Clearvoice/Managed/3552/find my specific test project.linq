<Query Kind="Expression">
</Query>

from p in Projects.Where (p => p.Project_id==61910)
from qg in p.Quota_groups.Where(qg=>qg.Is_closed==false && qg.User_invitations.Any (u =>u.Prelim_survey_status_code=='T' && u.Final_survey_status_dt.HasValue==false ))
select new{ p,qg,uis=qg.User_invitations.Where (u => u.Prelim_survey_status_code=='T' && u.Final_survey_status_dt.HasValue==false )}
//importing as complete:
//not importing :
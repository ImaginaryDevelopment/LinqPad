<Query Kind="Statements">
</Query>

var q=from p in Projects.Where (p => p.Project_id==61910)
from qg in p.Quota_groups.Where(qg=>qg.Is_closed==false && qg.User_invitations.Any (u =>u.Prelim_survey_status_code=='T' && u.Final_survey_status_dt.HasValue==false ))
select new{ p,qg,uis=qg.User_invitations.Where (u => u.Prelim_survey_status_code=='T' && u.Final_survey_status_dt.HasValue==false )};
var allMemIds=q.SelectMany (x => x.uis).Select (x => x.Member_id.ToString()).ToArray().Dump("all");
var successes=allMemIds.Skip(1).Take(2).Dump("successes").Prepend("memberid").ToArray();


var outputPath=Environment.ExpandEnvironmentVariables("%userprofile%\\downloads\\successes_")+DateTime.Now.ToString("yyyyMMdd")+".csv";
outputPath.Dump();
System.IO.File.WriteAllLines(outputPath,successes);
//importing as complete:
//not importing :
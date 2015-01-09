<Query Kind="Expression">
  <Connection>
  </Connection>
</Query>

from p in Projects.Where(p=>p.Is_enabled && ! p.Is_deleted && p.Project_status_code=='O' && p.Survey_integration_url.Contains("##ver"))
let environment = "localhost:16280"
let path = "/SurveyInvitation.aspx?vid="
select new { 
	p.Project_id,
	p.Org_id,
	p.Project_name,
	invitations = User_invitations.Where(ui=>p.Quota_groups.Any(qg=>qg.Project_quota_id== ui.Project_quota_id))
		.Select(ui=>
		new {ui.User_invitation_guid, link = new LINQPad.Hyperlinq("http://" + environment + path + ui.User_invitation_guid.ToString())}
		),
	p.Survey_integration_url,
	
	
}

<Query Kind="Expression">
  <Connection>
    <ID>282ab884-278b-4d31-a865-51e85288681f</ID>
    <Persist>true</Persist>
  </Connection>
</Query>
from m in Member_members.Where(m=>m.Verified_bit_field>0 && m.Is_active && m.Is_doi && !m.Is_fraud)
where m.Member_id>0
 && m.Org_id==1
let invitations = User_invitations.Where(ui=>ui.Member_id==m.Member_id && ui.Is_available_survey==true)
where invitations.Any(i=>i.Quota_group.Project.Project_status_code=='O')
where invitations.Count()>1
select new{m.Email_address,m.Org_id,m.Org.Org_name, m.Password, m.Member_id, Count=invitations
	.Where(i=> i.Quota_group.Project.Project_status_code=='O' && i.Prelim_survey_status_code=='U' )
	.Select(i=>
		new{ link=
			new LINQPad.Hyperlinq("http://localhost:16280/surveyinvitation.aspx?vid="+ i.User_invitation_guid, i.User_invitation_guid.ToString()),
			i.Quota_group.Project.Project_id,
			i.Project_quota_id,
			guid = i.User_invitation_guid,
			qg = i.Quota_group}
		)}
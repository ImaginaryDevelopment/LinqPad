<Query Kind="Statements">
</Query>

var interesting=Quota_groups.Where (qg => qg.Project_quota_id==333772).Select (qg => qg.User_invitations.Where (u =>new[]{
//not uploaded:
925352,
//uploaded success
1908182,3519679}.Contains( u.Member_id)).Select (u => new{u.Member_id, u.Prelim_survey_status_code,u.User_invitation_id, u.Final_survey_status_code, u.Final_survey_status_dt}));
interesting.Dump();
using(var tran = new TransactionScope( TransactionScopeOption.RequiresNew , new TransactionOptions(){ IsolationLevel= System.Transactions.IsolationLevel.ReadUncommitted })){
Quota_groups.Where (qg => qg.Project_quota_id==333772).Select (qg => qg.User_invitations.Where (u =>new[]{
//not uploaded:
925352,
//uploaded success
1908182,3519679}.Contains( u.Member_id)).Select (u => new{u.Member_id, u.Prelim_survey_status_code,u.User_invitation_id, u.Final_survey_status_code, u.Final_survey_status_dt})).Dump();
}
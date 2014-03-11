<Query Kind="Statements" />

using(var txn = new TransactionScope( TransactionScopeOption.Required, new System.Transactions.TransactionOptions{ IsolationLevel=System.Transactions.IsolationLevel.ReadUncommitted}))
{
	(from m in Member_members.Where (m =>m.Update_dt>DateTime.Today.AddDays(-1) || m.Is_active==false && m.Org_id==1 && (m.Is_deleted || m.Complaint_count>0 || m.Dnc_dt.HasValue || m.Is_dnc || m.Bounce_dt.HasValue || m.Is_bounce || m.Scrub_dt.HasValue || m.Is_scrub))
	let user = m.Member_guidAspnet_Users
	let ms = user.Aspnet_Membership
	let isLoginEnabled = !(m.Is_deleted || m.Complaint_count>0 || m.Dnc_dt.HasValue || m.Is_dnc || m.Bounce_dt.HasValue || m.Is_bounce || m.Scrub_dt.HasValue || m.Is_scrub)
	
	//orderby m.Util_country.Country_name=="United States of America" || m.Util_country.Country_name=="Canada" descending, m.Update_dt descending
	select new{ m.Email_address,ms.Email,m.Password,Name= m.First_name+" "+m.Last_name,isLoginEnabled,ms.IsApproved,ms.IsLockedOut,m.Is_bounce,m.Update_by,m.Update_dt,m.Member_id,  m.Util_country.Country_name,sms=Sms_logs.Where (s => s.Member_id==m.Member_id), mh=Member_change_logs.Where (me =>me.Update_dt>DateTime.Now.AddDays(-10) && me.Member_id==m.Member_id)}
	)
	.Where(x=>x.Member_id== 221576)
	.Take(20)
	.Dump();
}
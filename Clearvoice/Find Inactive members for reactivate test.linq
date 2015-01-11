<Query Kind="Expression">
</Query>

// find inactive members for testing the reactivate flow
//specific to org of current debug session
(from m in Member_members
	.Where (m => 
	m.Org_id==1
	
	)
let user = m.Member_guidAspnet_Users
let ms = user.Aspnet_Membership
let isLoginEnabled = !(m.Is_deleted || m.Complaint_count>0 || m.Dnc_dt.HasValue || m.Is_dnc || m.Bounce_dt.HasValue || m.Is_bounce || m.Scrub_dt.HasValue || m.Is_scrub)
where !isLoginEnabled
//orderby m.Util_country.Country_name=="United States of America" || m.Util_country.Country_name=="Canada" descending, m.Update_dt descending
select new{m.Email_address,isLoginEnabled,MembershipEmail=ms.Email,m.Password, m.Reactivate_dt,
	Name= m.First_name+" "+m.Last_name,ms.IsApproved,ms.IsLockedOut,m.Is_bounce,m.Update_by,m.Update_dt,m.Member_id,  m.Util_country.Country_name,sms=Sms_logs.Where (s => s.Member_id==m.Member_id),
	mh=Member_change_logs.Where (me =>me.Update_dt>DateTime.Now.AddDays(-10) && me.Member_id==m.Member_id)}
).Take(20).ToArray()

.Select (d => new{d.Email_address,d.isLoginEnabled, MembershipEmail=Util.HighlightIf(d.MembershipEmail,x=>x!=d.Email_address),d.Password,d.Name,d.IsApproved,d.IsLockedOut,d.Is_bounce,d.Update_by,d.Update_dt,d.Member_id,
	d.Country_name,d.sms,d. mh})
//Util.HighlightIf(d,x=>x!=null && x.Email_address!=x.MembershipEmail))



//public bool IsLoginEnabled { get { return !(IsDeleted || ComplaintCount > 0 || DncDt.HasValue || IsDnc || BounceDt.HasValue || IsBounce || ScrubDt.HasValue || IsScrub); } }


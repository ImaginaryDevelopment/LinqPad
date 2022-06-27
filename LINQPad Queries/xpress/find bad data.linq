<Query Kind="SQL">
  <Connection>
    <ID>6d9036c3-dd08-4e55-8c56-0d7d31c5ebfc</ID>
    <Persist>true</Persist>
    <Server>prog7-pc</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>ApplicationDatabase</Database>
    <UserName>xpu10</UserName>
    <IncludeSystemObjects>true</IncludeSystemObjects>
  </Connection>
</Query>

-- super bad data

print 'journal entries where the patientId disagrees with the payment.patientid or appointment.appointmentpatientid'
select je.journalentryid, je.appointmentid, je.patientid, a.appointmentpatientid, py.patientid as paymentPatientId
	,(
		select distinct Convert(varchar(max),ch.patientid) + ',' --,ch.chargeid 
		from charge ch
		where je.appointmentid = ch.appointmentid and je.patientid <> ch.patientid
		for xml path ('')
	) as chargePatientIds -- charges with different patientIds from the journal entry
	from accounts.journalentry je
	left join appointments a on je.appointmentid = a.appointmentid
	left join payment py on je.paymentid = py.paymentid
	where je.patientid <> a.appointmentpatientid or je.patientid <> py.patientid
	
print 'charges where the patientid doesn''t match the appointmentPatientid'
select a.appointmentpatientid, ch.patientid from appointments a join charge ch on ch.appointmentid = a.appointmentid where a.appointmentpatientid != ch.patientid

print 'journal entries with a patientAccountId debit or credit, without a patientId'
select je.journalentryid, je.creditaccountid,je.debitaccountid
from accounts.journalentry je
where je.patientid is null and (
	exists (select 1 from patients pt where (pt.accountid = je.creditaccountid or pt.accountid = je.debitaccountid))
)

	


-- older migration may not have this column
if exists( select 1 from INFORMATION_SCHEMA.columns c
    where c.TABLE_SCHEMA='dbo'
    and c.TABLE_NAME='payment'
    and c.COLUMN_NAME = 'appointmentid')
begin
 	print 'payments where the patientId doesn''t match the appointmentPatientId'
	select py.paymentid,py.patientid,a.appointmentpatientid from payment py join appointments a on py.appointmentid = a.appointmentid where py.patientid != a.appointmentpatientid
end
if exists(select 1 from sys.tables where name = 'charges')
begin
 	print 'if the source of the error appears to be migration, or if it was bad before'
	select ch1.chargeid as ChargeId1,ch1.patientid ChPatientId1, a1.appointmentid VID1, a1.appointmentpatientid APtId1
		,ch2.chargeid as ChargeId2,ch2.patientid ChPatientId2, a2.appointmentid VID2, a2.appointmentpatientid APtId2
	from charges ch1 
	join charge ch2 on ch1.chargeid = ch2.chargeid
	left join appointments a1 on ch1.appointmentid = a1.appointmentid
	left join appointments a2 on ch2.appointmentid = a2.appointmentid
	where 
		ch1.chargeid in (select ch.chargeid from appointments a join charge ch on ch.appointmentid = a.appointmentid where a.appointmentpatientid != ch.patientid)
		or ch2.chargeid in (select ch.chargeid from appointments a join charge ch on ch.appointmentid = a.appointmentid where a.appointmentpatientid != ch.patientid)
end
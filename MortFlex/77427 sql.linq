<Query Kind="SQL">
  <Connection>
    <ID>c9b76ea2-c7e7-4a9e-a336-ec4241c6dac5</ID>
    <Persist>true</Persist>
    <Server>rpsql2008r2dev</Server>
    <SqlSecurity>true</SqlSecurity>
    <Database>NOVA08RLS</Database>
    <UserName>WINRLS</UserName>
    <Password>AQAAANCMnd8BFdERjHoAwE/Cl+sBAAAA1WLKs9qc4USFiwcJ5tmkhgAAAAACAAAAAAADZgAAwAAAABAAAACrzDqjgelbVgZelHzxoUCkAAAAAASAAACgAAAAEAAAANaxhqC+PlSpfWS3MUIfULoIAAAAIqZ2P2kltAwUAAAAsZVmuOxGnoVr0wOGD/We2Jqm8Z4=</Password>
  </Connection>
</Query>

alter table inc add CC_TRADE_ID	NVARCHAR(10)
alter table inc add CC_TRADE_DATE	DATE
alter table inc add CC_TRADE_AMOUNT	NUMERIC(12,2)
alter table inc add  CC_COUPON	NUMERIC(7,4)
alter table inc add CC_DEALER	NVARCHAR(25)
alter table inc add CC_SECURITY_TYPE	NVARCHAR(35)

alter table invcommprice add CC_NRA NUMERIC(7,4)
alter table invcommprice add CC_EARLY_DELIVERY NUMERIC(7,4)
alter table invcommprice add CC_DESK_INCENTIVE NUMERIC(7,4)
--DEALER	
insert into gencode (groups,code,description,type,length,rec_id) values ('I_DEALER','1','AO','A',25, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_DEALER','2','BF','A',25, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_DEALER','3','BK','A',25, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_DEALER','4','GMAC','A',25, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_DEALER','5','Provident Funding','A',25, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_DEALER','6','Homeward Residential','A',25, NEWID())
--SECURITY
insert into gencode (groups,code,description,type,length,rec_id) values ('I_SECURITY','1','FNMA 15YR','A',15, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_SECURITY','2','FNMA 10YR','A',15, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_SECURITY','3','FNMA 30YR','A',15, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_SECURITY','4','FNMA 20YR','A',15, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_SECURITY','5','GNMA I 30YR','A',15, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_SECURITY','6','GNMA II 30YR','A',15, NEWID())

-- BEGIN SCREEN 2-3
alter table lninvestorcomm add CC_NRA NUMERIC(7,4)
alter table lninvestorcomm add CC_EARLY_DELIVERY NUMERIC(7,4)
alter table lninvestorcomm add CC_DESK_INCENTIVE NUMERIC(7,4)

alter table custom1 add INVESTOR NVARCHAR(50)
alter table custom1 add NON_LLPA NUMERIC(7,4)
alter table custom1 add TOTAL_LLPA NUMERIC(7,4)
drop table inv_price_adjustment
create table INV_PRICE_ADJUSTMENT (
	ACCOUNT_ID uniqueidentifier NOT NULL foreign key references accounts (account_id) ,
	DESCRIPTION NVARCHAR(25) not null,
	AMOUNT NUMERIC(7,4) NOT NULL,
	primary key(account_id,description)
	)
	
--ADJDESC
insert into gencode (groups,code,description,type,length,rec_id) values ('I_ADJDESC','1','10 Yr Pay Up','A',35, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_ADJDESC','2','20 Yr Pay Up','A',35, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_ADJDESC','3','Adverse Market Fee','A',35, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_ADJDESC','4','Buy Down','A',35, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_ADJDESC','5','Buy Up','A',35, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_ADJDESC','6','Cap Adjustment','A',35, NEWID())
insert into gencode (groups,code,description,type,length,rec_id) values ('I_ADJDESC','7','Cash Out','A',35, NEWID())
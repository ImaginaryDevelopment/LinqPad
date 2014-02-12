<Query Kind="Statements" />

var q= from t in TRACKs
	join ac in ACCOUNTS on t.ACCOUNT_ID equals ac.ACCOUNT_ID
	join lnLeft in LNINVESTORCOMMs on t.ACCOUNT_ID equals lnLeft.ACCOUNT_ID into lnl
	from ln in lnl.DefaultIfEmpty()
	join stLeft in SCODEs on t.SCODE_ID equals stLeft.SCODE_ID into stl
	from st in stl.DefaultIfEmpty()
	join tLeft in TERMS on t.ACCOUNT_ID equals tLeft.ACCOUNT_ID into tl
	from term in tl.DefaultIfEmpty()
	join custom1Left in CUSTOM1s on t.ACCOUNT_ID equals custom1Left.ACCOUNT_ID into c1l
	from c1 in c1l.DefaultIfEmpty()
	join paLeft in INV_PRICE_ADJUSTMENTs on t.ACCOUNT_ID equals paLeft.ACCOUNT_ID into pal
	from ipa in pal.DefaultIfEmpty()
	
	join wbLeft in WEBLNWRKDTLs on new{ term.LOAN_PROGRAM,AccountNumber=ac.ACCOUNT_NUMBER}  
		equals new{wbLeft.LOAN_PROGRAM,AccountNumber=wbLeft.SESSION_ID} into wbl
	from wb in wbl.DefaultIfEmpty()
select new{ 
	ac.ACCOUNT_NUMBER, 
	t.ACCOUNT_ID,
	inc=t.INC!=null? new{t.INC.DESCRIPTION, t.INC.CC_COUPON, t.INC.CC_DEALER,t.INC.CC_SECURITY_TYPE,t.INC.INVESTOR_ID}: null,
	term= term!=null? term.TOTAL_LLPA:null,
	StatusDesc=st.DESCRIPTION,
	hasLoanInvComm= ln!=null,
	loanInvestorComm= ln!=null? new{ ln.ADJUSTMENTS,ln.COMM_NUM,ln.INVEST_COMM_NUM,ln.INVEST_LOAN_NUM}:null,
	custom1= c1!=null? new{ c1.INVESTOR,c1.NON_LLPA,c1.MAX_PRICE}:null,
	invPriceAdjustment= ipa !=null? new{ ipa.DESCRIPTION,ipa.AMOUNT}:null,
	wb
	};
q.Where(x=>x.ACCOUNT_NUMBER=="4000000380").Dump();
q.Dump();
//q.Where(x=>x.hasCustom1 && x.NON_LLPA!=null).Take(2).Dump();
q.Where(x=> x.hasLoanInvComm!=null).Take(2).Dump("with loaninvcomm");
q.Where(x=>x.term!=null).Take(2).Dump("with terms");
q.Where(x=>x.inc.INVESTOR_ID!=null).Dump();
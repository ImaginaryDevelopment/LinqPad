<Query Kind="Program">
  <NuGetReference>Newtonsoft.Json</NuGetReference>
  <Namespace>Newtonsoft.Json</Namespace>
</Query>

void Main()
{
	var members=Member_members;
	//members.Where (m => m.Verified_bit_field>0).Dump();
	var goodItems=from h in Histories.Where (h =>h.Reward_history_id==20345750 || h.Is_approved!=true&& (h.Is_exported ==null || h.Is_exported==false) && (new int?[]{15,40,79}.Contains(h.Reward_catalog_id)))//20345750
		join m in members on
				h.Member_id equals m.Member_id
				select new{h,m};
	var tupled=goodItems.Select (i => Tuple.Create(i.h,i.m)).ToArray().Dump("good items"); 
	var actual= History_GetUnapprovedRewardsByRewardCatalogIds("15,40,79,",null,null).Dump("actual");
//	if(goodItems.Any (i =>i.m.Is_fraud)==false){
//		//lets add a fraud one?
//		CloneFraud(members,tupled);
//		
//	}
	if(goodItems.Any (i => i.m.Verified_bit_field==0 && i.m.Is_fraud==false)==false){
		CloneUnverifiedNonFraud(members,tupled); //cloning unverified
	}
	if(goodItems.Any (i => i.m.Verified_bit_field!=0)==false){
		CloneVerified(members,tupled); //cloning unverified
	}
	
}
public void Clone(IQueryable<Member_member> members,Tuple<History,Member_member>[] goodItems,Expression<Func<Member_member,bool>> memberCriteria){
	var oldCount=goodItems.Dump().Count ();
	var goodBase= goodItems.First ();
		using (TransactionScope tran = new TransactionScope(TransactionScopeOption.RequiresNew)) {
			
			var memberToClone = members
				.Where (me =>me.Org_id==goodBase.Item2.Org_id && goodItems.Select (i => i.Item2.Member_id)
					.Contains(me.Member_id)==false)
				.Dump("Candidates")
				.Where (memberCriteria).First ();
		
				var fake= JsonConvert.DeserializeObject<History>( JsonConvert.SerializeObject(goodItems.First ().Item1));
				fake.Member_id=memberToClone.Member_id;
				
				Histories.InsertOnSubmit(fake);
		
		
		
				this.SubmitChanges();
				if(goodItems.Count()>oldCount) //make sure we added something
				{
					goodItems.Dump("Completing inserts");
					tran.Complete();
					
				}
			
		}
}
public void CloneUnverifiedNonFraud(IQueryable<Member_member> members,Tuple<History,Member_member>[] goodItems){
	Clone(members,goodItems,me=>me.Verified_bit_field!=0);
}
public void CloneVerified(IQueryable<Member_member> members,Tuple<History,Member_member>[] goodItems){
	Clone(members,goodItems,me=>me.Verified_bit_field!=0);
}
// Define other methods and classes here
public void CloneFraud(IQueryable<Member_member> members,Tuple<History,Member_member>[] goodItems){
		Clone(members,goodItems,me=>me.Is_fraud);
}
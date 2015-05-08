<Query Kind="FSharpExpression">
  <Connection>
  </Connection>
</Query>
//this.SubmitChanges()

let answers = [
	98,472
	105,536
	104,531
	130,761
	101,499
	102,509
	99,479
	106,544
	107,559
	]
let demographics = 
	answers
	|> Seq.map (fun (x,y) -> x)
let answerIds = 
	answers
	|> Seq.map (fun (x,y) -> y)
query { for m in this.Member_members do
		where(m.Member_id = memberId)
		select m},
	
query { for d in this.Demographics do
		where (demographics.Contains(d.Demographic_id))
		select (d.Demographic_id,d.Demographic_name)},
query { for a in this.Answers do
				where (answerIds.Contains(a.Answer_id))
				select (a.Answer_id,a.Demographic_id,a.Answer_name)},
				
query { for da in this.Demographic_answers do
		where (da.Member_id = memberId && demographics.Contains(da.Demographic_id))
		select da}

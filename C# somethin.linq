<Query Kind="Statements" />

//when f ==0 there are only 362880 permutations
//(362880*2).Dump();

var q= from f in Enumerable.Range(2,1)
from second in Enumerable.Range(1,1)
from third in Enumerable.Range(0,10)
from fourth in Enumerable.Range(0,10)
from fifth in Enumerable.Range(0,10)
from sixth in Enumerable.Range(0,10)
from seventh in Enumerable.Range(0,10)
from eight in Enumerable.Range(0,10)
from ninth in Enumerable.Range(0,10)
from tenth in Enumerable.Range(0,10)
where f!=second && f!=third && f!=fourth && f!=fifth && f!=sixth && f!=seventh && f != eight && f!=ninth && f!=tenth
 && second !=third && second!=fourth && second!=fifth && second!=sixth && second!=seventh && second!=eight && second!=ninth && second!=tenth
 && third != fourth && third !=fifth && third!=sixth && third!=seventh && third!=eight && third!=ninth && third!=tenth
 && fourth!= fifth && fourth!=sixth && fourth!=seventh && fourth!=eight && fourth!=ninth && fourth!=tenth
 && fifth!=sixth && fifth!=seventh && fifth!=eight && fifth!=ninth && fifth != tenth
 && sixth!=seventh && sixth!=eight && sixth!=ninth && sixth!=tenth
 && seventh!=eight && seventh!=ninth && seventh!=tenth
 && eight!=ninth && eight!=tenth
 && ninth!=tenth
orderby f,second,third,fourth,fifth,sixth,seventh,eight,ninth,tenth
select f+second.ToString()+third+fourth.ToString()+fifth+sixth.ToString()+seventh+eight.ToString()+ninth+tenth.ToString();
var skip=999999-362880-362880-40320;
q.Count().Dump("permutations");
q.Skip(skip.Dump("skipping")).Take(3).Dump();
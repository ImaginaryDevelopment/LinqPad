<Query Kind="Expression">
  <Connection>
  </Connection>
</Query>

from m in this.Event_logs.GetType().GetGenericArguments()[0].GetMembers().Where(m=>m.DeclaringType != typeof(object))
select new{ m,Attrs= m.GetCustomAttributes()}
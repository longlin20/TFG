/*select 1-1;
select * from t1 natural left outer join t
select * from t join s  join u;
select * from (t join s  join u);
select * from t join (s  join u);
select * from t right join s
*/

select * from (t join s) join u;





select c from (select a c from t) v where v.c=1;




select * from (t natural full join s) left join u on t.a=u.a;
--select * from (t natural full join s) left join u on 10*s.c=u.b;
--select * from (t natural full join s) left join u on 10*s.c=u.b or 100*t.b=u.b;
--select * from (t natural full join s) natural full join u;


select iif(count(*)>0,'ok','error') from select * from t minus select * from s

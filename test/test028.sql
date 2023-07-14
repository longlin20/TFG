select * from (t full join s) join u;
select c from (select a c from t) v where v.c=1;
select * from (t natural full join s) left join u on 10*s.c=u.b or 100*t.b=u.b;


--super slower if i swap statement 1 with statement 2


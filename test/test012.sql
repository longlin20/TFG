create or replace view v1_1(a) as select t1.a from v1_2 t1,v2_2 t2 where t1.a=t2.a

insert into t values (1,'1')
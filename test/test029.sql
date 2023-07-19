create table t3(a3,b3,c3) as select a from n as;
insert into emp values ('987',2500,'987'), ('456',2000,'456'), ('123',1000,'456'), ('235',1000,'987'), ('567',800,'123'), ('678',600,'567'), ('789',500,'678');

select x from b where x div 2 = 0;
select x from b where x mod 2 = 0;
select x from b where x rem 2 = 0;

create table t3(a3,b3,c3) as select a from n as div;
create table t3(a3,b3,c3) as select a from n as rem;
create table t3(a3,b3,c3) as select a from n as mod;


/*
with((select(all,top(all),no_offset,*,[],from([(l,_)]),where(true),group_by([]),having(true),order_by([],[])),_),[(select(all,top(all),no_offset,[expr(cte(1,number(A)),_,number(A))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),l(a:_)),not((select(all,top(all),no_offset,[expr(cte(1,number(B)),_,number(B))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),l(a:_))),(select(all,top(all),no_offset,[expr(cte(1,number(C)),_,number(C))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),l(a:_))]),_
with((select(all,top(all),no_offset,*,[],from([(l,_)]),where(true),group_by([]),having(true),order_by([],[])),_),[(select(all,top(all),no_offset,[expr(cte(1,number(A)),_,number(A))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),l(a:_)),(not select(all,top(all),no_offset,[expr(cte(1,number(B)),_,number(B))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),l(a:_)),(select(all, top(all),no_offset,[expr(cte(1,number(C)),_,number(C))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),l(a:_))]),_
*/
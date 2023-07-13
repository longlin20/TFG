select distinct top 1 top * from t


--select(distinct,top(expr(1,_,number(int))),no_offset,[expr(attr(_,top,_)*attr(_,from,_),t,number(_))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),_)
--select(distinct,top(expr(1,_,number(int))),no_offset,[expr(attr(_,top,_)*attr(_,from,_),t,number(_))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),_

--select iif(count(*)>0,'ok','error') from select * from t minus select * from s


--CORRECTO
--select * from a join union select * from b;

--TODO
--select * from a join union union select * from b;

select extract hour from time)
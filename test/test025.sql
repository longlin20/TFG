--Other cond_factor and sql_factor, etc..


select 'ok' from dual where 'asdf' like '%'
select 'ok' from dual where 'asdf' like 'as__'
select 'ok' from dual where 'as_df' like '%_%' escape '_'

select substr(lower('A')||upper('b'),1,2)
select cast('1' as float)
select cast(month(date '2017-02-01') as string)
select extract(hour from time '22:05:31')

select length('a')+length('b')
select concat('a','b')

select 'a'||'b'
select 'a'+'b'
select cast('1' as float)
select cast(month(date '2017-02-01') as string)
select date '2017-02-01'-date '2016-02-01'
select current_time-current_time

select iif(count(*)>0,'ok','error') from select * from t minus select * from s
select case when 1=1 then 'a' else 'b' end
select case 1 when 1 then 'a' else 'b' end

create or replace table e(a int, b float default pi/2);


select date '2017-02-01' - 1
select cast((datetime '1-1-1 0:0:0' - 1) as string);

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   Other ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

-- opening parenthesis ''('' , 26
--select extract hour from time '22:05:31')

-- valid datetime field (year, month, day, hour, minute, second) , 16
--select extract(hur from time '22:05:31')

-- FROM , 21
--select extract(hour frm time '22:05:31')

-- valid datetime expression , 26
--select extract(hour from 3)

-- closing parenthesis '')'' , end of the program
--select extract(hour from time '22:05:31'

-- AS , 17
--select cast('1' sa float)

-- valid type , 20
--select cast('1' as foat)
 
-- comma , 27
--select iif(count(*)>0,'ok') from select * from t minus select * from s

-- valid expression , 29
--select iif(count(*)>0,'ok', ) from select * from t minus select * from s

-- an expression or WHEN , 13
--select case whn 1=1 then 'a' else 'b' end

-- THEN , 22
--select case when 1=1 hen 'a' else 'b' end

-- END , end of program
--select case when 1=1 then 'a' else 'b'

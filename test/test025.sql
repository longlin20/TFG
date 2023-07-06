--Other cond_factor and sql_factor

/*
select 'ok' from dual where 'asdf' like '%'
select 'ok' from dual where 'asdf' like 'as__'
select 'ok' from dual where 'as_df' like '%_%' escape '_'

select extract(hour from time '22:05:31')
*/

select 'a'||'b'
select 'a'+'b'
select cast('1' as float)
select cast(month(date '2017-02-01') as string)
select date '2017-02-01'-date '2016-02-01'

--select iif(count(*)>0,'ok','error') from select * from t minus select * from s


select current_time-current_time

--select date '2017-02-01' - 1
--select cast((datetime '1-1-1 0:0:0' - 1) as string);

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   Other ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

-- opening parenthesis ''(''
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


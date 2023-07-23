/*assume select 1 not in l(a) select * from l;

create table "t"("a" int);

create table [t]([a] int);

create table `t`([a] int);


select 1 into i;
select r into i;

*/

--SELECT * FROM t WHERE a=$v$;
--[1]
--[%]
--[+] 
--[>]
--[t\[]
--[t\ []
--[t\]]

ROLLBACK TO SAVEPOINT "s""p1"
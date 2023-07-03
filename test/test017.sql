/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DQL (Data Query Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SELECT(I) STATEMENTS

% DQLstmt ::=
%   (DQLstmt) 
%   |
%   UBSQL

% UBSQL ::= 
%   SELECTstmt 

% SELECTstmt ::=
%   SELECT [TOP IntegerExpression] [[ALL|DISTINCT]] SelectExpressionList
%     [INTO SelectTargetList]
%   [FROM Rels
%    [WHERE WhereCondition]
%    [GROUP BY Atts]
%    [HAVING HavingCondition]
%    [ORDER BY OrderDescription]
%    [OFFSET IntegerExpression [LIMIT IntegerExpression]]
%    [FETCH FIRST IntegerExpression ROWS ONLY]]*/

/*
select * from t 
select person.age from t 

select distinct * from t
select distinct top 1 * from t
select top 1 * from t
select top 1 distinct * from t

SELECT 1 INTO v from t
select age into v from t
select a from (select a from t)

SELECT * FROM t WHERE a=$v$

select * from t group by "1"
select department,max(salary) from employee group by department;
select count(*) from t group by a

select a from taras group by a having sum(b)=1
SELECT Department FROM employee GROUP BY Department HAVING COUNT(Salary)>1
select 1 from t having age <= all (select a from s)

SELECT Nombre, Calle, "Código postal"
FROM Empleados NATURAL INNER JOIN Domicilios 
ORDER BY "Código postal", Nombre

select n from n offset 10 limit 10

select distinct * from t fetch first 1 rows only
select t.a from t join r on t.a=r.a where t.a>=all (select min(a) from s);

*/

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   SELECT STATEMENTS ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

--SELECT list , 14/23
--select top 1 from t
--select distinct top 1 top * from t

-- FROM clause , 10
--select a top 1 * from t

--
--select a * from t

-- a valid relation , 15
--select * from 1 right join t2

-- valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT) , 18/18
--select * from t1 rigt s join t2
--select * from t1 1 join t2

-- comma or closing parenthesis ''')'' , 41
--select * from t1 inner join t2 using (c b)

-- BY , 28
--select person from t group yb a 
--SELECT N FROM E ORDER Yb N

-- valid WHERE condition
--SELECT * FROM t WHERE

-- a comparison operator , end of the program
--SELECT * FROM t WHERE a

-- valid expression , end of the program
--SELECT * FROM t WHERE a=

-- ORDER BY criteria
--SELECT N FROM E ORDER BY by

-- Only one TOP/LIMIT/FETCH specification is allowed , void
--select top 1 distinct * from t fetch first 1 rows only





--SELECT(2)
/*
select * from t1 division t2
select * from t1 join t2, t3 inner join t4
select * from t full join s on t.a=s.a
select * from t1 join t2 using (c)
SELECT * FROM s LEFT JOIN (q RIGHT JOIN sp ON q.sno=sp.sno) ON s.sno=q.sno
select * from t1 natural left outer join t
select * from t natural right join s
select * from t1 table1 right join t2 as table2


select 1;
select 1 a;
select 1 a, a+1;
select a+2,1 a, a+1;
select a+2,1+1 a, a+1;
*/








--select * from (t join s) join u;

/*select * from (t natural full join s) left join u on t.a=u.a;
select * from (t natural full join s) left join u on s.a=u.a;
select * from (t natural full join s) left join u on 10*s.c=u.b;
select * from (t natural full join s) left join u on 10*s.c=u.b or 100*t.b=u.b;
select * from (t natural full join s) natural full join u;*/
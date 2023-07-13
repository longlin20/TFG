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
select count(*) from t group by a

select a from taras group by a having sum(b)=1
SELECT Department FROM employee GROUP BY Department HAVING COUNT(Salary)>1
select 1 from t having age <= all (select a from s)


SELECT Nombre, Calle, "Codigo postal"
FROM Empleados NATURAL INNER JOIN Domicilios 
ORDER BY "Codigo postal", Nombre


select n from n offset 10 limit 10;

select distinct * from t fetch first 1 rows only;

--correct but can't check with test.pl (i guess is because min() and max())
--select department,max(salary) from employee group by department;
--select t.a from t join r on t.a=r.a where t.a>=all (select min(a) from s);

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   SELECT STATEMENTS 1 ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

--SELECT list , 14/23
--select top 1 from t
--select distinct top 1 top * from t

-- comma or closing parenthesis '')'' , 41
--select * from t1 inner join t2 using (c b)

-- BY , 28/23
--select person from t group yb a 
--SELECT N FROM E ORDER Yb N

-- valid WHERE condition , end of the program
--SELECT * FROM t WHERE

-- a comparison operator , end of the program
--SELECT * FROM t WHERE a

-- valid expression , end of the program
--SELECT * FROM t WHERE a=

-- ORDER BY criteria
--SELECT N FROM E ORDER BY by

-- Only one TOP/LIMIT/FETCH specification is allowed , void
--select top 1 distinct * from t fetch first 1 rows only
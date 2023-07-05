/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DQL (Data Query Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SELECT(II) STATEMENTS

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


select * from t join s join u;
select * from (t join s join u);
select * from t join (s join u);
select * from t join s join u;

--select * from (t join s) join u;

/*select * from (t natural full join s) left join u on t.a=u.a;
select * from (t natural full join s) left join u on s.a=u.a;
select * from (t natural full join s) left join u on 10*s.c=u.b;
select * from (t natural full join s) left join u on 10*s.c=u.b or 100*t.b=u.b;
select * from (t natural full join s) natural full join u;*/

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   SELECT STATEMENTS 2 ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

-- a valid relation , 15
--select * from 1 right join t2

-- valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT) , 20/18
--select * from t1 t s join t2
--select * from t1 1 join t2
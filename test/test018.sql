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


select 1;
select 1 a;
select 1 a, a+1;
select a+2,1 a, a+1;
select a+2,1+1 a, a+1;


select * from t join s join u;
select * from (t join s join u);
select * from t join (s join u);


/*
select * from (t join s) join u;

select * from (t natural full join s) left join u on t.a=u.a;
select * from (t natural full join s) left join u on s.a=u.a;
select * from (t natural full join s) left join u on 10*s.c=u.b;
select * from (t natural full join s) left join u on 10*s.c=u.b or 100*t.b=u.b;
select * from (t natural full join s) natural full join u;
*/
/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   SELECT STATEMENTS 2 ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

-- a valid relation , 15
--select * from 1 right join t2

-- valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT) , 20/18
--select * from t1 t s join t2
--select * from t1 1 join t2
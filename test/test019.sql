/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DQL (Data Query Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

UNION, EXCEPT, MINUS, INTERSECT STATEMENTS

% DQLstmt ::=
%   (DQLstmt)
%   |
%   UBSQL

% UBSQL ::= 
%   DQLstmt UNION [ALL] DQLstmt
%   |
%   DQLstmt EXCEPT [ALL] DQLstmt
%   |
%   DQLstmt MINUS [ALL] DQLstmt
%   |
%   DQLstmt INTERSECT [ALL] DQLstmt*/

/*
select * from a union select * from b;
create view parent(parent,child) as select * from father union select * from mother;
select * from p union select * from q union select pqs.x,p.y from pqs,p where pqs.y=p.x union select pqs.x,q.y from pqs,q where pqs.y=q.x;
create view n(n) as select 0 union all select n+1 from n


select * from a except select * from b;
SELECT dni FROM vista1 EXCEPT ((SELECT dniEmp FROM distribucion) UNION (SELECT dniDir FROM proyectos));

CREATE VIEW vista2 AS SELECT dni FROM programadores INTERSECT SELECT dni FROM analistas;
*/

--TODO

/*
(select * from s) union (select * from t);
(select * from s) intersect (select * from t);
(select * from s) except (select * from t);
*/

select a from s where b not in (select c from t)



/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   UNION, EXCEPT, MINUS, INTERSECT STATEMENTS ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */


-- select statement , 23
--select * from a union selct * from b;
--SELECT * FROM s UNION Al SELECT * FROM q;

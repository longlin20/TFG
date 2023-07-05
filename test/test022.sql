/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DML (Data Manipulation Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

DELETE and UPDATE STATEMENTS

% DMLstmt ::=
%   DELETE FROM TableName [[AS] Identifier] [WHERE Condition]
%   |
%   UPDATE TableName [[AS] Identifier] SET Att=Expr {,Att=Expr} [WHERE Condition]*/


delete from t1
delete from t1 t
delete from t1 as "newTableName"
delete from t where b='a1';
DELETE FROM t WHERE edad > 0;
delete from t where b in (select * from a);
delete from t where not exists (select * from c where c.a=a.a);

delete from t WHERE ((a.age > 25) AND (salary > 50000))
delete from t WHERE ((city = 'San Francisco') AND ((age >= 25) OR (city = 'New York')))
delete from t WHERE (((status = 'active')) AND ((city = 'London')) OR ((age < 30)))

update t set a=1
update t1 as d set a=1
UPDATE Empleados SET Sueldo = Sueldo*1.1 
update t1 as c set a=(select b from s where s.a=c.a) where a=1;



/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   DELETE and UPDATE ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */


-- FROM , 8
--delete * from t

-- table name , end of program
--delete from 

-- valid WHERE condition , 21/23
--delete from t where >0
--update t set a=1 where

-- closing parenthesis '')''
--delete from t WHERE ((name = 'John Doe')
--delete from t WHERE (((a.age > 25)) AND ((salary > 50000))

-- SET , 10
--update t a=1

-- sequence of column assignments Col=Expr , end of program
--update t set 
--update t set a=1,
--update t set a=1, w

-- an expression , end of program
--update t set a=1, w=
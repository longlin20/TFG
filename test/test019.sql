/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DML (Data Manipulation Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

DELETE and UPTADE STATEMENTS

% DMLstmt ::=
%   DELETE FROM TableName [[AS] Identifier] [WHERE Condition]
%   |
%   UPDATE TableName [[AS] Identifier] SET Att=Expr {,Att=Expr} [WHERE Condition]*/

--delete from t1
--delete from t1 as "newTableName"
--delete from c where b='a1';
--DELETE FROM t WHERE edad > 0;
--delete from b where b in (select * from a);
--delete from a where not exists (select * from c where c.a=a.a);*/
--delete from "s"
--update t as d set a=1
--update t set a=1

--delete from t WHERE ((a.age > 25) AND (salary > 50000))
--delete from t WHERE ((city = 'San Francisco') AND ((age >= 25) OR (city = 'New York')))
--delete from t WHERE (((status = 'active')) AND ((city = 'London')) OR ((age < 30)))

/*ERROR*/

-- closing parenthesis ''')'''
--delete from t WHERE ((name = 'John Doe')
--delete from t WHERE (((a.age > 25)) AND ((salary > 50000))
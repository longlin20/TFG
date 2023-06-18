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
update t set a=1

/*ERROR*/

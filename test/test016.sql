/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DDL (Data Definition Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

DROP and CompleteSchema := DQLstmt STATEMENTS

% DDLstmt ::=
%   DROP TABLE DropTableClauses TableName{,TableName} DropTableClauses % Extended syntax following MySQL, SQL Server and others
%   |
%   DROP VIEW DropViewClauses ViewName DropViewClauses
%   |
%   DROP DATABASE [DatabaseName]
%   |
%   CompleteSchema := DQLstmt                    % Addition to support HR-SQL syntax */



drop table t
drop view v

drop table if exists t
drop table t cascade 
drop table t cascade constraints
drop view if exists v
drop view cascade v
drop view v if exists

drop database
drop database db
drop database $des

--my_view(age int) := SELECT age FROM my_table;


/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   DROP and CompleteSchema := DQLstmt STATEMENTS ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

-- TABLE, VIEW or DATABASE , 6
--drop t

-- EXISTS , 15
--drop table if exist t

-- table name or optional drop table clauses(IF EXISTS, CASCADE or CASCADE CONSTRAINTS) , 12
--drop table is t

-- view name or optional drop view clauses(IF EXISTS, CASCADE) , 11
--drop view is v

-- valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT) , 13
--drop view v exist

-- valid type , 13
--my_view(age it) := SELECT age FROM my_table

-- column identifier , 17
--my_view(age int,) := SELECT age FROM my_table

-- colon ':' , 18
--my_view(age int) = SELECT age FROM my_table

-- equals '=' , 20
--my_view(age int) : SELECT age FROM my_table
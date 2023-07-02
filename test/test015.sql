/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DDL (Data Definition Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ALTER and RENAME STATEMENTS

% DDLstmt ::=
%   ALTER TABLE TableName [ADD|DROP] | [[COLUMN] Att | CONSTRAINT [ConstraintName] TableConstraint] 
%   |
%   ALTER TABLE TableName ALTER [COLUMN] Att [AttDefinition | SET [DATA] TYPE Type]
%   |
%   RENAME TABLE TableName TO TableName
%   |
%   RENAME VIEW ViewName TO ViewName*/



alter table t1 add a int
alter table t1 add a int not null
alter table t1 add constraint primary key(a, c)
alter table t1 add  constraint not null b;
alter table t1 add  constraint unique(b);
alter table t1 add  constraint candidate key b;
alter table t1 add  constraint check a determined by b;
alter table t1 add  constraint check (a,b) determined by (a,b);
alter table t1 add  constraint check (a>0);

alter table t1 drop COLUMN a
alter table t1 drop a
alter table t1 drop constraint primary key a
alter table t1 drop constraint primary key(a)


alter table t1 alter column a1 set data type varchar(10)
alter table t1 alter column a1 string default ''

rename table t to s
rename view v to s



/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   ALTER and RENAME STATEMENTS ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

-- TABLE , 7
--alter t1 add constraint primary key(a)

-- Semantic unknown_table(t) , 13
--alter table t drop a

-- ALTER, ADD or DROP , 16
--alter table t1 ad a int

-- CONSTRAINT , 20
--alter table t1 add primary key(a)

-- valid table constraint (NOT, PRIMARY, UNIQUE, FOREIGN, CHECK, CANDIDATE)
--alter table t1 add constraint far key(a)

-- valid type , end of the program
--alter table t1 add a

-- valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT) , 45/20
--alter table t1 add constraint primary key(a), primary key(a)
--rename table t to s(a)

--Semantic unknown_column(t, a1)/unknown_column(t1, a) , 28/29
--alter table t alter column a1 set data type varchar(10)
--alter table t1 alter column a string default ''

-- SET , 32
--alter table t1 alter column a1 et data type varchar(10)

-- DATA TYPE or TYPE , 36/41
--alter table t1 alter column a1 set ata type varchar(10)
--alter table t1 alter column a1 set data tyep varchar(10)

-- TABLE or VIEW , 8
--rename t to s

-- table name , 19
--rename table t to (s)

-- TO , 15/16
--rename table t(a) to s
--rename table v as t

-- a positive integer , 46
--alter table t1 alter a1 set data type number()
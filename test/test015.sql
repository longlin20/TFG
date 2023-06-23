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


alter table t alter column a set data type varchar(10)
alter table t alter column a string default ''


/*
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


*/






/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   ALTER and RENAME STATEMENTS ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

-- valid table constraint (NOT, PRIMARY, UNIQUE, FOREIGN, CHECK, CANDIDATE)
--alter table t add constraint frimary key(a)

-- TABLE
--alter t add constraint primary key(a)

-- unknown_table(t) , 13
--alter table t drop a

-- ALTER , 16
--alter table t1 ad a int

-- valid type , end of the program
--alter table t1 add a

-- CONSTRAINT
--alter table t add primary key(a)

-- valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT) , 45
--alter table t1 add constraint primary key(a), primary key(a)


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



alter table t1 add a int;
alter table t1 add a int not null;
alter table t1 add constraint primary key(a, c);
alter table t1 add constraint not null b;
alter table t1 add constraint unique(b);
alter table t1 add constraint candidate key b;
alter table t1 add constraint check a determined by b;
alter table t1 add constraint check (a,b) determined by (a,b);
alter table t1 add constraint check (a>0);

alter table t1 drop COLUMN a;
alter table t1 drop a;
alter table t1 drop constraint primary key a;
alter table t1 drop constraint primary key(a);

alter table t1 alter column a1 set data type varchar(10);
alter table t1 alter column a1 string default '';

rename table t to s;
rename view v to s;


/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   ALTER and RENAME STATEMENTS ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

--test043-test060
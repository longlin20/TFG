/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DML (Data Manipulation Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

INSERT STATEMNENTS

% DMLstmt ::=
%   INSERT INTO TableName[(Att {,Att})] VALUES (ExprDef {,ExprDef}) {, (ExprDef {,ExprDef})}
%   |
%   INSERT INTO TableName DEFAULT VALUES
%   |
%   INSERT INTO TableName[(Att {,Att})] DQLstmt*/

/*insert into t1 select
insert into [t1] select
insert into "t1" select*/
--insert into t1 default values
--insert into t3 default values
--insert into t1 select 
--insert into t2 values(1, '2')
--insert into t1 values (1,'Ventas'), (2,'Contabilidad');
--insert into t3 values ('1','n1','d1'),('2','n2','d2');
--INSERT INTO  t1 VALUES (DATE '2000-060-01');*/
--INSERT INTO t3 VALUES (TIME '12:00:01', 2.5, 1), (DATE '2012-01-01', DEFAULT, NULL);
/*INSERT INTO  t2 VALUES (TIME '12:00:01', DATE '2000-0600-01')
INSERT INTO  t2 VALUES (TIME '12:00:01', DATE '2000-0600-01')*/
--INSERT INTO  T1 VALUES (TIMESTAMP '2023-06-01 13:45:30')

/*ERROR*/

-- table name
--insert into 222 values (1, '1')

-- Semantic: Unmatching number of values => 2 (must be 3)
--insert into t3 values (1, '1')

--
--insert into t1 values ('V1')    ('V2');

-- Expected closing parenthesis or comma 
--insert into t1 values(1 '2')
--insert into t3 values(1 '2')

-- Semantic: Invalid DATE String format => must be 'Int-Int-Int'
--INSERT INTO  t1 VALUES (DATE '2000-a-01');

-- Expected string
--INSERT INTO  t1 VALUES (DATE 2000);

-- VALUES, select statement, or DEFAULT VALUES
--insert into t1 defa values
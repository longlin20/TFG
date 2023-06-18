/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DML (Data Manipulation Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

INSERT INTO STATEMENTS

% DMLstmt ::=
%   INSERT INTO TableName[(Att {,Att})] VALUES (ExprDef {,ExprDef}) {, (ExprDef {,ExprDef})}
%   |
%   INSERT INTO TableName DEFAULT VALUES
%   |
%   INSERT INTO TableName[(Att {,Att})] DQLstmt*/

/*insert into `t1` select
insert into t1 select
insert into [t1] select
insert into "t1" select
insert into t1 default values
insert into t3 default values
insert into t2 values(1, '2')*/
--insert into t2 values (1,'Ventas'), (2,'Contabilidad')
--insert into t3 values ('1','n1','d1'),('2','n2','d2');
--INSERT INTO t1 VALUES (DATE '2011-06-1');
--INSERT INTO t1 VALUES (default), (NULL);

--INSERT INTO t3 VALUES (TIME '12:00:01', 2.5, 1), (DATE '2012-01-01', DEFAULT, NULL);
--INSERT INTO  t2 VALUES (TIME '12:00:01', DATE '2000-06-01')
--INSERT INTO  T1 VALUES (TIMESTAMP BC '2023-06-01 13:45:30'), (DATETIME '2023-06-17 17:35:45')
--insert into t1(a1) values (1)
--insert into t3(a3,b3,c3) values (1,2,'a')
--insert into t2(a3,b3,c3) values (1,2,'a')


/*ERROR*/

-- table name
--insert into 222 values (1, '1')

-- Semantic: Unmatching number of values => 2 (must be 3)
--insert into t3 values (1, '1')

-- Semantic: Unmatching number of values => 3 (must be 2)
--insert into t3(a2,a3) values (1,2,'a')

-- Semantic: Invalid DATE String format => must be 'Int-Int-Int'
--INSERT INTO  t1 VALUES (DATE '2000-a-01');

--
--insert into t2(a3,b3,a3) values (1,2,'a')

--
--insert into t1 values ('V1')    ('V2');

-- closing parenthesis or comma 
--insert into t1 values(1 '2')
--insert into t3 values(1 '2')

-- comma 
--insert into t2(a3 c) values (1,'a')

-- string
--INSERT INTO  t1 VALUES (DATE 2000);

-- VALUES 
--insert into t1 default v

-- VALUES, select statement, or DEFAULT VALUES
--insert into t1 default 
--insert into t1 defa values

-- closing bracket
--insert into [t1 select
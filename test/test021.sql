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


insert into t1 default values
insert into t3 default values

insert into t2 values(1.5E2, '2')
insert into t2 values (1,'Ventas'), (2,'Contabilidad')
insert into t3 values ('1','n1','d1'),('2','n2','d2');

INSERT INTO t1 VALUES (default);

INSERT INTO t3 VALUES (TIME '12:00:01', 2.5, -1), (DATE '2012-01-01', DEFAULT, 'A');
INSERT INTO  T1 VALUES (TIMESTAMP BC '2023-06-01 13:45:30'), (DATETIME '2023-06-17 17:35:45')
insert into t1(a1) values (-1.3E-2)
insert into t3(a3,b3,c3) values (1,2,'a')
insert into t2(a3,b3,c3) values (1,2,'a')

insert into c select a.a,b.b from a,b where a.a=b.b or b.b='a1';


/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   INSERT INTO STATEMENTS ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

-- table name , 13
--insert into 222 values (1, '1')

-- VALUES , 24
--insert into t1 default v

-- opening parenthesis ''('' , 22
--insert into t values 1

-- comma 
--insert into t2(a3 c) values (1,'a')

-- closing parenthesis '')'' or comma , 25
--insert into t2 values(1 '2')

--a sequence of columns between parentheses , 15/15
--insert into t()
--insert into t(1,2)

-- VALUES, select statement, or DEFAULT VALUES
--insert into t
--insert into t(a) (1)
--insert into t1 defa values

-- string , 30
--INSERT INTO  t1 VALUES (DATE 2000);

-- TIME String format must be 'Int(hour):Int(minute):Int(second)' , 29
--INSERT INTO t1 VALUES (TIME '122:07:01')

-- DATE String format must be [BC] 'Int(Year)-Int(month)-Int(day)' , 29
--INSERT INTO t1 VALUES (DATE '20116-00-02')

-- DATETIME/TIMESTAMP String format must be [BC] 'Int(Year)-Int(month)-Int(day) Int(hour):Int(minute):Int(second)' , 38
--INSERT INTO  t1 VALUES (TIMESTAMP BC '2023-06-01 1345:30')

-- Semantic: Unmatching number of values => 2 (must be 3)
--insert into t3 values (1, '1')

-- Semantic: Unmatching number of values => 3 (must be 2)
--insert into t3(a2,a3) values (1,2,'a')

-- Semantic, Column names must be different in [a3,b3,a3] , 15
--insert into t2(a3,b3,a3) values (1,2,'a')




/*
-- closing bracket
--insert into [t1 select
*/
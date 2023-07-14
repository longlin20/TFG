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

--test096-test109


-- closing bracket
--insert into [t1 select

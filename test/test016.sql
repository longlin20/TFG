--   INSERT INTO TableName[(Att {,Att})] VALUES (ExprDef {,ExprDef}) {, (ExprDef {,ExprDef})}
--   |
--   INSERT INTO TableName DEFAULT VALUES
--   |
--   INSERT INTO TableName[(Att {,Att})] DQLstmt


--insert into t1 select
--insert into [t1] select
--insert into "t1" select
--insert into t1 default values
--insert into t2 default values
--insert into t1 select 
--insert into t3 values (1, '1')
--insert into t2 values(1, '2')
--insert into t1 values (1,'Ventas'), (2,'Contabilidad');
--insert into t3 values ('1','n1','d1'),('2','n2','d2');
INSERT INTO  t2 VALUES (DATE '2000-0600-01', 2.5)
INSERT INTO  t1 VALUES (TIME '12:00:01')
INSERT INTO  t2 VALUES (TIME '12:00:01', DATE '2000-0600-01')


/*ERROR*/

-- Expected Table name
--insert into e values (1, '1')

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
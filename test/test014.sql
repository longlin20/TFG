/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DDL (Data Definition Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

CREATE STATEMENTS

% DDLstmt ::=
%   CREATE [OR REPLACE] TABLE CompleteConstrainedSchema
%   |
%   CREATE [OR REPLACE] TABLE TableName [(] LIKE TableName [)]
%   |
%   CREATE [OR REPLACE] TABLE TableName [(] AS DQLstmt [)]
%   |
%   CREATE [OR REPLACE] VIEW Schema AS DQLstmt
%   |
%   CREATE DATABASE DatabaseName*/


/*
create table t(a int);
create table "t"("a" int)
create table [t]([a] int)
create table c(a string,b string);
create table edge(origin string,destination string);
create table flights(airline string,frm string,"to" string,departs int,arrives int);
CREATE TABLE employee(Name VARCHAR(20), Department VARCHAR(20), Salary INT);
create table emp(dni string primary key, numdep int references dpto(nd));
create table trab(dni string references emp, npro int, primary key(dni,npro));
create table takes(eID string, cID string, tYear int, tMonth int, tDay int, primary key (eID, cID));
CREATE TABLE flight(origin string, destination string, "time" real);
create table emp(dnisupervisor string, check dnisupervisor in select dni from emp);


create or replace table t(a int, b int);
create or replace table t(a int, b int, foreign key (a) references s)
create or replace table t(a int, b int, c int, d int, check (a,b) determined by (c,d))


create table t like s

create table t3(a3,b3,c3) as select a from n

create view v("a") as select b from "t"
create view "v"("a") as select b as c from "t"
create view "v"(a) as select b.* from "t"
create view "v"([a]) as select * from "t"

create database x
*/


/*ERROR , column*/

-- valid type , 17/19
--create table t(a) 
--create table t(a intiger)
--create table emp(a, check dnisupervisor in select dni from emp);

--AS, LIKE or column identifier , 16
--create table t('a' intiger)
--create table emp(check dnisupervisor in select dni from emp);

-- NULL , 37
--create or replace table t(a int not nul)

-- KEY, 41
--create or replace table t(a int primary kye) 

-- TABLE or VIEW , 19
--create or replace able t(a int);

-- view schema , the end of the program
--create view

-- AS , 18
--create view v(a) s

-- SQL DQL statement , the end of the program
--create view v(a) as

-- KEY , 42
--create or replace table t(a int candidate)

-- BY , 44
--create or replace table t(a int determined from a)

-- table name , 43
--create or replace table t(a int references)

--  a column name , 46
--create or replace table t(a int references s()a)

-- comma or column constraints , 33
--create or replace table t(a int defaul 0)

-- positive integer , 34/38
--create or replace table t(a char())
--create or replace table t(a number(1,))

-- OR REPLACE, TABLE, VIEW or DATABASE , 8
--create replace table t(a int);

-- REPLACE
--create or table t(a int);

-- valid SQL DDL statement (table name) , 21
--create table t like 2

-- valid SQL DQL statement (SELECT, WITH or ASSUME) , the end of the program
--create table t(a) as

--AS, or column name , 19/19
--create table t(a) like s
--create table t(a) a




-------------------------------------------------------------

--TODO todos los errores de  TableConstraint


--INCORRECT 
-- 
--create view v as select 1

--INCORRECT
-- a sequence of column names
--create view v() as select 1

-- NOT CORRECT
-- Expected valid column constraint (NOT, NULL, PRIMARY, UNIQUE, REFERENCES, DEFAULT, CHECK, CANDIDATE, DETERMINED) , 46
--???create or replace table t(a int references s null)
--create or replace table t(a int references s a)

-- 
--create or replace table t(a int foreign key s)

-- NOT CORRECT expect where condition or (Att {,Att}) DETERMINED BY (Att {,Att})
-- 
--create or replace table t(a integer check (>0))

-- NOT CORRECT
-- where condition or a list of columns followed by ''DETERMINED BY'' and another list of columns , 43
--create or replace table t(a integer check >0)

-- NOT CORRECT expect where condition or list of columns
-- create or replace table t(a integer check b DETERMINED BY c,d)

-- comma or closing parenthesis ')' , 18/58
--???create table t(a intiger)
--create or replace table t(a int, b int, foreign key (a,b references s)


-- valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT) , 27/23
--create table t(a integer) as --INCORRECT espect QDLstmt
--create table t like sa)

--INCORRECT eExpect valid SQL statement
--create table t like s (a)


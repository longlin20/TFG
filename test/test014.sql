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
/*create table emp(dnisupervisor string, check dnisupervisor in select dni from emp);*/
create table t(a integer check (a>-1E-1))

create or replace table t(a int, b int);
create or replace table t(a int, b int, foreign key (a) references s)
create or replace table t(a int, b int, c int, d int, check (a,b) determined by (c,d))

create table t like s
/*
create table t3(a3,b3,c3) as select a from n

create view v("a") as select b from "t"
create view "v"("a") as select b as c from "t"
create view "v"([a]) as select b.* from "t"
create view v(x,y) as select * from a left join b on x=y where x>1
*/
create database x


/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   CREATE STATEMENTS ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */


-- REPLACE , 11
--create or table t(a int);

-- OR REPLACE, TABLE, VIEW or DATABASE , 8
--create replace table t(a int);

-- TABLE or VIEW , 19
--create or replace able t(a int);

--AS, LIKE or column identifier , 16/18
--create table t('a' intiger)
--create table emp(check dnisupervisor in select dni from emp);

-- valid type , 17/18/19/62
--create table t(a) 
--create table t(a intiger)
--create table emp(a, null b);
--create or replace table t(a integer check b DETERMINED BY c,d)

-- typed schema , 14
--create table t

-- a positive integer , 34/38
--create or replace table t(a char())
--create or replace table t(a number(1,0))

-- NULL , 37
--create or replace table t(a int not nul)

-- KEY, 41/42
--create or replace table t(a int primary kye) 
--create or replace table t(a int candidate)

-- BY , 44
--create or replace table t(a int determined from a)

-- table name , 43
--create or replace table t(a int references)

--  a column name , 46
--create or replace table t(a int references s()a)

-- comma or column constraints , 33
--create or replace table t(a int defaul 0)

-- REFERENCES , 47
--create or replace table t(a int, foreign key s)

-- comma or closing parenthesis ')' , 58
--create or replace table t(a int, b int, foreign key (a,b references s)

-- valid column constraint (NOT, NULL, PRIMARY, UNIQUE, REFERENCES, DEFAULT, CHECK, CANDIDATE, DETERMINED) , 46
--create or replace table t(a int references s a)

-- valid table constraint (NOT, PRIMARY, UNIQUE, FOREIGN, CHECK, CANDIDATE) , 43
--create or replace table t(a int, unique s,)

-- valid SQL DQL statement (SELECT, WITH or ASSUME) , the end of the program
--create table t(a) as 

--AS, or column name , 19/19
--create table t(a) like s
--create table t(a) a

-- valid SQL DDL statement (table name) , 21
--create table t like 2

-- valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT) , 23
--create table t like sa)

-- view schema , the end of the program
--create view

-- AS , 18
--create view v(a) s

-- valid SQL DQL statement (SELECT, WITH or ASSUME) , the end of the program
--create view v(a) as

-- column sequence separated by commas , 15
--create view v() as select * from a
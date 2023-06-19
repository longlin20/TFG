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

--create or replace table t(a int candidate)
--create or replace table t(a int determined from a)
--create or replace table t(a int references)
--create or replace table t(a int references s a)
--create or replace table t(a int references s()a)
--create or replace table t(a int foreign key s)
--create or replace table t(a int defaul 0)
--create or replace table t(a integer check (>0))
--create or replace table t(a integer check >0)
--create or replace table t(a int, b int, foreign key a,b references s)
--create or replace table t(a int, b int, c int, d int, check (a,b) determined by c,d)
--create or replace table t(a char())
--create or replace table t(a number(1,))
create or replace table t(a int, b int);
create table c(a string,b string);
create table edge(origin string,destination string);
create table flights(airline string,frm string,"to" string,departs int,arrives int);
CREATE TABLE employee(Name VARCHAR(20), Department VARCHAR(20), Salary INT);
create table "t"("a" int)
create table [t]([a] int)
create table emp(dni string primary key, numdep int references dpto(nd));
--create table emp(dni string primary key, sueldo int, dnisupervisor string, check dnisupervisor in select dni from emp);
create table trab(dni string references emp, npro int, primary key(dni,npro));
create table takes(eID string, cID string, tYear int, tMonth int, tDay int, primary key (eID, cID));
CREATE TABLE flight(origin string, destination string, "time" real);


/*ERROR*/

-- valid type
--create table t(a)
--create table t(a intiger)

-- a column name
--create table t('a' intiger)

-- 
--create table t(a integer) as

-- NULL
--create or replace table t(a int not nul)

-- KEY
--create or replace table t(a int primary kye) 

--
--create replace table t(a int);

-- 
--create or table t(a int);

-- 
--create or replace able t(a int);
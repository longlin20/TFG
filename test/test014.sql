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


--create table t(a integer check (a>4.13E2))


create table t(a int);
create table "t"("a" int);
create table [t]([a] int);
create table c(a string,b string);
create table edge(origin string,destination string);
create table flights(airline string,frm string, "to" string,departs int,arrives int);
CREATE TABLE employee(Name VARCHAR(20), Department VARCHAR(20), Salary INT);
create table emp(dni string primary key, numdep int references dpto(nd));
create table trab(dni string references emp, npro int, primary key(dni,npro));
create table takes(eID string, cID string, tYear int, tMonth int, tDay int, primary key (eID, cID));
CREATE TABLE flight(origin string, destination string, "time" real);
create table emp(dnisupervisor string, check dnisupervisor in select dni from emp);
create table t(a integer check (a>-1E-1));

create or replace table t(a int, b int);
create or replace table t(a int, b int, foreign key a references s);
create or replace table t(a int, b int, c int, d int, check (a,b) determined by (c,d));

create table t like s;

create table t3(a3,b3,c3) as select a from n;
create table t3(a3,b3,c3) as select a from n as a;

create view v("a") as select b from "t";
create view "v"("a") as select b as c from "t";
create view "v"([a]) as select b.* from "t";
create view v(x,y) as select * from a left join b on x=y where x>1;


create database x;




/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   CREATE STATEMENTS ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

--test010-041
/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % ISL (Information Schema Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ISLstmt ::=
%   SHOW TABLES
%   |
%   SHOW VIEWS
%   |
%   SHOW DATABASES
%   |
%   DESCRIBE [TableName|ViewName]*/

show tables

show views

SHOW DATABASES

DESCRIBE t

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   ISL STATEMENTS ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

-- table name , 10
--DESCRIBE 2

-- TABLES, VIEWS or DATABASES , 6/end of program
--show t
--show
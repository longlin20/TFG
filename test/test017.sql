/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DQL (Data Query Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SELECT STATEMENTS

% DQLstmt ::=
%   (DQLstmt) 
%   |
%   UBSQL

% UBSQL ::= 
%   SELECTstmt */


--select distinct * from t
--select distinct top 1 * from t
--select top 1 * from t
--select top 1 distinct * from t


--select distinct * from t fetch first 1 rows only
--select top 1 distinct * from t fetch first 1 rows only

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   SELECT STATEMENTS ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

--SELECT list , 14/23
--select top 1 from t
--select distinct top 1 top * from t

-- FROM clause , 10
--select a top 1 * from t

--
--select a * from t
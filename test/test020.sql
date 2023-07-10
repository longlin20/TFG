/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DQL (Data Query Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

WITH and ASSUME STATEMENTS

% DQLstmt ::=
%   (DQLstmt) 
%   |
%   UBSQL

% UBSQL ::= 
%   WITH LocalViewDefinition {,LocalViewDefinition} DQLstmt
%   |
%   ASSUME LocalAssumption {,LocalAssumption} DQLstmt  % Not in the standard*/


assume select 1 in l(a) select * from l;
assume select 1 in l(a), select 1 not in l(a), select 1 in l(a) select * from l;

ASSUME 
    (SELECT flight.origin,connect.destination
     FROM flight,connect
     WHERE flight.destination = connect.origin) 
  IN 
    connect(origin,destination) 
SELECT * FROM connect;

ASSUME 
    SELECT 'mad','lon',-2.204 
    UNION
    SELECT 'par','ber',3.0
  IN 
    flight(origin,destination,"time") 
SELECT * FROM travel;


with p(a) as select a from t select a from p;

with p(a) as select 1 union select a+1 from p select top 10 a from p;

create view 
paths(origin,destination) as 
with recursive path(origin,destination) as 
(select * from edge)
 union 
(select path.origin,edge.destination from path,edge where path.destination=edge.origin) 
select * from path;

WITH RECURSIVE Reaches(frm,"to") AS (SELECT frm,"to" FROM flights) UNION (SELECT R1.frm,R2."to" FROM Reaches AS R1, Reaches AS R2 WHERE R1."to"=R2.frm) SELECT * FROM Reaches;

create view reach(frm,"to") as WITH Triples(airline,frm,"to") AS SELECT airline,frm,"to" FROM flights, RECURSIVE Reaches(airline,frm,"to") AS (SELECT * FROM Triples) UNION (SELECT Triples.airline,Triples.frm,Reaches."to" FROM Triples,Reaches WHERE Triples."to" = Reaches.frm AND Triples.airline=Reaches.airline) (SELECT frm,"to" FROM Reaches WHERE airline = 'UA') EXCEPT (SELECT frm,"to" FROM Reaches WHERE airline = 'AA');

WITH 
  even(x) AS
  SELECT 0
  UNION ALL
  SELECT odd.x+1
  FROM odd
  WHERE x < 10
  ,
  odd(x) AS
  SELECT even.x+1
  FROM even
  WHERE x < 10
  SELECT x FROM odd
 ;


with recursive p(x) as select * from r
except select * from q,
recursive q(x) as select * from r 
except select * from p
select * from p;


with media(m) as (select avg(a) from t) select a from t, media where a>m;


with v(a) as select 0 select 1/a from v where a>0;


/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   WITH and ASSUME STATEMENTS ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

-- SELECT statement , end of the program
--assume select 1 in t(a)
--with v(a) as select 1
--with v(a) as select 1 from dual

-- AS , 11
--with v(a) select 1 select * from v

-- schema , 6
--with 2 select 1 select * from v
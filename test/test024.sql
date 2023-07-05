/*% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % TML (Transaction Management Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TMLstmt ::=
%   COMMIT [WORK]
%   |
%   ROLLBACK [WORK] [TO SAVEPOINT SavepointName]
%   |
%   SAVEPOINT SavepointName*/

COMMIT
COMMIT WORK
ROLLBACK
ROLLBACK WORK
ROLLBACK WORK TO SAVEPOINT "sp1"
ROLLBACK TO SAVEPOINT "sp1"
SAVEPOINT "sp2"

/* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   TML STATEMENTS ERROR , column
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% */

--  SAVEPOINT , 18/15
--ROLLBACK WORK TO point "sp1"
--ROLLBACK TO point "sp1"

-- double quotes id (savepoint name) , 28/23
--ROLLBACK WORK TO SAVEPOINT sp1
--ROLLBACK TO SAVEPOINT 'sp1'
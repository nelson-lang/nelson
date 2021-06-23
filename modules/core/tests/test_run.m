%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http:%www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
assert_isequal(nargin('run'), 3);
assert_isequal(nargout('pause'), 1);
%=============================================================================
addpath([modulepath('core'), '/tests']);
%=============================================================================
PATH_REF_1 = [modulepath('core'), '/tests/run_file1.m'];
if length(PATH_REF_1) > 50
    PATH_REF_1 = 'run_file1.m';
end
%=============================================================================
PATH_REF_2 = [modulepath('core'), '/tests/run_file2.m'];
if length(PATH_REF_2) > 50
    PATH_REF_2 = 'run_file2.m';
end
%=============================================================================
PATH_REF_3 = [modulepath('core'), '/tests/test_run.m'];
if length(PATH_REF_3) > 50
    PATH_REF_3 = 'test_run.m';
end
%=============================================================================
try
  run([modulepath('core'), '/tests/run_file1.m']);
  msg = '';
catch
  msg = getLastReport();
end
%=============================================================================
REF = '
Error: 
Undefined variable: TEST_CONDITION

at line     8 of ''PATH_REF_2''
at line     6 of ''PATH_REF_1''
at line    49 of ''PATH_REF_3''
';
REF = replace(REF, 'PATH_REF_2', PATH_REF_2);
REF = replace(REF, 'PATH_REF_1', PATH_REF_1);
REF = replace(REF, 'PATH_REF_3', PATH_REF_3);
assert_isequal(msg, REF);
%=============================================================================
TEST_CONDITION = 1;
try
  run([modulepath('core'), '/tests/run_file1.m']);
  msg = '';
catch
  msg = getLastReport();
end
%=============================================================================
REF = '
Error in clc
Wrong number of input arguments.

at line    10 of ''PATH_REF_2''
at line     6 of ''PATH_REF_1''
at line    63 of ''PATH_REF_3''
';
REF = replace(REF, 'PATH_REF_2', PATH_REF_2);
REF = replace(REF, 'PATH_REF_1', PATH_REF_1);
REF = replace(REF, 'PATH_REF_3', PATH_REF_3);
assert_isequal(msg, REF);
%=============================================================================
TEST_CONDITION = 2;
try
  run([modulepath('core'), '/tests/run_file1.m']);
  msg = '';
catch
  msg = getLastReport();
end
%=============================================================================
REF = '
Error in fun_run (line 7)
qqq

at line    12 of ''PATH_REF_2''
at line     6 of ''PATH_REF_1''
at line    77 of ''PATH_REF_3''
';
REF = replace(REF, 'PATH_REF_2', PATH_REF_2);
REF = replace(REF, 'PATH_REF_1', PATH_REF_1);
REF = replace(REF, 'PATH_REF_3', PATH_REF_3);
assert_isequal(msg, REF);
%=============================================================================
TEST_CONDITION = 3;
try
  run([modulepath('core'), '/tests/run_file1.m']);
  msg = '';
catch
  msg = getLastReport();
end
%=============================================================================
REF = '
Error: 
g

at line    14 of ''PATH_REF_2''
at line     6 of ''PATH_REF_1''
at line    91 of ''PATH_REF_3''
';
REF = replace(REF, 'PATH_REF_2', PATH_REF_2);
REF = replace(REF, 'PATH_REF_1', PATH_REF_1);
REF = replace(REF, 'PATH_REF_3', PATH_REF_3);
assert_isequal(msg, REF);
%=============================================================================

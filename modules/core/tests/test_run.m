%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
assert_isequal(nargin('run'), 3);
assert_isequal(nargout('pause'), 1);
%=============================================================================
addpath([modulepath('core'), '/tests']);
%=============================================================================
PATH_REF_1 = [modulepath('core', 'tests'), '/run_file1.m'];
if length(PATH_REF_1) > 50
  PATH_REF_1 = 'run_file1.m';
end
%=============================================================================
PATH_REF_2 = [modulepath('core', 'tests'), '/run_file2.m'];
if length(PATH_REF_2) > 50
  PATH_REF_2 = 'run_file2.m';
end
%=============================================================================
PATH_REF_3 = [modulepath('core', 'tests'), '/test_run.m'];
if length(PATH_REF_3) > 50
  PATH_REF_3 = 'test_run.m';
end
%=============================================================================
try
  run([modulepath('core', 'tests'), '/run_file1.m']);
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
at line    33 of ''PATH_REF_3''
';
REF = replace(REF, 'PATH_REF_2', PATH_REF_2);
REF = replace(REF, 'PATH_REF_1', PATH_REF_1);
REF = replace(REF, 'PATH_REF_3', PATH_REF_3);
assert_isequal(msg, REF);
%=============================================================================
TEST_CONDITION = 1;
try
  run([modulepath('core', 'tests'), '/run_file1.m']);
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
at line    47 of ''PATH_REF_3''
';
REF = replace(REF, 'PATH_REF_2', PATH_REF_2);
REF = replace(REF, 'PATH_REF_1', PATH_REF_1);
REF = replace(REF, 'PATH_REF_3', PATH_REF_3);
assert_isequal(msg, REF);
%=============================================================================
TEST_CONDITION = 2;
try
  run([modulepath('core', 'tests'), '/run_file1.m']);
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
at line    61 of ''PATH_REF_3''
';
REF = replace(REF, 'PATH_REF_2', PATH_REF_2);
REF = replace(REF, 'PATH_REF_1', PATH_REF_1);
REF = replace(REF, 'PATH_REF_3', PATH_REF_3);
assert_isequal(msg, REF);
%=============================================================================
TEST_CONDITION = 3;
try
  run([modulepath('core', 'tests'), '/run_file1.m']);
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
at line    75 of ''PATH_REF_3''
';
REF = replace(REF, 'PATH_REF_2', PATH_REF_2);
REF = replace(REF, 'PATH_REF_1', PATH_REF_1);
REF = replace(REF, 'PATH_REF_3', PATH_REF_3);
assert_isequal(msg, REF);
%=============================================================================

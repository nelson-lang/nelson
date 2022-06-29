%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
fptr = str2func('cos');
f = parfeval(backgroundPool, fptr, 0, 55);
R = wait(f, 'finished');
assert_isequal(f.State, 'finished');
%=============================================================================
fptr = str2func('cos');
f = parfeval(backgroundPool, fptr, 0, 55);
pause(2);
R = wait(f, 'running');
assert_isequal(f.State, 'finished');
assert_istrue(R);
%=============================================================================
fptr = str2func('pause');
f = parfeval(backgroundPool, fptr, 0, 55);
tic()
R = wait(f, 'finished', 1);
T = toc();
assert_isequal(f.State, 'running');
assert_isfalse(R);
assert_isapprox(T, 1, 1e-1);
%=============================================================================
fptr = str2func('pause');
f = parfeval(backgroundPool, fptr, 0, 55);
tic()
R = wait(f, 'running', 1);
T = toc();
assert_isequal(f.State, 'running');
assert_istrue(R);
%=============================================================================
fptr = str2func('pause');
for i = 1:15
 f(i) = parfeval(backgroundPool, fptr, 0, 5);
end
tic()
R = wait(f, 'finished');
toc()
assert_istrue(R);
%=============================================================================
fptr = str2func('pause');
for i = 1:15
 f(i) = parfeval(backgroundPool, fptr, 0, 5);
end
tic()
R = wait(f, 'running');
toc()
assert_istrue(R);
%=============================================================================


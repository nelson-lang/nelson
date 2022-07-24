%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
F = str2func('cos');
assert_isequal(F(.5), cos(.5));
%=============================================================================
F = str2func('@cos')
assert_isequal(F(.5), cos(.5));
%=============================================================================
A = str2func('@(x) x*sqrt(x);');
R = eval('A(4)');
assert_isequal(R, 8);
B = A(4);
assert_isequal(B, 8);
%=============================================================================
A = str2func('@(x) x*sqrt(x)');
R = eval('A(4)');
assert_isequal(R, 8);
B = A(4);
assert_isequal(B, 8);
%=============================================================================
A = str2func('@(x) disp(x+1)');
A(4)
assert_checkerror('B = A(4)', _('Wrong number of output arguments.'));
%=============================================================================
b = str2func('@(x, y) find(x > y)')
M = rand(4, 3, 5);
[R, C] = b(M, 0.9)
REF_R = [2     4     1     2     4     1     2     4     4     3     1]';
REF_C = [ 1     1     3     3     3     4     5     5     6    10    15]';
assert_isequal(R, REF_R);
assert_isequal(C, REF_C);
assert_checkerror('[R, C,d,e] = b(M, 0.9)', _('Wrong number of output arguments.'));
%=============================================================================

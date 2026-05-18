%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('allbetween'), -1);
assert_isequal(nargout('allbetween'), 1);
%=============================================================================
A = [2 3 4];
R = allbetween(A, 2, 4);
assert_istrue(R);
%=============================================================================
A = [2 3 4];
R = allbetween(A, 2, 4, 'open');
assert_isfalse(R);
%=============================================================================
A = [3 4];
R = allbetween(A, 2, 4, 'openleft');
assert_istrue(R);
R = allbetween(A, 2, 4, 'openright');
assert_isfalse(R);
%=============================================================================
A = [2 3; 4 5];
R = allbetween(A, 2, 5);
assert_istrue(R);
R = allbetween(A, 2, 4);
assert_isfalse(R);
%=============================================================================
A = int16([-1 0 1]);
R = allbetween(A, int8(-1), uint8(1));
assert_istrue(R);
%=============================================================================
A = uint32([2 3 4]);
R = allbetween(A, uint16(2), uint64(4), 'open');
assert_isfalse(R);
%=============================================================================
A = single([2.5 3 3.5]);
R = allbetween(A, single(2), single(4), 'open');
assert_istrue(R);
%=============================================================================
A = [complex(3, 4) complex(0, 3)];
R = allbetween(A, 3, 5);
assert_istrue(R);
R = allbetween(A, 3, 5, 'openright');
assert_isfalse(R);
%=============================================================================
A = [complex(1, 0) complex(0, 2); complex(3, 4) complex(5, 12)];
R = allbetween(A, [1 2], [2; 13]);
assert_istrue(R);
%=============================================================================
A = logical([true true false]);
R = allbetween(A, false, true);
assert_istrue(R);
R = allbetween(A, false, true, 'openleft');
assert_isfalse(R);
%=============================================================================
A = string({'bee', 'cat'});
R = allbetween(A, string('ant'), string('dog'));
assert_istrue(R);
%=============================================================================
A = uint8([2 3; 4 5]);
R = allbetween(A, uint8([2; 4]), uint8([4 5]));
assert_istrue(R);
%=============================================================================
T = table([2; 3; 4], [10; 11; 12], 'VariableNames', {'A', 'B'});
R = allbetween(T, 2, 4, 'DataVariables', 'A');
assert_istrue(R);
R = allbetween(T, 2, 4);
assert_isfalse(R);
R = allbetween(T, 2, 12, 'DataVariables', {'A', 'B'});
assert_istrue(R);
R = allbetween(T, 2, 12, 'DataVariables', 2);
assert_istrue(R);
%=============================================================================

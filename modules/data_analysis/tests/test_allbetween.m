%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('allbetween'), -1);
asserts.isequal(nargout('allbetween'), 1);
%=============================================================================
A = [2 3 4];
R = allbetween(A, 2, 4);
asserts.istrue(R);
%=============================================================================
A = [2 3 4];
R = allbetween(A, 2, 4, 'open');
asserts.isfalse(R);
%=============================================================================
A = [3 4];
R = allbetween(A, 2, 4, 'openleft');
asserts.istrue(R);
R = allbetween(A, 2, 4, 'openright');
asserts.isfalse(R);
%=============================================================================
A = [2 3; 4 5];
R = allbetween(A, 2, 5);
asserts.istrue(R);
R = allbetween(A, 2, 4);
asserts.isfalse(R);
%=============================================================================
A = int16([-1 0 1]);
R = allbetween(A, int8(-1), uint8(1));
asserts.istrue(R);
%=============================================================================
A = uint32([2 3 4]);
R = allbetween(A, uint16(2), uint64(4), 'open');
asserts.isfalse(R);
%=============================================================================
A = single([2.5 3 3.5]);
R = allbetween(A, single(2), single(4), 'open');
asserts.istrue(R);
%=============================================================================
A = [complex(3, 4) complex(0, 3)];
R = allbetween(A, 3, 5);
asserts.istrue(R);
R = allbetween(A, 3, 5, 'openright');
asserts.isfalse(R);
%=============================================================================
A = [complex(1, 0) complex(0, 2); complex(3, 4) complex(5, 12)];
R = allbetween(A, [1 2], [2; 13]);
asserts.istrue(R);
%=============================================================================
A = logical([true true false]);
R = allbetween(A, false, true);
asserts.istrue(R);
R = allbetween(A, false, true, 'openleft');
asserts.isfalse(R);
%=============================================================================
A = string({'bee', 'cat'});
R = allbetween(A, string('ant'), string('dog'));
asserts.istrue(R);
%=============================================================================
A = uint8([2 3; 4 5]);
R = allbetween(A, uint8([2; 4]), uint8([4 5]));
asserts.istrue(R);
%=============================================================================
T = table([2; 3; 4], [10; 11; 12], 'VariableNames', {'A', 'B'});
R = allbetween(T, 2, 4, 'DataVariables', 'A');
asserts.istrue(R);
R = allbetween(T, 2, 4);
asserts.isfalse(R);
R = allbetween(T, 2, 12, 'DataVariables', {'A', 'B'});
asserts.istrue(R);
R = allbetween(T, 2, 12, 'DataVariables', 2);
asserts.istrue(R);
%=============================================================================
asserts.istrue(allbetween([2 3 4], 2, 4));
asserts.isfalse(allbetween([2 3 4], 2, 4, 'open'));
%=============================================================================

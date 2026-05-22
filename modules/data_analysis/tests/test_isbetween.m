%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('isbetween'), -1);
assert_isequal(nargout('isbetween'), 1);
%=============================================================================
A = [1 2 3 4 5];
R = isbetween(A, 2, 4);
REF = logical([0 1 1 1 0]);
assert_isequal(R, REF);
%=============================================================================
R = isbetween(A, 2, 4, 'open');
REF = logical([0 0 1 0 0]);
assert_isequal(R, REF);
%=============================================================================
R = isbetween(A, 2, 4, 'openleft');
REF = logical([0 0 1 1 0]);
assert_isequal(R, REF);
R = isbetween(A, 2, 4, 'closedright');
assert_isequal(R, REF);
%=============================================================================
R = isbetween(A, 2, 4, 'openright');
REF = logical([0 1 1 0 0]);
assert_isequal(R, REF);
R = isbetween(A, 2, 4, 'closedleft');
assert_isequal(R, REF);
%=============================================================================
A = [1 2 3; 4 5 6];
low = [1 3 3];
high = [4 5 5];
R = isbetween(A, low, high);
REF = logical([1 0 1; 1 1 0]);
assert_isequal(R, REF);
%=============================================================================
A = int8([-3 -2 -1 0 1 2 3]);
R = isbetween(A, int8(-1), int8(2));
REF = logical([0 0 1 1 1 1 0]);
assert_isequal(R, REF);
%=============================================================================
A = uint16([1 2 3 4 5]);
R = isbetween(A, uint8(2), uint32(4));
REF = logical([0 1 1 1 0]);
assert_isequal(R, REF);
%=============================================================================
A = single([1 1.5 2 2.5 3]);
R = isbetween(A, single(1.5), single(2.5), 'openright');
REF = logical([0 1 1 0 0]);
assert_isequal(R, REF);
%=============================================================================
A = [complex(1, 1) complex(3, 4) complex(4, 4)];
R = isbetween(A, 2, 5);
REF = logical([0 1 0]);
assert_isequal(R, REF);
R = isbetween(A, complex(0, 2), complex(3, 4), 'openleft');
REF = logical([0 1 0]);
assert_isequal(R, REF);
%=============================================================================
A = [complex(1, 0) complex(0, 2); complex(3, 4) complex(5, 12)];
low = [1 2];
high = [2; 5];
R = isbetween(A, low, high);
REF = logical([1 1; 1 0]);
assert_isequal(R, REF);
%=============================================================================
A = logical([0 1 1 0]);
R = isbetween(A, false, true);
REF = logical([1 1 1 1]);
assert_isequal(R, REF);
R = isbetween(A, false, true, 'openleft');
REF = logical([0 1 1 0]);
assert_isequal(R, REF);
%=============================================================================
A = uint8([1 2; 3 4; 5 6]);
low = uint8([1; 3; 5]);
high = uint8([2 4]);
R = isbetween(A, low, high);
REF = logical([1 1; 0 1; 0 0]);
assert_isequal(R, REF);
%=============================================================================
A = string({'ant', 'bee', 'cat'});
R = isbetween(A, string('bee'), string('cat'));
REF = logical([0 1 1]);
assert_isequal(R, REF);
%=============================================================================
A = ['a' 'b' 'c' 'd'];
R = isbetween(A, 'b', 'c');
REF = logical([0 1 1 0]);
assert_isequal(R, REF);
%=============================================================================
A = [NaN 2 3];
R = isbetween(A, 1, 3);
REF = logical([0 1 1]);
assert_isequal(R, REF);
%=============================================================================
A = zeros(0, 3);
R = isbetween(A, 1, 3);
REF = logical(zeros(0, 3));
assert_isequal(R, REF);
%=============================================================================
T = table([1; 2; 3], [4; 5; 6], 'VariableNames', {'A', 'B'});
R = isbetween(T, 2, 5);
REF = logical([0 1; 1 1; 1 0]);
assert_isequal(R, REF);
%=============================================================================
R = isbetween(T, 2, 5, 'DataVariables', 'A');
REF = logical([0; 1; 1]);
assert_isequal(R, REF);
%=============================================================================
R = isbetween(T, table([2; 2; 2], [4; 4; 4], 'VariableNames', {'A', 'B'}), ...
    table([3; 3; 3], [5; 5; 5], 'VariableNames', {'A', 'B'}));
REF = logical([0 1; 1 1; 1 0]);
assert_isequal(R, REF);
%=============================================================================
R = isbetween(T, 2, 5, 'DataVariables', {'A', 'B'});
REF = logical([0 1; 1 1; 1 0]);
assert_isequal(R, REF);
%=============================================================================
R = isbetween(T, 2, 5, 'DataVariables', 2);
REF = logical([1; 1; 0]);
assert_isequal(R, REF);
%=============================================================================
R = isbetween(T, 2, 5, 'DataVariables', [false true], 'OutputFormat', 'tabular');
REF = table(logical([1; 1; 0]), 'VariableNames', {'B'});
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('isbetween([1 2], 1, 2, ''badinterval'')', _('Invalid interval type.'));
%=============================================================================

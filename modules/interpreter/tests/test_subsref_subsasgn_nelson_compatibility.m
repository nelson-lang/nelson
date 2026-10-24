%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
A = [1 4 7 10; 2 5 8 11; 3 6 9 12];
asserts.isequal(A(2, [1 3 4]), [2 8 11]);
asserts.isequal(A(end, end - 1), 9);
%=============================================================================
B = A;
B(2, [1 3]) = [20 80];
asserts.isequal(B, [1 4 7 10; 20 5 80 11; 3 6 9 12]);
%=============================================================================
v = 1:5;
mask = [true false true false true];
asserts.isequal(v(mask), [1 3 5]);
v(mask) = [10 30 50];
asserts.isequal(v, [10 2 30 4 50]);
v(2:end - 1) = [];
asserts.isequal(v, [10 50]);
%=============================================================================
txt = 'abcd';
asserts.isequal(txt(2:3), 'bc');
txt(2:3) = 'XY';
asserts.isequal(txt, 'aXYd');
txt(2) = [];
asserts.isequal(txt, 'aYd');
%=============================================================================
str = string({'red', 'green', 'blue'});
asserts.isequal(str(2), string('green'));
str(2) = string('lime');
asserts.isequal(str, string({'red', 'lime', 'blue'}));
%=============================================================================
C = {1, [2 3 4], 'four'};
asserts.isequal(C(2:3), {[2 3 4], 'four'});
asserts.isequal(C{2}(end), 4);
C{2}(2) = 30;
asserts.isequal(C{2}, [2 30 4]);
C{2}(end) = [];
asserts.isequal(C{2}, [2 30]);
C(1) = [];
asserts.isequal(C, {[2 30], 'four'});
%=============================================================================
D = {0, 0};
[D{1:2}] = deal(10, 20);
asserts.isequal(D, {10, 20});
%=============================================================================
S(1).name = 'alpha';
S(2).name = 'beta';
asserts.isequal({S.name}, {'alpha', 'beta'});
%=============================================================================
field = 'name';
asserts.isequal({S.(field)}, {'alpha', 'beta'});
S(2).(field) = 'gamma';
asserts.isequal({S.name}, {'alpha', 'gamma'});
%=============================================================================
T.data.values = [1 2 3];
field = 'values';
asserts.isequal(T.data.(field)(2:end), [2 3]);
T.data.(field)(2) = 20;
asserts.isequal(T.data.values, [1 20 3]);
T.data.(field)(end) = [];
asserts.isequal(T.data.values, [1 20]);
%=============================================================================
U(1).payload = {1, [2 3 4]};
asserts.isequal(U(1).payload{2}(2), 3);
U(1).payload{2}(2) = 30;
asserts.isequal(U(1).payload{2}, [2 30 4]);
%=============================================================================

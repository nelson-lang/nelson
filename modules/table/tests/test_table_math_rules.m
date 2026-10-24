%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T1 = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
T2 = table([30; 40], [10; 20], 'VariableNames', {'B', 'A'});
R = T1 + T2;
asserts.isequal(R.A, [11; 22]);
asserts.isequal(R.B, [33; 44]);
%=============================================================================
T1 = table([1; 2], 'VariableNames', {'A'}, 'RowNames', {'r1', 'r2'});
T2 = table([20; 10], 'VariableNames', {'A'}, 'RowNames', {'r2', 'r1'});
R = T1 + T2;
asserts.isequal(R.A, [11; 22]);
asserts.isequal(R.Properties.RowNames, {'r1'; 'r2'});
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
R = T .* [10 100];
asserts.isequal(R.A, [10; 20]);
asserts.isequal(R.B, [300; 400]);
R = T .* [10; 100];
asserts.isequal(R.A, [10; 200]);
asserts.isequal(R.B, [30; 400]);
R = T .* [10 100; 20 200];
asserts.isequal(R.A, [10; 40]);
asserts.isequal(R.B, [300; 800]);
asserts.checkerror('T + ones(2, 2, 2)', _('Multidimensional array operands are not supported.'));
%=============================================================================
R = sum(T, 2);
asserts.isequal(R.sum, [4; 6]);
R = mean(T);
asserts.isequal(R.A, 1.5);
asserts.isequal(R.B, 3.5);
%=============================================================================
T.Properties.VariableUnits = {'m', 'm'};
R = T + T;
asserts.isequal(R.Properties.VariableUnits, {'m', 'm'});
R = T .* T;
asserts.isequal(R.Properties.VariableUnits, {});
R = T .^ 2;
asserts.isequal(R.Properties.VariableUnits, {});
asserts.checkerror('U = T; U.Properties.VariableUnits = {''s'', ''m''}; T + U', _('Table variable units must be compatible.'));
%=============================================================================
TT1 = timetable(seconds([1; 2]), [1; 2], [3; 4], 'VariableNames', {'A', 'B'});
TT2 = timetable(seconds([2; 1]), [40; 30], [20; 10], 'VariableNames', {'B', 'A'});
R = TT1 + TT2;
asserts.istrue(istimetable(R));
asserts.isequal(R.Properties.RowTimes, seconds([1; 2]));
asserts.isequal(R.A, [11; 22]);
asserts.isequal(R.B, [33; 44]);
S = sum(TT1, 2);
asserts.istrue(istimetable(S));
asserts.isequal(S.sum, [4; 6]);
M = mean(TT1);
asserts.istrue(istable(M));
asserts.isfalse(istimetable(M));
%=============================================================================
E = eventtable(timetable(seconds([1; 2]), [10; 20]));
R = E + 1;
asserts.isequal(class(R), 'eventtable');
asserts.isequal(R.Var1, [11; 21]);
M = mean(E);
asserts.isequal(class(M), 'table');
%=============================================================================

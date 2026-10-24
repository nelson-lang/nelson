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
% an assignment past the last row grows the table: the variables not
% assigned get their default value
T = table([1; 2], {'a'; 'b'}, ["x"; "y"], categorical({'c'; 'd'}), ...
  datetime(2020, 1, [1; 2]), seconds([1; 2]), [true; false], int8([1; 2]), ...
  'VariableNames', {'D', 'C', 'S', 'K', 'DT', 'DU', 'L', 'I'});
lastwarn('');
R = T;
R{5, 'D'} = 9;
[~, id] = lastwarn();
assert_isequal(id, 'Nelson:table:rowsAddedExistingVars');
assert_isequal(size(R), [5 8]);
assert_isequal(R.D, [1; 2; 0; 0; 9]);
assert_isequal(R.C(3:5), {[]; []; []});
assert_istrue(all(ismissing(R.S(3:5))));
assert_istrue(all(isundefined(R.K(3:5))));
assert_istrue(all(isnat(R.DT(3:5))));
assert_isequal(R.DU(3:5), seconds([0; 0; 0]));
assert_isequal(R.L, [true; false; false; false; false]);
assert_isequal(R.I, int8([1; 2; 0; 0; 0]));
%=============================================================================
% braces and parentheses
warning('off', 'Nelson:table:rowsAddedExistingVars');
R = T;
R{5, 1} = 9;
assert_isequal(R.D, [1; 2; 0; 0; 9]);
R = T;
R(5, 1) = {9};
assert_isequal(R.D, [1; 2; 0; 0; 9]);
R = T;
R{5, 'New'} = 9;
assert_isequal(R.New, [0; 0; 0; 0; 9]);
assert_isequal(R.I, int8([1; 2; 0; 0; 0]));
T2 = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
%=============================================================================
% every variable assigned: no warning
warning('on', 'Nelson:table:rowsAddedExistingVars');
lastwarn('');
R = T2;
R{3:4, :} = [5 6; 7 8];
R(6, :) = table(9, 10, 'VariableNames', {'A', 'B'});
assert_isequal(lastwarn(), '');
assert_isequal(R.A, [1; 2; 5; 7; 0; 9]);
assert_isequal(R.B, [3; 4; 6; 8; 0; 10]);
%=============================================================================
% a table without rows
T0 = table(zeros(0, 1), seconds(zeros(0, 1)), strings(0, 1), cell(0, 1), zeros(0, 2), ...
  'VariableNames', {'A', 'DU', 'S', 'C', 'W'});
warning('off', 'Nelson:table:rowsAddedExistingVars');
R = T0;
R{2, 1} = 3;
assert_isequal(R.A, [0; 3]);
assert_isequal(R.DU, seconds([0; 0]));
assert_istrue(all(ismissing(R.S)));
assert_isequal(R.C, {[]; []});
assert_isequal(R.W, zeros(2, 2));
%=============================================================================
% row names: Row<k> for the rows added by number, the given names otherwise
TR = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, 'RowNames', {'r1', 'r2'});
R = TR;
R{4, 1} = 9;
assert_isequal(R.Properties.RowNames, {'r1'; 'r2'; 'Row3'; 'Row4'});
assert_isequal(R.A, [1; 2; 0; 9]);
R = TR;
R{'r9', 1} = 9;
assert_isequal(R.Properties.RowNames, {'r1'; 'r2'; 'r9'});
assert_isequal(R.B, [3; 4; 0]);
R = TR;
R{{'r9', 'r1', 'r8'}, 'A'} = [7; 8; 9];
assert_isequal(R.Properties.RowNames, {'r1'; 'r2'; 'r9'; 'r8'});
assert_isequal(R.A, [8; 2; 7; 9]);
R = TR;
R('r9', :) = {9, 8};
assert_isequal(R{'r9', :}, [9 8]);
R = table([1; 2], 'RowNames', {'Row3', 'b'});
R{3, 1} = 5;
assert_isequal(R.Properties.RowNames, {'Row3'; 'b'; 'Row3_1'});
R = T2;
R{'r9', 1} = 9;
assert_isequal(R.Properties.RowNames, {'Row1'; 'Row2'; 'r9'});
warning('on', 'Nelson:table:rowsAddedExistingVars');
%=============================================================================
% deleting an unknown row name is still an error
assert_checkerror('R = TR; R(''zz'', :) = [];', 'Unrecognized table row name ''zz''.', 'Nelson:table:notFound');
%=============================================================================
% timetables: missing row times, or the times given
warning('off', 'Nelson:table:rowsAddedExistingVars');
TT = timetable(seconds([1; 2]), [1; 2], [3; 4], 'VariableNames', {'A', 'B'});
R = TT;
R{4, 1} = 9;
assert_isequal(R.A, [1; 2; 0; 9]);
assert_isequal(R.B, [3; 4; 0; 0]);
assert_isequal(R.Properties.RowTimes(1:2), seconds([1; 2]));
assert_istrue(all(isnan(R.Properties.RowTimes(3:4))));
R = TT;
R(4, :) = {9, 8};
assert_isequal(R.B, [3; 4; 0; 8]);
R = TT;
R{seconds(9), 1} = 9;
assert_isequal(R.Properties.RowTimes, seconds([1; 2; 9]));
assert_isequal(R.A, [1; 2; 9]);
TD = timetable(datetime(2020, 1, [1; 2]), [1; 2], 'VariableNames', {'A'});
R = TD;
R{4, 1} = 9;
assert_isequal(R.A, [1; 2; 0; 9]);
assert_istrue(all(isnat(R.Properties.RowTimes(3:4))));
warning('on', 'Nelson:table:rowsAddedExistingVars');
%=============================================================================

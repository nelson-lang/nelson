%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% unique on timetables: row times are the first key, then the variables
%=============================================================================
TT = timetable(seconds([3; 1; 3; 2; 1]), [5; 7; 5; 9; 7], {'b'; 'a'; 'b'; 'c'; 'a'}, 'VariableNames', {'X', 'S'});
[C, ia, ic] = unique(TT);
assert_istrue(istimetable(C));
assert_isequal(C.Properties.RowTimes, seconds([1; 2; 3]));
assert_isequal(C.X, [7; 9; 5]);
assert_isequal(C.S, {'a'; 'c'; 'b'});
assert_isequal(ia, [2; 4; 1]);
assert_isequal(ic, [3; 1; 3; 2; 1]);
assert_isequal(C, TT(ia, :));
assert_isequal(TT, C(ic, :));
%=============================================================================
[C, ia, ic] = unique(TT, 'stable');
assert_isequal(C.Properties.RowTimes, seconds([3; 1; 2]));
assert_isequal(ia, [1; 2; 4]);
assert_isequal(ic, [1; 2; 1; 3; 2]);
%=============================================================================
[C, ia, ic] = unique(TT, 'last');
assert_isequal(C.Properties.RowTimes, seconds([1; 2; 3]));
assert_isequal(ia, [5; 4; 3]);
assert_isequal(ic, [3; 1; 3; 2; 1]);
%=============================================================================
[C, ia, ic] = unique(TT, 'stable', 'last');
assert_isequal(C.Properties.RowTimes, seconds([3; 2; 1]));
assert_isequal(ia, [3; 4; 5]);
assert_isequal(ic, [1; 3; 1; 2; 3]);
%=============================================================================
% equal row times: the variables break the tie
TT = timetable(seconds([2; 1; 2; 1]), [9; 8; 7; 8]);
[C, ia, ic] = unique(TT);
assert_isequal(C.Properties.RowTimes, seconds([1; 2; 2]));
assert_isequal(C.Var1, [8; 7; 9]);
assert_isequal(ia, [2; 3; 1]);
assert_isequal(ic, [3; 1; 2; 1]);
%=============================================================================
% equal data at different row times: rows stay distinct
TT = timetable(seconds([2; 1; 3]), [5; 5; 5]);
[C, ia, ic] = unique(TT);
assert_isequal(height(C), 3);
assert_isequal(ia, [2; 1; 3]);
assert_isequal(ic, [2; 1; 3]);
%=============================================================================
% NaT row times sort last and stay distinct
TT = timetable(datetime(2020, 1, [2; 1; 2; NaN; NaN]), [1; 1; 1; 1; 1]);
[C, ia, ic] = unique(TT);
assert_isequal(ia, [2; 1; 4; 5]);
assert_isequal(ic, [2; 1; 2; 3; 4]);
[C, ia, ic] = unique(TT, 'TreatMissingAsDistinct', false);
assert_isequal(ia, [2; 1; 4]);
assert_isequal(ic, [2; 1; 2; 3; 3]);
%=============================================================================
% NaN in a variable at equal row times
TT = timetable(seconds([1; 1; 1; 1]), [NaN; 2; NaN; 2]);
[C, ia, ic] = unique(TT);
assert_isequal(ia, [2; 1; 3]);
assert_isequal(ic, [2; 1; 3; 1]);
%=============================================================================
% a variable named 'Time' (row times renamed) does not collide with the time key
TT = timetable(seconds([2; 1; 2]), [4; 4; 4]);
TT.Properties.DimensionNames{1} = 'Date';
TT.Properties.VariableNames = {'Time'};
[C, ia] = unique(TT);
assert_isequal(ia, [2; 1]);
assert_isequal(C.Properties.DimensionNames, {'Date', 'Variables'});
assert_isequal(C.Properties.RowTimes, seconds([1; 2]));
%=============================================================================
TT = timetable(seconds(zeros(0, 1)), zeros(0, 1));
[C, ia, ic] = unique(TT);
assert_istrue(istimetable(C));
assert_isequal(size(C), [0 1]);
assert_isequal(size(ia), [0 1]);
assert_isequal(size(ic), [0 1]);
%=============================================================================

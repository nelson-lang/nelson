%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a row time subscript selects all the rows at that time, none when it is
% not a row time
TT = timetable(seconds([1; 2; 2; 3]), [1; 2; 3; 4], 'VariableNames', {'A'});
R = TT(seconds(2), :);
assert_isequal(R.A, [2; 3]);
assert_isequal(R.Properties.RowTimes, seconds([2; 2]));
R = TT(seconds([3 2 9]), :);
assert_isequal(R.A, [4; 2; 3]);
R = TT(seconds(2.5), :);
assert_isequal(size(R), [0 1]);
R = TT(seconds(2.5), 'A');
assert_isequal(size(R), [0 1]);
assert_isequal(TT{seconds(2), 'A'}, [2; 3]);
assert_isequal(size(TT{seconds(2.5), 'A'}), [0 1]);
%=============================================================================
TD = timetable(datetime(2020, 1, [1; 2; 2]), [1; 2; 3]);
R = TD(datetime(2020, 1, 2), :);
assert_isequal(R.Var1, [2; 3]);
R = TD(datetime(2020, 1, 5), :);
assert_isequal(size(R), [0 1]);
%=============================================================================
% deleting by row time removes all the rows at that time
R = TT;
R(seconds(2), :) = [];
assert_isequal(R.A, [1; 4]);
%=============================================================================
% assignment still adds a row for a new time
R = TT;
R(seconds(5), :) = {5};
assert_isequal(R.A, [1; 2; 3; 4; 5]);
assert_isequal(R.Properties.RowTimes, seconds([1; 2; 2; 3; 5]));
%=============================================================================

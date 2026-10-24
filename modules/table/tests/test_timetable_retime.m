%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([1; 2; 3]), [10; 20; 30], 'VariableNames', {'A'});
R = retime(TT, seconds([1; 1.5; 2]), 'linear');
asserts.isequal(R.Properties.RowTimes, seconds([1; 1.5; 2]));
asserts.isequal(R.A, [10; 15; 20]);
R = retime(TT, seconds([1; 1.5; 4]), 'previous');
asserts.isequal(R.A, [10; 10; 30]);
%=============================================================================
% named time step with aggregation
t = datetime(2023, 1, 1) + hours([0 12 24 36 48])';
TT = timetable(t, [10; 20; 30; 40; 50], 'VariableNames', {'v'});
R = retime(TT, 'daily', 'mean');
assert_isequal(height(R), 3);
assert_isequal(R.v', [15 35 50]);
R = retime(TT, 'daily', 'sum');
assert_isequal(R.v', [30 70 50]);
%=============================================================================
TT = timetable(seconds([1; 3]), [10; 30], 'VariableNames', {'A'});
R = retime(TT, 'regular', 'linear', 'TimeStep', seconds(1));
asserts.isequal(R.Properties.RowTimes, seconds([1; 2; 3]));
asserts.isequal(R.A, [10; 20; 30]);
%=============================================================================
R = retime(TT, seconds([1; 2; 3]), 'makima');
asserts.isequal(R.A, [10; 20; 30]);
%=============================================================================
R = retime(TT, seconds([1; 2; 4]), 'fillwithconstant', 'Constant', -1);
asserts.isequal(R.A, [10; -1; -1]);
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
TT = timetable(seconds([3; 1; 2]), [30; 10; 20], 'VariableNames', {'A'});
[S, I] = sortrows(TT);
asserts.isequal(S.Properties.RowTimes, seconds([1; 2; 3]));
asserts.isequal(S.A, [10; 20; 30]);
asserts.isequal(I, [2; 3; 1]);
[S, I] = sortrows(TT, 'Time', 'descend');
asserts.isequal(S.Properties.RowTimes, seconds([3; 2; 1]));
asserts.isequal(I, [1; 3; 2]);
%=============================================================================
TT = timetable(datetime(2026, 1, [3; 1; 2]), {'north'; 'south'; 'west'}, ...
  datetime(2026, 2, [3; 1; 2]), 'VariableNames', {'Region', 'RestorationTime'});
R = TT(1:3, :);
asserts.isequal(R.Region, TT.Region);
asserts.isequal(R.RestorationTime, TT.RestorationTime);
[S, I] = sortrows(TT);
asserts.isequal(S.Properties.RowTimes, datetime(2026, 1, [1; 2; 3]));
asserts.isequal(S.Region, {'south'; 'west'; 'north'});
asserts.isequal(I, [2; 3; 1]);
%=============================================================================

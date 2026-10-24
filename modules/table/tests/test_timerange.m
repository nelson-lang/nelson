%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargout('timerange'), 1);
%=============================================================================
TT = timetable(seconds((1:5)'), (10:10:50)', 'VariableNames', {'A'});
R = TT(timerange(seconds(2), seconds(4)), :);
asserts.isequal(R.A, [20; 30]);
R = TT(timerange(seconds(2), seconds(4), 'closed'), :);
asserts.isequal(R.A, [20; 30; 40]);
R = TT(timerange('-inf', seconds(3), 'closed'), :);
asserts.isequal(R.A, [10; 20; 30]);
%=============================================================================
t = datetime(2018, 1:6, 15)';
TT = timetable(t, (1:6)', 'VariableNames', {'Price'});
R = TT(timerange('2018-02-01', 'months'), :);
asserts.isequal(R.Price, 2);
R = TT(timerange('2018-01-01', '2018-05-01', 'quarters'), :);
asserts.isequal(R.Price, (1:6)');
%=============================================================================

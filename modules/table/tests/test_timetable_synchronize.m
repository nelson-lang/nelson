%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
Left = timetable(seconds([1; 3]), [10; 30], 'VariableNames', {'A'});
Right = timetable(seconds([2; 3]), [200; 300], 'VariableNames', {'B'});
U = synchronize(Left, Right);
asserts.isequal(U.Properties.RowTimes, seconds([1; 2; 3]));
asserts.isequal(U.A([1 3]), [10; 30]);
asserts.istrue(isnan(U.A(2)));
asserts.istrue(isnan(U.B(1)));
asserts.isequal(U.B([2 3]), [200; 300]);
I = synchronize(Left, Right, 'intersection');
asserts.isequal(I.Properties.RowTimes, seconds(3));
asserts.isequal(I.A, 30);
asserts.isequal(I.B, 300);
%=============================================================================
U = synchronize(Left, Right, 'union', 'linear');
asserts.isequal(U.Properties.RowTimes, seconds([1; 2; 3]));
asserts.isequal(U.A, [10; 20; 30]);
asserts.isequal(U.B, [100; 200; 300]);
%=============================================================================
R = synchronize(Left, Right, 'regular', 'linear', 'TimeStep', seconds(1));
asserts.isequal(R.Properties.RowTimes, seconds([1; 2; 3]));
asserts.isequal(R.A, [10; 20; 30]);
asserts.isequal(R.B, [100; 200; 300]);
%=============================================================================

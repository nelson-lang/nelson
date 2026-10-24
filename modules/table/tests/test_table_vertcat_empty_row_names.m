%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% vertical concatenation keeps the row names of a table without rows: the
% result has row names when one of the tables has row names
TR = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, 'RowNames', {'r1', 'r2'});
T0 = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
%=============================================================================
k2 = [TR([], :); T0([], :)];
assert_isequal(size(k2), [0 2]);
assert_isequal(size(k2.Properties.RowNames), [0 1]);
R = evalc('disp(k2)');
assert_istrue(startsWith(R, '        A    B'));
%=============================================================================
k2 = [T0([], :); TR([], :)];
assert_isequal(size(k2.Properties.RowNames), [0 1]);
k2 = [TR([], :); TR([], :)];
assert_isequal(size(k2.Properties.RowNames), [0 1]);
k2 = vertcat(TR([], :), T0([], :), T0([], :));
assert_isequal(size(k2.Properties.RowNames), [0 1]);
%=============================================================================
k2 = [T0([], :); T0([], :)];
assert_isequal(size(k2.Properties.RowNames), [0 0]);
R = evalc('disp(k2)');
assert_istrue(startsWith(R, '    A    B'));
%=============================================================================
% the rows of a table without row names are named after their row number
k2 = [TR([], :); T0];
assert_isequal(k2.Properties.RowNames, {'Row1'; 'Row2'});
assert_isequal(k2.A, [1; 2]);
k2 = [T0; TR([], :)];
assert_isequal(k2.Properties.RowNames, {'Row1'; 'Row2'});
k2 = [T0([], :); TR];
assert_isequal(k2.Properties.RowNames, {'r1'; 'r2'});
k2 = [TR; T0([], :)];
assert_isequal(k2.Properties.RowNames, {'r1'; 'r2'});
k2 = [TR; T0];
assert_isequal(k2.Properties.RowNames, {'r1'; 'r2'; 'Row3'; 'Row4'});
k2 = [T0; TR];
assert_isequal(k2.Properties.RowNames, {'Row1'; 'Row2'; 'r1'; 'r2'});
k2 = vertcat(TR([], :), T0, T0);
assert_isequal(k2.Properties.RowNames, {'Row1'; 'Row2'; 'Row3'; 'Row4'});
%=============================================================================
% without row names, nothing changes
k2 = [T0; T0];
assert_isequal(k2.Properties.RowNames, {});
assert_isequal(k2.A, [1; 2; 1; 2]);
%=============================================================================
% rows added by an assignment are still named Row<k>
T = TR;
T(4, :) = {5, 6};
assert_isequal(T.Properties.RowNames, {'r1'; 'r2'; 'Row3'; 'Row4'});
%=============================================================================

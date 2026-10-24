%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, ...
  'RowNames', {'r1', 'r2'}, 'DimensionNames', {'Rows', 'Vars'}, ...
  'Description', 'classdef table', 'UserData', 42);
asserts.isequal(T.Properties.VariableNames, {'A', 'B'});
asserts.isequal(T.Properties.RowNames, {'r1'; 'r2'});
asserts.isequal(T.Properties.DimensionNames, {'Rows', 'Vars'});
asserts.isequal(T.Properties.Description, 'classdef table');
asserts.isequal(T.Properties.UserData, 42);
asserts.isequal(T.Properties.VariableTypes, ["double", "double"]);
%=============================================================================
% RowNames is stored as a column (N x 1) whatever the input orientation.
T = table([1; 2; 3], 'RowNames', {'a', 'b', 'c'});
asserts.isequal(size(T.Properties.RowNames), [3 1]);
T = table([1; 2; 3], 'RowNames', {'a'; 'b'; 'c'});
asserts.isequal(size(T.Properties.RowNames), [3 1]);
T = table([1; 2; 3], 'RowNames', ["a"; "b"; "c"]);
asserts.isequal(size(T.Properties.RowNames), [3 1]);
asserts.isequal(T.Properties.RowNames, {'a'; 'b'; 'c'});
% Row-name-based indexing still works.
T = table([10; 20; 30], 'VariableNames', {'x'}, 'RowNames', {'r1', 'r2', 'r3'});
asserts.isequal(T{'r2', 'x'}, 20);
% Empty RowNames is an empty (0 x 0) cell.
T = table([1; 2; 3]);
asserts.isequal(size(T.Properties.RowNames), [0 0]);
asserts.istrue(iscell(T.Properties.RowNames));
%=============================================================================

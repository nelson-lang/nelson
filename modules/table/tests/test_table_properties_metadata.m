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
assert_isequal(T.Properties.VariableNames, {'A', 'B'});
assert_isequal(T.Properties.RowNames, {'r1', 'r2'});
assert_isequal(T.Properties.DimensionNames, {'Rows', 'Vars'});
assert_isequal(T.Properties.Description, 'classdef table');
assert_isequal(T.Properties.UserData, 42);
assert_isequal(T.Properties.VariableTypes, ["double", "double"]);
%=============================================================================

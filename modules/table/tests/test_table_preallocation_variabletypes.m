%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table('Size', [2 2], 'VariableTypes', {'double', 'string'}, ...
  'VariableNames', {'X', 'Y'});
assert_isequal(size(T), [2 2]);
assert_isequal(T.Properties.VariableNames, {'X', 'Y'});
assert_isequal(T.Properties.VariableTypes, ["double", "string"]);
T.X = [7; 8];
T.Y = ["a"; "b"];
assert_isequal(T.X, [7; 8]);
assert_isequal(T.Y, ["a"; "b"]);
%=============================================================================

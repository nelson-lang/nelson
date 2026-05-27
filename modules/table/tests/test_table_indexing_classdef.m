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
  'RowNames', {'r1', 'r2'});
assert_isequal(T.A, [1; 2]);
assert_isequal(T{:, 'B'}, [3; 4]);
assert_isequal(T{'r2', 'A'}, 2);
S = T({'r2', 'r1'}, {'B', 'A'});
assert_isequal(class(S), 'table');
assert_isequal(S.Properties.VariableNames, {'B', 'A'});
assert_isequal(S.B, [4; 3]);
%=============================================================================

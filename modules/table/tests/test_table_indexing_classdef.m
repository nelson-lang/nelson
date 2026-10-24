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
asserts.isequal(T.A, [1; 2]);
asserts.isequal(T{:, 'B'}, [3; 4]);
asserts.isequal(T{'r2', 'A'}, 2);
S = T({'r2', 'r1'}, {'B', 'A'});
asserts.isequal(class(S), 'table');
asserts.isequal(S.Properties.VariableNames, {'B', 'A'});
asserts.isequal(S.B, [4; 3]);
%=============================================================================

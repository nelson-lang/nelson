%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
S = table({'a'; 'a'; 'b'; 'b'}, {'X'; 'Y'; 'X'; 'Y'}, [1; 3; 2; 4], ...
  'VariableNames', {'ID', 'Measure', 'Value'});
U = unstack(S, 'Value', 'Measure');
asserts.isequal(U.Properties.VariableNames, {'ID', 'X', 'Y'});
asserts.isequal(U.X, [1; 2]);
asserts.isequal(U.Y, [3; 4]);
%=============================================================================

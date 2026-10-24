%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
T = table([1; 2], [3; 4], [5 6; 7 8], 'VariableNames', {'A', 'B', 'Pair'});
%=============================================================================
M = mergevars(T, {'A', 'B'}, 'NewVariableName', 'AB');
asserts.isequal(M.Properties.VariableNames, {'AB', 'Pair'});
asserts.isequal(M.AB, [1 3; 2 4]);
%=============================================================================
S = splitvars(T, 'Pair', 'NewVariableNames', {'P1', 'P2'});
asserts.isequal(S.Properties.VariableNames, {'A', 'B', 'P1', 'P2'});
asserts.isequal(S.P1, [5; 7]);
asserts.isequal(S.P2, [6; 8]);
%=============================================================================
C = convertvars(T, {'A', 'B'}, @(x) single(x));
asserts.isequal(class(C.A), 'single');
asserts.isequal(class(C.B), 'single');
%=============================================================================
R = rows2vars(table({'r1'; 'r2'}, [10; 20], 'VariableNames', {'Name', 'Value'}), 'VariableNamesSource', 'Name');
asserts.isequal(R.Properties.VariableNames, {'OriginalVariableNames', 'r1', 'r2'});
%=============================================================================

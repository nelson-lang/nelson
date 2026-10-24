%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Table variable names may contain spaces and other non-identifier
% characters. The linear one-pass rebuilds must preserve them.
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'Last Name', 'Blood Pressure'});
asserts.isequal(T.Properties.VariableNames, {'Last Name', 'Blood Pressure'});
asserts.isequal(T.('Last Name'), [1; 2]);
%=============================================================================
R = convertvars(T, {'Last Name'}, 'double');
asserts.isequal(R.('Last Name'), [1; 2]);
%=============================================================================
R = renamevars(T, 'Last Name', 'Family Name');
asserts.isequal(R.Properties.VariableNames, {'Family Name', 'Blood Pressure'});
%=============================================================================
R = movevars(T, 'Blood Pressure', 'Before', 'Last Name');
asserts.isequal(R.Properties.VariableNames, {'Blood Pressure', 'Last Name'});
%=============================================================================
R = addvars(T, [5; 6], 'NewVariableNames', {'New Column'});
asserts.isequal(R.Properties.VariableNames, {'Last Name', 'Blood Pressure', 'New Column'});
asserts.isequal(R.('New Column'), [5; 6]);
%=============================================================================
R = removevars(T, 'Blood Pressure');
asserts.isequal(R.Properties.VariableNames, {'Last Name'});
%=============================================================================
U = table([-1; 2], 'VariableNames', {'My Var'});
R = abs(U);
asserts.isequal(R.('My Var'), [1; 2]);
R = U + 1;
asserts.isequal(R.('My Var'), [0; 3]);
%=============================================================================
A = table([1; 2], 'VariableNames', {'A B'});
B = table([3; 4], 'VariableNames', {'C D'});
R = [A, B];
asserts.isequal(R.Properties.VariableNames, {'A B', 'C D'});
%=============================================================================
s = table2struct(T, 'ToScalar', true);
asserts.isequal(s.('Last Name'), [1; 2]);
%=============================================================================
TM = table([1; 2], 'VariableNames', {'base'});
TM.M = [10 20; 30 40];
R = splitvars(TM, 'M');
asserts.isequal(R.Properties.VariableNames, {'base', 'M_1', 'M_2'});
%=============================================================================

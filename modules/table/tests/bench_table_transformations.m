%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
n = 1000;
G = repmat({'a'; 'b'; 'c'; 'd'}, 250, 1);
A = (1:n)';
B = sin(A / 100);
C = A .* B;
tic();
T = table(G, A, B, 'VariableNames', {'G', 'A', 'B'});
T = addvars(T, C, 'NewVariableNames', {'C'});
R = groupcounts(T, 'G');
H = head(T, 100);
S = table2struct(H);
D = table2cell(H);
toc();
asserts.isequal(height(T), n);
asserts.isequal(height(R), 4);
asserts.isequal(length(S), 100);
asserts.isequal(size(D, 1), 100);
%=============================================================================

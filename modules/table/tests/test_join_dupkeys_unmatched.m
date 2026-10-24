%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Join types with duplicate keys on both sides and unmatched rows on both
% sides. Row order follows the Nelson convention: left rows ascending, each
% with its right matches ascending, then right-only rows appended ascending.
%=============================================================================
L = table([1; 2; 2; 3; 5], (10:10:50)', 'VariableNames', {'k', 'a'});
R = table([2; 2; 3; 4], [201; 202; 300; 400], 'VariableNames', {'k', 'b'});
%=============================================================================
J = innerjoin(L, R, 'Keys', 'k');
asserts.isequal(J.Properties.VariableNames, {'k', 'a', 'b'});
asserts.isequal(J.k, [2; 2; 2; 2; 3]);
asserts.isequal(J.a, [20; 20; 30; 30; 40]);
asserts.isequal(J.b, [201; 202; 201; 202; 300]);
%=============================================================================
J = outerjoin(L, R, 'Keys', 'k');
asserts.isequal(J.Properties.VariableNames, {'k_L', 'a', 'k_R', 'b'});
asserts.isequal(J.k_L, [1; 2; 2; 2; 2; 3; 5; NaN]);
asserts.isequal(J.a, [10; 20; 20; 30; 30; 40; 50; NaN]);
asserts.isequal(J.k_R, [NaN; 2; 2; 2; 2; 3; NaN; 4]);
asserts.isequal(J.b, [NaN; 201; 202; 201; 202; 300; NaN; 400]);
%=============================================================================
J = outerjoin(L, R, 'Keys', 'k', 'Type', 'left');
asserts.isequal(J.k_L, [1; 2; 2; 2; 2; 3; 5]);
asserts.isequal(J.a, [10; 20; 20; 30; 30; 40; 50]);
asserts.isequal(J.k_R, [NaN; 2; 2; 2; 2; 3; NaN]);
asserts.isequal(J.b, [NaN; 201; 202; 201; 202; 300; NaN]);
%=============================================================================
J = outerjoin(L, R, 'Keys', 'k', 'Type', 'right');
asserts.isequal(J.k_L, [2; 2; 2; 2; 3; NaN]);
asserts.isequal(J.a, [20; 20; 30; 30; 40; NaN]);
asserts.isequal(J.k_R, [2; 2; 2; 2; 3; 4]);
asserts.isequal(J.b, [201; 202; 201; 202; 300; 400]);
%=============================================================================
J = outerjoin(L, R, 'Keys', 'k', 'MergeKeys', true);
asserts.isequal(J.Properties.VariableNames, {'k', 'a', 'b'});
asserts.isequal(J.k, [1; 2; 2; 2; 2; 3; 5; 4]);
asserts.isequal(J.a, [10; 20; 20; 30; 30; 40; 50; NaN]);
asserts.isequal(J.b, [NaN; 201; 202; 201; 202; 300; NaN; 400]);
%=============================================================================
% Multiple keys, duplicates and unmatched combinations.
L = table([1; 1; 2; 2], {'x'; 'y'; 'x'; 'z'}, (1:4)', 'VariableNames', {'k1', 'k2', 'a'});
R = table([1; 2; 2; 3], {'x'; 'x'; 'z'; 'x'}, (11:14)', 'VariableNames', {'k1', 'k2', 'b'});
%=============================================================================
J = innerjoin(L, R, 'Keys', {'k1', 'k2'});
asserts.isequal(J.Properties.VariableNames, {'k1', 'k2', 'a', 'b'});
asserts.isequal(J.k1, [1; 2; 2]);
asserts.isequal(J.k2, {'x'; 'x'; 'z'});
asserts.isequal(J.a, [1; 3; 4]);
asserts.isequal(J.b, [11; 12; 13]);
%=============================================================================
J = outerjoin(L, R, 'Keys', {'k1', 'k2'}, 'Type', 'left');
asserts.isequal(J.k1_L, [1; 1; 2; 2]);
asserts.isequal(J.k2_L, {'x'; 'y'; 'x'; 'z'});
asserts.isequal(J.a, [1; 2; 3; 4]);
asserts.isequal(J.k1_R, [1; NaN; 2; 2]);
asserts.isequal(J.b, [11; NaN; 12; 13]);
%=============================================================================
% String key column with a cell data column, unmatched rows on both sides.
L = table(["a"; "b"; "b"; "d"], {'L1'; 'L2'; 'L3'; 'L4'}, 'VariableNames', {'k', 'a'});
R = table(["b"; "c"; "a"], [91; 92; 93], 'VariableNames', {'k', 'b'});
%=============================================================================
J = innerjoin(L, R, 'Keys', 'k');
asserts.isequal(J.k, ["a"; "b"; "b"]);
asserts.isequal(J.a, {'L1'; 'L2'; 'L3'});
asserts.isequal(J.b, [93; 91; 91]);
%=============================================================================
J = outerjoin(L, R, 'Keys', 'k', 'Type', 'left');
asserts.isequal(J.a, {'L1'; 'L2'; 'L3'; 'L4'});
asserts.isequal(J.b, [93; 91; 91; NaN]);
%=============================================================================
% Disjoint keys: inner join yields an empty, correctly typed result.
L = table([1; 2], [10; 20], 'VariableNames', {'k', 'a'});
R = table([3; 4], [30; 40], 'VariableNames', {'k', 'b'});
J = innerjoin(L, R, 'Keys', 'k');
asserts.isequal(J.Properties.VariableNames, {'k', 'a', 'b'});
asserts.isequal(height(J), 0);
%=============================================================================

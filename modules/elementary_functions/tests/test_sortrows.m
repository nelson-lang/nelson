%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('sortrows'), -1);
asserts.isequal(nargout('sortrows'), -1);
%=============================================================================
% B = sortrows(A) : lexicographic ascending
R = sortrows([3 1; 1 2; 1 1]);
REF = [1 1; 1 2; 3 1];
asserts.isequal(R, REF);
%=============================================================================
% B = sortrows(A, column)
R = sortrows([3 1; 1 2], 2);
REF = [3 1; 1 2];
asserts.isequal(R, REF);
%=============================================================================
% B = sortrows(A, -column) : descending on that column
R = sortrows([3 1; 1 2], -1);
REF = [3 1; 1 2];
asserts.isequal(R, REF);
%=============================================================================
% [B, index] = sortrows(...)
[B, idx] = sortrows([3 1; 1 2; 1 1]);
asserts.isequal(B, [1 1; 1 2; 3 1]);
asserts.isequal(idx, [3; 2; 1]);
%=============================================================================
% char matrix
R = sortrows(['ba'; 'ab'; 'aa']);
asserts.isequal(R, ['aa'; 'ab'; 'ba']);
%=============================================================================
% cell array of characters
R = sortrows({'b'; 'a'; 'c'});
asserts.isequal(R, {'a'; 'b'; 'c'});
%=============================================================================
% string array
R = sortrows(["b"; "a"; "c"]);
asserts.isequal(R, ["a"; "b"; "c"]);
%=============================================================================
% NaN placed last for ascending, first for descending column
R = sortrows([3 1; NaN 2; 1 1]);
asserts.isequal(R, [1 1; 3 1; NaN 2]);
R = sortrows([3 1; NaN 2; 1 1], -1);
asserts.isequal(R, [NaN 2; 3 1; 1 1]);
%=============================================================================
% multi-column key vector
R = sortrows([1 3; 1 1; 2 2], [1 2]);
asserts.isequal(R, [1 1; 1 3; 2 2]);
%=============================================================================
% Direction option ('ascend'/'descend'), 3-argument form and per-column directions.
A = [3 1; 1 2; 2 3; 1 1];
asserts.isequal(sortrows(A, 'descend'), [3 1; 2 3; 1 2; 1 1]);
asserts.isequal(sortrows(A, [1 2], 'descend'), [3 1; 2 3; 1 2; 1 1]);
asserts.isequal(sortrows(A, [1 -2]), [1 2; 1 1; 2 3; 3 1]);
asserts.isequal(sortrows(A, [1 2], {'ascend', 'descend'}), [1 2; 1 1; 2 3; 3 1]);
[~, idx] = sortrows(A);
asserts.isequal(idx', [4 2 3 1]);
%=============================================================================

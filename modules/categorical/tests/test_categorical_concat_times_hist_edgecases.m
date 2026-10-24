%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'a', 'b'});
B = [A, {'c', ''}];
asserts.isequal(cellstr(B), {'a', 'b', 'c', '<undefined>'});
asserts.isequal(categories(B), {'a'; 'b'; 'c'});
C = [A; categorical({'b', 'c'})];
asserts.isequal(size(C), [2 2]);
asserts.isequal(cellstr(C), {'a', 'b'; 'b', 'c'});
D = cat(1, categorical({'x'}), {'y'});
asserts.isequal(cellstr(D), {'x'; 'y'});
%=============================================================================
P = categorical({'left'; 'right'; ''}) .* categorical({'one'; 'two'; 'one'});
asserts.isequal(categories(P), {'left one'; 'left two'; 'right one'; 'right two'});
asserts.isequal(cellstr(P), {'left one'; 'right two'; '<undefined>'});
Q = categorical({'left'}) .* categorical({'one', 'two'});
asserts.isequal(cellstr(Q), {'left one', 'left two'});
asserts.checkerror('categorical({''a'', ''b''}) .* categorical({''x'', ''y'', ''z''})', ...
  sprintf(_('%s must have compatible sizes.'), 'Categorical arrays'));
%=============================================================================
H = categorical({'b', 'a', 'b', '', 'c'}, {'a', 'b', 'c'}, {'a', 'b', 'c'});
asserts.isequal(countcats(H), [1 2 1]);
asserts.isequal(histcounts(H), [1 2 1]);
asserts.isequal(categoricalHist(H), [1 2 1]);
counts = categoricalHistogram(H);
asserts.isequal(counts, [1 2 1]);
%=============================================================================

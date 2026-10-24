%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = categorical({'b', '', 'a', 'b'});
[S, I] = sort(A);
asserts.isequal(cellstr(S), {'a', 'b', 'b', '<undefined>'});
asserts.isequal(I(:), [3; 1; 4; 2]);
asserts.isfalse(issorted(A));
asserts.istrue(issorted(S));
%=============================================================================
M = categorical({'b', 'a'; '', 'b'});
[SR, IR] = sortrows(M);
asserts.isequal(size(SR), [2 2]);
asserts.isequal(IR(:), [1; 2]);
asserts.istrue(issortedrows(SR));
%=============================================================================
O = categorical({'mid', 'low', 'high', '', 'mid'}, {'low', 'mid', 'high'}, ...
  {'low', 'mid', 'high'}, 'Ordinal', true);
[mn, imn] = min(O);
[mx, imx] = max(O);
asserts.isequal(cellstr(mn), {'low'});
asserts.isequal(imn, 2);
asserts.isequal(cellstr(mx), {'high'});
asserts.isequal(imx, 3);
asserts.isequal(cellstr(median(O)), {'mid'});
asserts.isequal(cellstr(mode(O)), {'mid'});
[B, I] = mink(O, 2);
asserts.isequal(cellstr(B), {'low', 'mid'});
asserts.isequal(I(:), [2; 1]);
[B, I] = maxk(O, 2);
asserts.isequal(cellstr(B), {'high', '<undefined>'});
asserts.isequal(I(:), [3; 4]);
[B, I] = topkrows(O(:), 2, 'ascend');
asserts.isequal(cellstr(B), {'low'; 'mid'});
asserts.isequal(I(:), [2; 1]);
%=============================================================================
asserts.checkerror('min(A)', sprintf(_('%s requires ordinal categorical arrays.'), 'Minimum'));
asserts.checkerror('min(A)', sprintf(_('%s requires ordinal categorical arrays.'), 'Minimum'), 'Nelson:categorical:requiresOrdinal');
asserts.checkerror('max(A)', sprintf(_('%s requires ordinal categorical arrays.'), 'Maximum'));
asserts.checkerror('max(A)', sprintf(_('%s requires ordinal categorical arrays.'), 'Maximum'), 'Nelson:categorical:requiresOrdinal');
asserts.checkerror('median(A)', sprintf(_('%s requires ordinal categorical arrays.'), 'Median'));
asserts.checkerror('median(A)', sprintf(_('%s requires ordinal categorical arrays.'), 'Median'), 'Nelson:categorical:requiresOrdinal');
asserts.checkerror('mink(A, 1)', sprintf(_('%s requires ordinal categorical arrays.'), 'mink'));
asserts.checkerror('mink(A, 1)', sprintf(_('%s requires ordinal categorical arrays.'), 'mink'), 'Nelson:categorical:requiresOrdinal');
asserts.checkerror('maxk(A, 1)', sprintf(_('%s requires ordinal categorical arrays.'), 'maxk'));
asserts.checkerror('maxk(A, 1)', sprintf(_('%s requires ordinal categorical arrays.'), 'maxk'), 'Nelson:categorical:requiresOrdinal');
%=============================================================================

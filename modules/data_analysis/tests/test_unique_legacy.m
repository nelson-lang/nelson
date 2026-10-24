%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% 'legacy' reproduces the older semantics of unique:
%  - values are returned sorted ascending,
%  - the index ia points at the LAST occurrence of each repeated value,
%  - for a non-'rows' input the index outputs follow the input orientation.
%=============================================================================
% row vector: index outputs are row vectors, ia is the last occurrence
x = [3 1 2 1 3];
[C, ia, ic] = unique(x, 'legacy');
asserts.isequal(C, [1 2 3]);
asserts.isequal(ia, [4 3 5]);
asserts.isequal(ic, [3 1 2 1 3]);
asserts.istrue(isrow(ia));
asserts.istrue(isrow(ic));
asserts.isequal(C, x(ia));
asserts.isequal(x, C(ic));
%=============================================================================
% single output is still the sorted unique set
asserts.isequal(unique([3 1 2 1 3], 'legacy'), [1 2 3]);
%=============================================================================
% column vector: index outputs are column vectors
y = [3; 1; 2; 1; 3];
[C, ia, ic] = unique(y, 'legacy');
asserts.isequal(C, [1; 2; 3]);
asserts.isequal(ia, [4; 3; 5]);
asserts.isequal(ic, [3; 1; 2; 1; 3]);
asserts.istrue(iscolumn(ia));
asserts.istrue(iscolumn(ic));
%=============================================================================
% matrix without 'rows' is treated as a column: index outputs are columns
M = [3 1; 2 1; 3 2];
[C, ia, ic] = unique(M, 'legacy');
asserts.istrue(iscolumn(C));
asserts.istrue(iscolumn(ia));
asserts.istrue(iscolumn(ic));
asserts.isequal(M(:), C(ic));
%=============================================================================
% 'rows' + 'legacy': index outputs stay column vectors, ia is last occurrence.
% This is the combination used by iFEM's tool/myunique.m.
A = [1 2; 3 4; 1 2; 3 4; 5 6];
[C, ia, ic] = unique(A, 'rows', 'legacy');
asserts.isequal(C, [1 2; 3 4; 5 6]);
asserts.isequal(ia, [3; 4; 5]);
asserts.isequal(ic, [1; 2; 1; 2; 3]);
asserts.istrue(iscolumn(ia));
asserts.istrue(iscolumn(ic));
asserts.isequal(C, A(ia, :));
asserts.isequal(A, C(ic, :));
%=============================================================================
% 'legacy' differs from the default 'first' behavior for ia
A = [9 2 9 5];
[~, ia_first] = unique(A);
[~, ia_legacy] = unique(A, 'legacy');
asserts.isequal(ia_first, [2; 4; 1]);
asserts.isequal(ia_legacy, [2 4 3]);
%=============================================================================
% 'legacy' cannot be combined with the modern selection flags
assert_checkerror('unique([1 2 1], ''legacy'', ''stable'')', ...
  'The ''legacy'' flag cannot be combined with the ''sorted'', ''stable'', ''first'' or ''last'' flags.');
assert_checkerror('unique([1 2 1], ''legacy'', ''first'')', ...
  'The ''legacy'' flag cannot be combined with the ''sorted'', ''stable'', ''first'' or ''last'' flags.');
assert_checkerror('unique([1 2 1], ''legacy'', ''last'')', ...
  'The ''legacy'' flag cannot be combined with the ''sorted'', ''stable'', ''first'' or ''last'' flags.');
assert_checkerror('unique([1 2 1], ''legacy'', ''sorted'')', ...
  'The ''legacy'' flag cannot be combined with the ''sorted'', ''stable'', ''first'' or ''last'' flags.');
%=============================================================================
% unknown option message now mentions 'legacy'
assert_checkerror('unique([1 2 1], ''badoption'')', ...
  'unique option must be ''rows'', ''sorted'', ''stable'', ''first'', ''last'' or ''legacy''');
%=============================================================================

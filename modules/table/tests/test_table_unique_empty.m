%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% unique on a table without rows validates its options like any other table
%=============================================================================
T = table(zeros(0, 1));
assert_checkerror('unique(T, ''stable'', ''sorted'')', ...
  'You can specify either ''sorted'' or ''stable'' but not both.', ...
  'Nelson:data_analysis:uniqueSortedStableCombination');
assert_checkerror('unique(T, ''badoption'')', ...
  'unique option must be ''rows'', ''sorted'', ''stable'', ''first'', ''last'' or ''legacy''', ...
  'Nelson:data_analysis:uniqueOptionRowsSortedStable');
%=============================================================================
% valid options still return an empty table and empty column indices
[C, ia, ic] = unique(T);
assert_isequal(size(C), [0 1]);
assert_isequal(ia, zeros(0, 1));
assert_isequal(ic, zeros(0, 1));
[C, ia, ic] = unique(T, 'stable');
assert_isequal(size(C), [0 1]);
assert_isequal(ia, zeros(0, 1));
assert_isequal(ic, zeros(0, 1));
[C, ia, ic] = unique(T, 'rows', 'last');
assert_isequal(ia, zeros(0, 1));
assert_isequal(ic, zeros(0, 1));
[C, ia, ic] = unique(T, 'TreatMissingAsDistinct', false);
assert_isequal(ia, zeros(0, 1));
assert_isequal(ic, zeros(0, 1));
%=============================================================================

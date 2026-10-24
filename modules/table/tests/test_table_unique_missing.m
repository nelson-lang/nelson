%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% unique on tables: rows holding missing values ('TreatMissingAsDistinct')
%=============================================================================
T = table([1; NaN; NaN; 2], [3; 4; 4; 5]);
[C, ia, ic] = unique(T);
assert_isequal(height(C), 4);
assert_isequal(ia, [1; 4; 2; 3]);
assert_isequal(ic, [1; 3; 4; 2]);
[C, ia, ic] = unique(T, 'TreatMissingAsDistinct', false);
assert_isequal(C.Var1, [1; 2; NaN]);
assert_isequal(C.Var2, [3; 5; 4]);
assert_isequal(ia, [1; 4; 2]);
assert_isequal(ic, [1; 3; 3; 2]);
%=============================================================================
T = table([1; 1; 2; 1], [7; 7; 8; 7]);
[C, ia, ic] = unique(T, 'TreatMissingAsDistinct', true);
assert_isequal(C.Var1, [1; 2]);
assert_isequal(ic, [1; 1; 2; 1]);
%=============================================================================

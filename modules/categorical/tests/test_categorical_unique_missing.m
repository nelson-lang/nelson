%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% unique on categorical: <undefined> values are missing values
%=============================================================================
A = categorical({'b', '', 'a', ''});
[C, ia, ic] = unique(A);
assert_isequal(cellstr(C), {'a'; 'b'; '<undefined>'; '<undefined>'});
assert_isequal(ia, [3; 1; 2; 4]);
assert_isequal(ic, [2; 3; 1; 4]);
[C, ia, ic] = unique(A, 'TreatMissingAsDistinct', false);
assert_isequal(cellstr(C), {'a'; 'b'; '<undefined>'});
assert_isequal(ia, [3; 1; 2]);
assert_isequal(ic, [2; 3; 1; 3]);
[C, ia] = unique(A, 'stable', 'TreatMissingAsDistinct', false);
assert_isequal(cellstr(C), {'b'; '<undefined>'; 'a'});
assert_isequal(ia, [1; 2; 3]);
%=============================================================================

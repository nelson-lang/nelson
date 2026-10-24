%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% unique: missing values handling ('TreatMissingAsDistinct')
%=============================================================================
A = [5 8 NaN NaN];
assert_isequal(unique(A), [5 8 NaN NaN]);
assert_isequal(unique(A, 'TreatMissingAsDistinct', true), [5 8 NaN NaN]);
assert_isequal(unique(A, 'TreatMissingAsDistinct', false), [5 8 NaN]);
assert_isequal(unique(A, "TreatMissingAsDistinct", 0), [5 8 NaN]);
%=============================================================================
[C, ia, ic] = unique([NaN 3 NaN 1 3], 'TreatMissingAsDistinct', false);
assert_isequal(C, [1 3 NaN]);
assert_isequal(ia, [4; 2; 1]);
assert_isequal(ic, [3; 2; 3; 1; 2]);
%=============================================================================
[C, ia, ic] = unique([NaN 3 NaN 1 3], 'last', 'TreatMissingAsDistinct', false);
assert_isequal(C, [1 3 NaN]);
assert_isequal(ia, [4; 5; 3]);
assert_isequal(ic, [3; 2; 3; 1; 2]);
%=============================================================================
[C, ia, ic] = unique([NaN 3 NaN 1 3], 'stable', 'TreatMissingAsDistinct', false);
assert_isequal(C, [NaN 3 1]);
assert_isequal(ia, [1; 2; 4]);
assert_isequal(ic, [1; 2; 1; 3; 2]);
%=============================================================================
[C, ia, ic] = unique([NaN 3 NaN 1 3], 'stable', 'last', 'TreatMissingAsDistinct', false);
assert_isequal(C, [NaN 1 3]);
assert_isequal(ia, [3; 4; 5]);
assert_isequal(ic, [1; 3; 1; 2; 3]);
%=============================================================================
C = unique(single([NaN; 2; NaN]), 'TreatMissingAsDistinct', false);
assert_isequal(C, single([2; NaN]));
[C, ia, ic] = unique([1 + 2i, NaN, 3, complex(NaN, 1)], 'TreatMissingAsDistinct', false);
assert_isequal(numel(C), 3);
assert_isequal(ia, [1; 3; 2]);
assert_isequal(ic, [1; 3; 2; 3]);
assert_isequal(unique(int8([3 1 3]), 'TreatMissingAsDistinct', false), int8([1 3]));
%=============================================================================
% rows: duplicates when the missing values are in the same columns
M = [1 NaN; 1 NaN; 2 3; NaN 1];
[C, ia, ic] = unique(M, 'rows');
assert_isequal(C, M);
assert_isequal(ia, [1; 2; 3; 4]);
assert_isequal(ic, [1; 2; 3; 4]);
[C, ia, ic] = unique(M, 'rows', 'TreatMissingAsDistinct', false);
assert_isequal(C, [1 NaN; 2 3; NaN 1]);
assert_isequal(ia, [1; 3; 4]);
assert_isequal(ic, [1; 1; 2; 3]);
%=============================================================================
% string <missing>
S = ["b", missing, "a", missing];
[C, ia, ic] = unique(S);
assert_isequal(C, ["a", "b", missing, missing]);
assert_isequal(ia, [3; 1; 2; 4]);
assert_isequal(ic, [2; 3; 1; 4]);
[C, ia, ic] = unique(S, 'stable');
assert_isequal(C, ["b", missing, "a", missing]);
assert_isequal(ia, [1; 2; 3; 4]);
assert_isequal(ic, [1; 2; 3; 4]);
[C, ia, ic] = unique(S, 'TreatMissingAsDistinct', false);
assert_isequal(C, ["a", "b", missing]);
assert_isequal(ia, [3; 1; 2]);
assert_isequal(ic, [2; 3; 1; 3]);
assert_isequal(unique(["", missing, ""]), ["", missing]);
%=============================================================================
SR = ["a", missing; "a", missing; "b", "c"];
[C, ia] = unique(SR, 'rows');
assert_isequal(C, SR);
assert_isequal(ia, [1; 2; 3]);
[C, ia, ic] = unique(SR, 'rows', 'TreatMissingAsDistinct', false);
assert_isequal(C, ["a", missing; "b", "c"]);
assert_isequal(ia, [1; 3]);
assert_isequal(ic, [1; 1; 2]);
%=============================================================================
assert_checkerror('unique([1 NaN], ''legacy'', ''TreatMissingAsDistinct'', false)', 'The ''legacy'' flag cannot be combined with ''TreatMissingAsDistinct''.');
assert_checkerror('unique([1 NaN], ''legacy'', ''TreatMissingAsDistinct'', false)', 'The ''legacy'' flag cannot be combined with ''TreatMissingAsDistinct''.', 'Nelson:data_analysis:uniqueLegacyTreatMissing');
assert_checkerror('unique([1 NaN], ''TreatMissingAsDistinct'')', '''TreatMissingAsDistinct'' must be followed by a logical scalar.');
assert_checkerror('unique([1 NaN], ''TreatMissingAsDistinct'')', '''TreatMissingAsDistinct'' must be followed by a logical scalar.', 'Nelson:data_analysis:uniqueTreatMissingAsDistinctValue');
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Expected values are listed explicitly below.
%=============================================================================
% default tolerance for double is 1e-12: a 1e-8 gap is not collapsed
assert_isequal(uniquetol([1 1.00000001 2]), [1 1.00000001 2]);
%=============================================================================
% an explicit tolerance collapses the near-duplicates, keeping the lowest value
[C, ia, ic] = uniquetol([2 1 2 1.0000001], 1e-6);
assert_isequal(C, [1 2]);
assert_isequal(ia, [2; 1]);
assert_isequal(ic, [2; 1; 2; 1]);
%=============================================================================
% C = A(ia) and A is within tolerance of C(ic)
A = [2 1 2 1.0000001];
Aia = A(ia);
assert_isequal(Aia(:)', C);
Cic = C(ic);
assert_istrue(max(abs(A(:) - Cic(:))) <= 1e-6 * max(abs(A(:))));
%=============================================================================
% grouping compares to the last kept representative, not the previous element
[C, ia, ic] = uniquetol([1 1.6 2.2], 0.3);
assert_isequal(C, [1 2.2]);
assert_isequal(ia, [1; 3]);
assert_isequal(ic, [1; 1; 2]);
%=============================================================================
% a monotone chain keeps every third value
assert_isequal(uniquetol([1 1.5 2 2.5 3], 0.3), [1 2 3]);
%=============================================================================
% the tolerance is scaled by the largest absolute value of the data
assert_isequal(uniquetol([1 1.5 2000], 1e-3), [1 2000]);
assert_isequal(uniquetol([1000 1000.5 2000], 1e-3), [1000 2000]);
%=============================================================================
% ByRows collapses near-equal rows
[C, ia, ic] = uniquetol([1 100; 1.0000001 100; 2 200], 1e-6, 'ByRows', true);
assert_isequal(C, [1 100; 2 200]);
assert_isequal(ia, [1; 3]);
assert_isequal(ic, [1; 1; 2]);
%=============================================================================
% ByRows uses a per-column data scale by default
R = uniquetol([1 1e6; 1.5 1e6], 1e-6, 'ByRows', true);
assert_isequal(R, [1 1e6; 1.5 1e6]);
%=============================================================================
% OutputAllIndices returns a cell of every index in each group
[C, ia] = uniquetol([2 1 2 1.0000001], 1e-6, 'OutputAllIndices', true);
assert_isequal(C, [1 2]);
assert_isequal(class(ia), 'cell');
assert_isequal(ia{1}, [2; 4]);
assert_isequal(ia{2}, [1; 3]);
%=============================================================================
% OutputAllIndices combined with ByRows
[C, ia] = uniquetol([1 10; 1.0000001 10; 2 20], 1e-6, 'ByRows', true, 'OutputAllIndices', true);
assert_isequal(C, [1 10; 2 20]);
assert_isequal(ia{1}, [1; 2]);
assert_isequal(ia{2}, 3);
%=============================================================================
% DataScale overrides the automatic scaling
assert_isequal(uniquetol([1 1.5 2], 0.4, 'DataScale', 1), [1 1.5 2]);
assert_isequal(uniquetol([1 1e6; 1.5 1e6], 1e-6, 'ByRows', true, 'DataScale', 1e6), [1 1e6]);
%=============================================================================
% NaN values are each kept as distinct entries at the end
assert_isequal(uniquetol([1 NaN 1.0000001 NaN], 1e-6), [1 NaN NaN]);
%=============================================================================
% empty input gives a 0x1 empty column
assert_isequal(size(uniquetol([])), [0 1]);
%=============================================================================
% a row vector stays a row and a column stays a column
assert_isequal(size(uniquetol([1 1.0000001 2], 1e-6)), [1 2]);
assert_isequal(size(uniquetol([1; 1.0000001; 2], 1e-6)), [2 1]);
%=============================================================================
% the class of the input is preserved and single uses a 1e-6 default
R = uniquetol(single([1 1.0000005 2]));
assert_isequal(class(R), 'single');
assert_isequal(R, single([1 2]));
%=============================================================================
% tol = 0 requires exact matches
assert_isequal(uniquetol([1 1 2 2 3], 0), [1 2 3]);
%=============================================================================
% an integer input is rejected
assert_checkerror('uniquetol(int32([1 2 3]))', _('First argument must be a real full matrix of type single or double.'));
%=============================================================================
% a complex input is rejected
assert_checkerror('uniquetol([1+2i 3])', _('First argument must be a real full matrix of type single or double.'));
%=============================================================================

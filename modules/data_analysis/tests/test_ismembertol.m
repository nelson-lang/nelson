%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% default tolerance is 1e-12 for double (only exact-ish matches)
[lia, locb] = ismembertol([1 2 3], [1.0000001 5 3]);
asserts.isequal(lia, logical([0 0 1]));
asserts.isequal(locb, [0 0 3]);
%=============================================================================
% explicit tolerance widens the match
[lia2, locb2] = ismembertol([1 2 3], [1.0000001 5 3], 1e-6);
asserts.isequal(lia2, logical([1 0 1]));
asserts.isequal(locb2, [1 0 3]);
%=============================================================================
% LocB returns the lowest index among duplicates within tolerance
[lia3, locb3] = ismembertol(2, [2 2 2], 1e-3);
asserts.isequal(lia3, true);
asserts.isequal(locb3, 1);
%=============================================================================
% tolerance is scaled by the data magnitude
asserts.isequal(ismembertol(1000, 1000.0005, 1e-6), true);
asserts.isequal(ismembertol(1, 1.5, 0.6), true);
asserts.isequal(ismembertol(1, 1.5, 0.1), false);
%=============================================================================
% empty and no-match cases
asserts.isequal(ismembertol([1 2 3], []), logical([0 0 0]));
asserts.isequal(ismembertol(5, [1 2 3]), false);
%=============================================================================
% single output form
asserts.isequal(ismembertol([1 3 5], [3 5 7], 1e-9), logical([0 1 1]));
%=============================================================================
asserts.checkerror('ismembertol(1, 2, -1)', _('TOL must be a nonnegative numeric scalar.'));
%=============================================================================

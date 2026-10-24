%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% sizes are real finite integers
assert_checkerror('rand(2.5)', _('A real integer value scalar expected.'));
assert_checkerror('rand(NaN)', _('A real integer value scalar expected.'));
assert_checkerror('rand(Inf)', _('A real integer value scalar expected.'));
assert_checkerror('randn(2.5)', _('A real integer value scalar expected.'));
assert_checkerror('randi(5, 2.5)', _('A real integer value scalar expected.'));
asserts.isequal(size(rand(true)), [1 1]);
asserts.isequal(size(rand([])), [0 0]);
asserts.isequal(size(randn([])), [0 0]);
asserts.isequal(size(randi(5, [])), [0 0]);
asserts.isequal(size(randi(5, -1)), [0 0]);
asserts.isequal(size(randi(5, 2, 3, 4)), [2 3 4]);
asserts.isequal(size(randi(5, 3)), [3 3]);
asserts.isequal(randi(true), 1);
%=============================================================================
% the range argument of randi
assert_checkerror('randi([2 5 7])', _('First input must be a positive scalar integer value IMAX, or two integer values [IMIN IMAX] with IMIN less than or equal to IMAX.'));
assert_checkerror('randi([])', _('First input must be a positive scalar integer value IMAX, or two integer values [IMIN IMAX] with IMIN less than or equal to IMAX.'));
assert_checkerror('randi({5})', _('First input must be a positive scalar integer value IMAX, or two integer values [IMIN IMAX] with IMIN less than or equal to IMAX.'));
%=============================================================================

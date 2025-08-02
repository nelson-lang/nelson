%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/284
% <-- Short Description -->
% Nth dimensions assignation of an empty array with 2d matrix did not work
%=============================================================================
clear('A');
A(:, :, 1) = [2 4; -2 1];
A(:, :, 2) = [9 13; -5 7];
A(:, :, 3) = [4 4; 8 -3];
assert_istrue(isvar('A'));
assert_isequal(size(A), [2 2 3]);
REF1 = [2 4; -2 1];
REF2 = [9 13; -5 7];
REF3 = [4 4; 8 -3];
assert_isequal(A(:, :, 1), REF1);
assert_isequal(A(:, :, 2), REF2);
assert_isequal(A(:, :, 3), REF3);
%=============================================================================

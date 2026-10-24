%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% scalar: binomial coefficient
asserts.isequal(nchoosek(10, 3), 120);
asserts.isequal(nchoosek(52, 5), 2598960);
asserts.isequal(nchoosek(3, 0), 1);
asserts.isequal(nchoosek(0, 0), 1);
asserts.isequal(nchoosek(30, 15), 155117520);
% integer classes preserved (with saturation)
R = nchoosek(uint8(6), 2);
asserts.isequal(R, uint8(15));
asserts.isequal(class(R), 'uint8');
% vector: matrix of combinations in the expected order
asserts.isequal(nchoosek([1 2 3 4], 2), [1 2; 1 3; 1 4; 2 3; 2 4; 3 4]);
asserts.isequal(nchoosek([7 5 6], 2), [7 5; 7 6; 5 6]);
asserts.isequal(nchoosek([1; 2; 3], 2), [1 2; 1 3; 2 3]);
asserts.isequal(nchoosek([5 7], 1), [5; 7]);
asserts.isequal(nchoosek([4 5 6], 3), [4 5 6]);
asserts.isequal(size(nchoosek([1 2 3], 0)), [1 0]);
% class preserved for vectors
R = nchoosek(single([1 2 3]), 2);
asserts.isequal(class(R), 'single');
% non-integer or negative scalars behave as one-element vectors (the expected behavior)
asserts.isequal(nchoosek(2.5, 1), 2.5);
asserts.isequal(size(nchoosek(2.5, 2)), [0 2]);
asserts.isequal(nchoosek(-1, 1), -1);
asserts.isequal(size(nchoosek([1 2], 5)), [0 5]);
% errors
asserts.checkerror('nchoosek(3, 5)', _('K must be an integer between 0 and N.'));
asserts.checkerror('nchoosek(10, -1)', _('K must be a nonnegative integer.'));
asserts.checkerror('nchoosek(10, 1.5)', _('K must be a nonnegative integer.'));
%=============================================================================

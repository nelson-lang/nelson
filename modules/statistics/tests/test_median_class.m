%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% integer classes are preserved, rounding half away from zero
asserts.isequal(median(int8([1 2])), int8(2));
asserts.isequal(class(median(int8([1 2]))), 'int8');
asserts.isequal(median(int8([-1 -2])), int8(-2));
asserts.isequal(median(int8([1 2 3])), int8(2));
asserts.isequal(median(uint8([250 251])), uint8(251));
asserts.isequal(median(int16([1 2; 3 4; 5 6])), int16([3 4]));
%=============================================================================
% single stays single
asserts.isequal(class(median(single([1 2 3]))), 'single');
asserts.isequal(median(single([1 2 3 4])), single(2.5));
%=============================================================================
% empty inputs
asserts.isequal(median([]), NaN);
asserts.isequal(median(int8([])), int8(0));
asserts.isequal(class(median(single([]))), 'single');
asserts.isequal(median(zeros(0, 3)), [NaN NaN NaN]);
%=============================================================================
% nanflag still works
asserts.isequal(median([1 NaN 3], 'omitnan'), 2);
asserts.isequal(median([1 NaN 3]), NaN);
%=============================================================================

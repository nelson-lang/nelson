%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
% mode of an empty array reduces the working dimension to 1: M is NaN, F is 0
% and C holds 0x1 empty arrays. [] (0x0) without dimension gives NaN, with a
% dimension it follows the rule.
%=============================================================================
asserts.isequal(mode(zeros(0, 3)), NaN(1, 3));
asserts.isequal(mode(zeros(0, 3), 2), zeros(0, 1));
asserts.isequal(mode(zeros(3, 0)), zeros(1, 0));
asserts.isequal(mode(zeros(3, 0), 2), NaN(3, 1));
asserts.isequal(mode(zeros(1, 0)), NaN);
asserts.isequal(mode(zeros(0, 3, 2)), NaN(1, 3, 2));
asserts.isequal(mode([]), NaN);
asserts.isequal(mode([], 1), zeros(1, 0));
asserts.isequal(mode([], 2), zeros(0, 1));
%=============================================================================
[M, F, C] = mode(zeros(0, 3));
asserts.isequal(M, NaN(1, 3));
asserts.isequal(F, zeros(1, 3));
asserts.isequal(C, {zeros(0, 1), zeros(0, 1), zeros(0, 1)});
[M, F, C] = mode(zeros(3, 0), 2);
asserts.isequal(F, zeros(3, 1));
asserts.isequal(C, {zeros(0, 1); zeros(0, 1); zeros(0, 1)});
[M, F, C] = mode([], 1);
asserts.isequal(M, zeros(1, 0));
asserts.isequal(F, zeros(1, 0));
asserts.isequal(C, cell(1, 0));
[M, F, C] = mode([]);
asserts.isequal(M, NaN);
asserts.isequal(F, 0);
asserts.isequal(C, {zeros(0, 1)});
%=============================================================================
% single and integer classes are kept (NaN becomes 0 for integers)
asserts.isequal(mode(single(zeros(0, 3))), single(NaN(1, 3)));
asserts.isequal(mode(int8(zeros(0, 3))), int8([0 0 0]));
asserts.isequal(mode(int8([])), int8(0));
asserts.isequal(mode(uint16(zeros(3, 0)), 2), uint16([0; 0; 0]));
[M, F, C] = mode(int8([]));
asserts.isequal(C, {int8(zeros(0, 1))});
[M, F, C] = mode(single(zeros(0, 2)));
asserts.isequal(C, {single(zeros(0, 1)), single(zeros(0, 1))});
asserts.isequal(mode(single([1 1 2])), single(1));
asserts.isequal(mode(int16([4 4 2; 4 1 2]), 2), int16([4; 1]));
%=============================================================================

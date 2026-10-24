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
% mean of an empty array reduces the working dimension to 1: the result keeps
% the other dimensions and is NaN (0/0) where it is not empty.
%=============================================================================
asserts.isequal(mean(zeros(0, 3)), NaN(1, 3));
asserts.isequal(mean(zeros(0, 3), 1), NaN(1, 3));
asserts.isequal(mean(zeros(0, 3), 2), zeros(0, 1));
asserts.isequal(mean(zeros(0, 3), 3), zeros(0, 3));
asserts.isequal(mean(zeros(3, 0)), zeros(1, 0));
asserts.isequal(mean(zeros(3, 0), 2), NaN(3, 1));
asserts.isequal(mean(zeros(1, 0)), NaN);
asserts.isequal(mean(zeros(0, 1)), NaN);
asserts.isequal(mean(zeros(0, 3, 2)), NaN(1, 3, 2));
asserts.isequal(mean(zeros(2, 0, 3), 2), NaN(2, 1, 3));
%=============================================================================
% [] (0x0) without dimension gives NaN, with a dimension it follows the rule
asserts.isequal(mean(zeros(0, 0)), NaN);
asserts.isequal(mean([], 1), zeros(1, 0));
asserts.isequal(mean([], 2), zeros(0, 1));
%=============================================================================
% nanflag
asserts.isequal(mean(zeros(0, 3), 'omitnan'), NaN(1, 3));
asserts.isequal(mean(zeros(0, 3), 'includenan'), NaN(1, 3));
asserts.isequal(mean(zeros(3, 0), 2, 'omitnan'), NaN(3, 1));
%=============================================================================
% vector of dimensions and 'all'
asserts.isequal(mean(zeros(0, 3), [1 2]), NaN);
asserts.isequal(mean(zeros(0, 3), 'all'), NaN);
%=============================================================================
% output classes
asserts.isequal(mean(single(zeros(0, 3))), single(NaN(1, 3)));
asserts.isequal(mean(single(zeros(0, 3)), 'double'), NaN(1, 3));
asserts.isequal(mean(single(zeros(0, 3)), 'native'), single(NaN(1, 3)));
asserts.isequal(mean(int8(zeros(0, 3))), NaN(1, 3));
asserts.isequal(mean(int8(zeros(0, 3)), 'double'), NaN(1, 3));
asserts.isequal(mean(int8(zeros(0, 3)), 'native'), int8([0 0 0]));
asserts.isequal(mean(uint16(zeros(3, 0)), 2, 'native'), uint16([0; 0; 0]));
asserts.isequal(mean(int32(zeros(0, 3)), 1, 'omitnan', 'native'), int32([0 0 0]));
asserts.isequal(mean(int8(zeros(0, 0)), 'native'), int8(0));
asserts.isequal(mean(int8(zeros(0, 3)), 'all', 'native'), int8(0));
asserts.isequal(mean(false(0, 3)), NaN(1, 3));
asserts.isequal(mean(char(zeros(0, 3))), NaN(1, 3));
asserts.isequal(mean(int8(zeros(0, 3)), 2, 'native'), int8(zeros(0, 1)));
%=============================================================================

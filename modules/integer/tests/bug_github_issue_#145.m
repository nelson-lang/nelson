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
% https://github.com/nelson-lang/nelson/issues/145
% <-- Short Description -->
% int32(NaN) did not return 0
%=============================================================================
assert_isequal(int8(NaN), int8(0));
assert_isequal(int16(NaN), int16(0));
assert_isequal(int32(NaN), int32(0));
assert_isequal(int64(NaN), int64(0));
assert_isequal(uint8(NaN), uint8(0));
assert_isequal(uint16(NaN), uint16(0));
assert_isequal(uint32(NaN), uint32(0));
assert_isequal(uint64(NaN), uint64(0));
assert_isequal(double(NaN), NaN);
assert_istrue(isnan(single(NaN)));

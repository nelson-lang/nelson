%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% 'native' computes in double and returns the class of the input;
% 'double' and 'default' return double for an integer input.
%=============================================================================
M = uint8([10:30:70; 20:30:80; 30:30:90]);
asserts.isequal(rms(M, 'native'), uint8([22 51 80]));
asserts.isequal(rms(M, 2, 'native'), uint8([47; 56; 65]));
asserts.isequal(rms(M, 'all', 'native'), uint8(56));
asserts.isequal(rms(M, [1 2], 'native'), uint8(56));
%=============================================================================
asserts.isequal(class(rms(M, 'native')), 'uint8');
asserts.isequal(class(rms(M, 'double')), 'double');
asserts.isequal(class(rms(M, 'default')), 'double');
asserts.isequal(class(rms(M)), 'double');
%=============================================================================
asserts.isequal(rms(int16([100 200 300]), 'native'), int16(216));
asserts.isequal(rms(int32([100000 200000]), 'native'), int32(158114));
asserts.isequal(rms(int64([4000000000 5000000000]), 'native'), int64(4527692569));
asserts.isequal(rms(uint16(300), 'native'), uint16(300));
%=============================================================================
% The native class may be unable to hold the result: it clamps, as a cast does.
asserts.isequal(rms(int8(-128), 'native'), int8(127));
asserts.isequal(rms(int8([100 -128 50]), 'native'), int8(98));
%=============================================================================
% logical is not an integer class, so 'native' keeps returning double.
asserts.isequal(class(rms(true, 'native')), 'double');
asserts.isequal(rms(true, 'native'), 1);
asserts.isequal(class(rms(logical([1 0 1]), 'native')), 'double');
%=============================================================================
% single and double inputs are unaffected by the integer handling.
asserts.isequal(class(rms(single([3 4]), 'native')), 'single');
asserts.isequal(class(rms(single([3 4]), 'double')), 'double');
asserts.isequal(class(rms([3 4], 'native')), 'double');
asserts.isapprox(rms([3 4], 'native'), sqrt(12.5), 1e-12);
%=============================================================================
% 'native' combines with a nan flag.
asserts.isequal(rms(int16([100 200 300]), 'native', 'omitnan'), int16(216));
%=============================================================================

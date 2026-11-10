%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% optional lane mode of 'native' integer sums (index order stays the default)
asserts.isfalse(feature('nativeIntegerLanes'));
k = (1:300000)';
V0 = int8(mod(k * 7919, 251) - 120);
indexOrder = sum(V0(1:100), 'native');
asserts.isequal(indexOrder, int8(127));
cleanup = onCleanup(@() feature('nativeIntegerLanes', false));
feature('nativeIntegerLanes', true);
asserts.istrue(feature('nativeIntegerLanes'));
%=============================================================================
% contiguous lines: index order below 44 elements, four lanes from 44
asserts.isequal(sum(V0(1:43), 'native'), int8(71));
asserts.isequal(sum(V0(1:44), 'native'), int8(-9));
asserts.isequal(sum(V0(1:63), 'native'), int8(35));
asserts.isequal(sum(V0(1:100), 'native'), int8(89));
asserts.isequal(sum(V0(1:100).', 'native'), int8(89));
asserts.isequal(sum(V0(1:1000), 'native'), int8(-48));
asserts.isequal(sum(V0(1:4000), 'native'), int8(-128));
asserts.isequal(sum(V0(1:8193), 'native'), int8(118));
asserts.isequal(sum(V0(1:10000), 'native'), int8(-89));
asserts.isequal(sum(reshape(V0(1:3 * 15), 15, 3), 'all', 'native'), int8(57));
%=============================================================================
% strided lines: index order below 2048 elements, blocks of 1024 from 2048
W0 = int8(mod(k * 104729, 251) - 125);
asserts.isequal(sum(reshape(W0(1:3 * 2047), 3, 2047), 2, 'native'), int8([104; 127; 109]));
asserts.isequal(sum(reshape(W0(1:3 * 4099), 3, 4099), 2, 'native'), int8([-75; 13; 50]));
asserts.isequal(sum(reshape(W0(1:3 * 20000), 3, 20000), 2, 'native'), int8([-45; 127; 127]));
%=============================================================================
% other integer classes follow the same layout
scaled = @(cls, n, a, b) cast(mod((1:n)' * a, 251) - b, cls) * idivide(intmax(cls), cast(127, cls));
asserts.isequal(sum(scaled('int64', 100, 7919, 120), 'native'), int64(6608872876801453) * int64(1000) + int64(529));
asserts.isequal(sum(scaled('int16', 100, 7919, 120), 'native'), int16(23476));
M = [scaled('int32', 1000, 7919, 120), scaled('int32', 1000, 104729, 125), scaled('int32', 1000, 31, 118)];
asserts.isequal(sum(M, 'native'), int32([-777828722 -2 -608735507]));
%=============================================================================
% products keep index order
P = int8(mod((1:100)' * 7919, 7) - 3);
P(P == 0) = int8(-1);
asserts.isequal(prod(P, 'native'), int8(-127));
%=============================================================================
feature('nativeIntegerLanes', false);
asserts.isequal(sum(V0(1:100), 'native'), int8(127));
%=============================================================================

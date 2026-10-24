%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression: with 'UniformOutput', true the preallocated output element size
% is fixed by iteration 0. A later iteration returning a wider data class than
% the first (e.g. int8 then double) used to memcpy 8 bytes into a 1-byte slot,
% corrupting neighbours / overrunning the final element. arrayfun must now
% raise a class-mismatch error instead, matching cellfun behavior.
%=============================================================================
% First element -> int8 (1 byte), later element -> double (8 bytes).
fun = @(x) subsref({int8(x), double(x)}, substruct('{}', {(x >= 2) + 1}));
MSG = 'All outputs of the function must be of the same data type when UniformOutput is true.';
asserts.checkerror('R = arrayfun(fun, [1 2 3]);', MSG);
%=============================================================================
% Reverse order (double first, then int8) is also rejected.
fun2 = @(x) subsref({double(x), int8(x)}, substruct('{}', {(x >= 2) + 1}));
asserts.checkerror('R = arrayfun(fun2, [1 2 3]);', MSG);
%=============================================================================
% Sanity: a homogeneous class still works and preserves type.
R = arrayfun(@(x) int8(x), int8([1 2 3]));
asserts.isequal(R, int8([1 2 3]));
asserts.isequal(class(R), 'int8');
%=============================================================================
% Sanity: the real/complex promotion path is unaffected.
R = arrayfun(@(x) subsref({double(x), complex(double(x), 1)}, ...
    substruct('{}', {(x >= 2) + 1})), [1 2 3]);
asserts.isequal(class(R), 'double');
asserts.istrue(~isreal(R));
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% sum/prod/cumsum/cumprod rejected string (double-quoted) option arguments
% ("Cannot convert string-arrays to any other type."); only char (single
% quoted) options were accepted. String scalars must be accepted too.
%=============================================================================
% sum: string type options
asserts.isequal(sum(int8([100 100 100]), "native"), int8(127));
asserts.isequal(class(sum(int8([1 2 3]), "native")), 'int8');
asserts.isequal(class(sum(int8([1 2 3]), "double")), 'double');
asserts.isequal(class(sum(int8([1 2 3]), "default")), 'double');
asserts.isequal(sum(int8([100 100; 100 100]), "all"), 400);
asserts.isequal(sum(int8([100 100; 100 100]), 1, "native"), int8([127 127]));
%=============================================================================
% prod: string type options
asserts.isequal(prod(int8([10 10 10]), "native"), int8(127));
asserts.isequal(class(prod(int8([1 2 3]), "double")), 'double');
%=============================================================================
% cumsum / cumprod: string direction and nan flags
asserts.isequal(cumsum(int8([1 2 3]), "reverse"), int8([6 5 3]));
asserts.isequal(cumsum([1 NaN 3], "omitnan"), [1 1 4]);
asserts.isequal(cumprod(int8([1 2 3]), "reverse"), int8([6 6 3]));
asserts.isequal(cumsum(int8([1 2; 3 4]), 2, "reverse"), int8([3 2; 7 4]));
%=============================================================================
% string and char options are equivalent
asserts.isequal(sum(int8([100 100 100]), "native"), sum(int8([100 100 100]), 'native'));
asserts.isequal(prod(int8([10 10 10]), "native"), prod(int8([10 10 10]), 'native'));
asserts.isequal(cumsum(int8([1 2 3]), "reverse"), cumsum(int8([1 2 3]), 'reverse'));
%=============================================================================

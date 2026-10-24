%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% statistics of [] give NaN.
asserts.isequal(mean([]), NaN);
asserts.isequal(median([]), NaN);
asserts.isequal(mode([]), NaN);
asserts.isequal(var([]), NaN);
asserts.isequal(std([]), NaN);
%=============================================================================
% classes
asserts.isequal(class(mean([])), 'double');
asserts.isequal(class(mean(int8([]))), 'double');
asserts.isequal(class(var([])), 'double');
%=============================================================================
% mode outputs for []
[m, f, c] = mode([]);
asserts.isequal(m, NaN);
asserts.isequal(f, 0);
asserts.isequal(c, {zeros(0, 1)});
%=============================================================================
% mode C output lists all values with maximal frequency
[m, f, c] = mode([2 2 1 3 3]);
asserts.isequal(m, 2);
asserts.isequal(f, 2);
asserts.isequal(c, {[2; 3]});
%=============================================================================

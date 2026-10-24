%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% unique deduplicates complex values with equal magnitude (regression)
asserts.isequal(unique([1 + 1i 1 - 1i 1 + 1i]), [1 - 1i 1 + 1i]);
asserts.isequal(unique([2i 1i 2i]), [1i 2i]);
%=============================================================================
% the expected order: sort by magnitude, then by angle
asserts.isequal(unique([1 + 2i 1 + 1i 1 + 2i 2 + 0i]), [1 + 1i 2 + 0i 1 + 2i]);
asserts.isequal(unique([3 + 0i 1 + 2i 1 + 1i 2 + 2i 1 + 2i]), [1 + 1i 1 + 2i 2 + 2i 3 + 0i]);
%=============================================================================
% two outputs: u follows input orientation, ia is always a column
[u, ia] = unique([2i 2i 1]);
asserts.isequal(u, [1 2i]);
asserts.isequal(ia, [3; 1]);
asserts.isequal(size(ia), [2 1]);
%=============================================================================
% three outputs: values, ia (first occurrence), ic
[u, ia, ic] = unique([2i 2i 1]);
asserts.isequal(u, [1 2i]);
asserts.isequal(ia, [3; 1]);
asserts.isequal(ic, [2; 2; 1]);
%=============================================================================
% stable option
asserts.isequal(unique([2i 1i 2i], 'stable'), [2i 1i]);
[u, ia, ic] = unique([2i 1i 2i], 'stable');
asserts.isequal(u, [2i 1i]);
asserts.isequal(ia, [1; 2]);
asserts.isequal(ic, [1; 2; 1]);
%=============================================================================
% column orientation preserved
asserts.isequal(unique([2i; 2i; 1]), [1; 2i]);
%=============================================================================
% single precision complex
[u, ia, ic] = unique(single([2i 2i 1]));
asserts.isequal(class(u), 'single');
asserts.isequal(u, single([1 2i]));
asserts.isequal(ia, [3; 1]);
asserts.isequal(ic, [2; 2; 1]);
%=============================================================================
% real path unchanged
asserts.isequal(unique([3 1 2 1 3]), [1 2 3]);
[u, ia, ic] = unique([3 1 2 1 3]);
asserts.isequal(u, [1 2 3]);
asserts.isequal(ia, [2; 3; 1]);
asserts.isequal(ic, [3; 1; 2; 1; 3]);
%=============================================================================

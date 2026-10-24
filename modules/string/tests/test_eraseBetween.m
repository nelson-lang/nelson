%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(eraseBetween('abcdef', 2, 5), 'af');
asserts.isequal(eraseBetween("a[bc]d", "[", "]"), "a[]d");
%=============================================================================
%=============================================================================
% Every start/stop pair is erased, not only the first.
asserts.isequal(eraseBetween("a[x]b[y]c", "[", "]"), "a[]b[]c");
%=============================================================================
s = ["a[x]b", missing, "c[y]d"];
r = eraseBetween(s, "[", "]");
asserts.isequal(ismissing(r), [false, true, false]);
asserts.isequal(r(1), "a[]b");
asserts.isequal(r(3), "c[]d");
%=============================================================================

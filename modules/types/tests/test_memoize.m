%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
asserts.isequal(nargin('memoize'), 1);
asserts.isequal(nargout('memoize'), 1);
%=============================================================================
mf = memoize(@(k) rand(1, 4));
asserts.isequal(class(mf), 'MemoizedFunction');
asserts.istrue(isa(mf, 'handle'));
asserts.istrue(isa(mf.Function, 'function_handle'));
%=============================================================================
% same key -> cached result is returned (identical random draw)
a = mf(1);
b = mf(1);
asserts.isequal(a, b);
% different key -> recomputed (distinct draw)
c = mf(2);
asserts.isfalse(isequal(a, c));
%=============================================================================
% clearCache forces recomputation
clearCache(mf);
d = mf(1);
asserts.isfalse(isequal(a, d));
% h.method without parentheses goes through subsref, the method still
% dispatches its own calls (keys/remove on the cache map)
mf.clearCache;
asserts.isfalse(isequal(d, mf(1)));
asserts.isequal(mf.Enabled, true);
%=============================================================================
% Enabled = false bypasses the cache
mf.Enabled = false;
e1 = mf(1);
e2 = mf(1);
asserts.isfalse(isequal(e1, e2));
%=============================================================================

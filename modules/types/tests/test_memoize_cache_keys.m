%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% cache hits and misses: one argument and several arguments build their keys
% the same way
%=============================================================================
mf = memoize(@(varargin) rand(1, 4));
a = mf(1);
asserts.isequal(mf(1), a);
% class is part of the key
asserts.isfalse(isequal(mf(int8(1)), a));
asserts.isfalse(isequal(mf(true), a));
c = mf('a');
asserts.isequal(mf('a'), c);
asserts.isfalse(isequal(mf(97), c));
% cell and string arguments
d = mf({1, 'x'});
asserts.isequal(mf({1, 'x'}), d);
s = mf("str");
asserts.isequal(mf("str"), s);
asserts.isfalse(isequal(mf('str'), s));
% several arguments
e = mf(1, 2);
asserts.isequal(mf(1, 2), e);
asserts.isfalse(isequal(e, a));
asserts.isfalse(isequal(mf(2, 1), e));
% the number of outputs is part of the key
f = memoize(@max);
[m, i] = f([3 7 2]);
asserts.isequal([m, i], [7 2]);
asserts.isequal(f([3 7 2]), 7);
[m, i] = f([3 7 2]);
asserts.isequal([m, i], [7 2]);
% unsupported inputs are not cached
g = mf(struct('v', 1));
asserts.isfalse(isequal(mf(struct('v', 1)), g));
%=============================================================================

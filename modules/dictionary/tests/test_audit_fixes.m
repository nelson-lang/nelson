%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% single complex keys: full data (real and imaginary parts) must be hashed
d = dictionary(single(1 + 2i), 10);
d(single(1 + 3i)) = 20;
asserts.isequal(numEntries(d), 2);
asserts.isequal(d(single(1 + 2i)), 10);
asserts.isequal(d(single(1 + 3i)), 20);
%=============================================================================
asserts.isfalse(keyMatch(single(1 + 2i), single(1 + 3i)));
asserts.istrue(keyMatch(single(1 + 2i), single(1 + 2i)));
%=============================================================================
% 0 and -0 must be the same key
asserts.istrue(keyMatch(0, -0));
asserts.isequal(keyHash(0), keyHash(-0));
asserts.istrue(keyMatch(single(0), single(-0)));
asserts.isequal(keyHash(single(0)), keyHash(single(-0)));
%=============================================================================
d2 = dictionary([0 -0], [1 2]);
asserts.isequal(numEntries(d2), 1);
asserts.isequal(d2(0), 2);
asserts.isequal(d2(-0), 2);
%=============================================================================
% keyMatch is a real comparison, not only a hash comparison
asserts.istrue(keyMatch(1, 1));
asserts.istrue(keyMatch(NaN, NaN));
asserts.isfalse(keyMatch(1, 0));
asserts.isfalse(keyMatch(NaN, Inf));
%=============================================================================
X = struct("field1", 1, "field2", 2);
Y = struct("field1", 1, "field2", 2);
asserts.istrue(keyMatch(X, Y));
Y.field1 = 3;
asserts.isfalse(keyMatch(X, Y));
%=============================================================================

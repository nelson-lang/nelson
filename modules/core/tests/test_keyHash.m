%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('keyHash'), 1);
assert_isequal(nargout('keyHash'), 1);
%=============================================================================
R = keyHash(2);
assert_isfalse(isequal(R, keyHash(3)));
for i = 1:1000
  assert_isequal(R, keyHash(2))
end
%=============================================================================
A = keyHash(["gg", "a"]);
B = keyHash(["a", "gg"]);
assert_isfalse(isequal(A, B));
%=============================================================================
A = keyHash({'gg', 'a'});
B = keyHash({'a', 'gg'});
assert_isfalse(isequal(A, B));
%=============================================================================
stA = [];
stA.A = 'gg';
stA.B = 'a';
stB = [];
stB.A = 'a';
stB.B = 'gg';
A = keyHash(stA);
B = keyHash(stB);
assert_isfalse(isequal(A, B));
%=============================================================================
A = [1 2 3 4];
B = [1 2 3 4];
keyHash(A);
keyHash(B);
assert_isequal(keyHash(A), keyHash(B));
%=============================================================================

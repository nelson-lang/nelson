%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('keyHash'), 1);
asserts.isequal(nargout('keyHash'), 1);
%=============================================================================
R = keyHash(2);
asserts.isfalse(isequal(R, keyHash(3)));
for i = 1:1000
  asserts.isequal(R, keyHash(2))
end
%=============================================================================
A = keyHash(["gg", "a"]);
B = keyHash(["a", "gg"]);
asserts.isfalse(isequal(A, B));
%=============================================================================
A = keyHash({'gg', 'a'});
B = keyHash({'a', 'gg'});
asserts.isfalse(isequal(A, B));
%=============================================================================
stA = [];
stA.A = 'gg';
stA.B = 'a';
stB = [];
stB.A = 'a';
stB.B = 'gg';
A = keyHash(stA);
B = keyHash(stB);
asserts.isfalse(isequal(A, B));
%=============================================================================
A = [1 2 3 4];
B = [1 2 3 4];
keyHash(A);
keyHash(B);
asserts.isequal(keyHash(A), keyHash(B));
%=============================================================================

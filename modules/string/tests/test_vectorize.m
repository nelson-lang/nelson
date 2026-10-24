%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(vectorize('x^2 + y*z - a/b'), 'x.^2 + y.*z - a./b');
asserts.isequal(vectorize('sin(x)+a.^b'), 'sin(x)+a.^b');
asserts.isequal(vectorize('f(''a*b'')'), 'f(''a.*b'')');
%=============================================================================
% char input still returns a char (class preserved).
asserts.istrue(ischar(vectorize('x^2*y')));
%=============================================================================
% function handle input returns the vectorized SOURCE as a char,
% equivalent to vectorize(func2str(fh)) - NOT a function handle.
% Chebfun's chebop relies on this: strcmp(func2str(fh), vectorize(fh)).
f = vectorize(@(x, y) x ^ 2 * y);
asserts.istrue(ischar(f));
asserts.isequal(f, vectorize(func2str(@(x, y) x ^ 2 * y)));
%=============================================================================
% An already-vectorized (or operator-free) handle is unchanged, so it compares
% equal to its own func2str - this is exactly the chebop 'already vectorized?'
% guard, and it must be TRUE to avoid needless re-vectorization.
op = @(x, u) diff(u, 2) + u;
asserts.istrue(strcmp(func2str(op), vectorize(op)));
%=============================================================================
% The dotting is applied to the body operators.
asserts.isequal(vectorize(@(x) x ^ 2), vectorize(func2str(@(x) x ^ 2)));
asserts.isequal(vectorize(@(x, y) x ^ 2 * y / 3), vectorize(func2str(@(x, y) x ^ 2 * y / 3)));
%=============================================================================

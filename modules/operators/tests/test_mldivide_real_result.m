%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A complex quotient with only zero imaginary parts is returned as real.
X = complex([1 2; 2 4; 3 6]) \ complex([1; 1; 1]);
asserts.istrue(isreal(X));
asserts.isapprox(X, [0; 3 / 14], 1e-14);
%=============================================================================
X = complex([1 2; 2 4; 3 6]) \ [1; 1; 1];
asserts.istrue(isreal(X));
X = [1 2; 2 4; 3 6] \ complex([1; 1; 1]);
asserts.istrue(isreal(X));
%=============================================================================
X = complex([1 2 3; 4 5 7]) \ complex([1; 2]);
asserts.istrue(isreal(X));
asserts.isapprox(X, [-0.2; 0; 0.4], 1e-14);
%=============================================================================
X = complex([2 1; 1 3]) \ complex([1; 2]);
asserts.istrue(isreal(X));
asserts.isapprox(X, [0.2; 0.6], 1e-14);
X = complex([2 1; 4 3]) \ complex([1; 2]);
asserts.istrue(isreal(X));
asserts.isapprox(X, [0.5; 0], 1e-14);
X = complex([2 1; 0 3]) \ complex([1; 2]);
asserts.istrue(isreal(X));
X = complex([2 0; 0 3]) \ complex([1; 2]);
asserts.istrue(isreal(X));
asserts.isequal(X, [0.5; 2 / 3]);
%=============================================================================
X = [1i 0; 0 2i] \ [1i; 2i];
asserts.istrue(isreal(X));
asserts.isequal(X, [1; 1]);
X = [1i 1i; 0 2i] \ [2i; 2i];
asserts.istrue(isreal(X));
asserts.isequal(X, [1; 1]);
%=============================================================================
X = complex(single([2 1; 4 3])) \ complex(single([1; 2]));
asserts.istrue(isreal(X));
asserts.isequal(class(X), 'single');
X = complex(single([1 2; 2 4; 3 6])) \ complex(single([1; 1; 1]));
asserts.istrue(isreal(X));
asserts.isequal(class(X), 'single');
X = complex(single([2 1; 4 3])) \ [1; 2];
asserts.istrue(isreal(X));
asserts.isequal(class(X), 'single');
%=============================================================================
Y = complex([1 2]) / complex([2 1; 4 3]);
asserts.istrue(isreal(Y));
asserts.isapprox(Y, [-2.5 1.5], 1e-14);
Y = complex([1 1 1]) / complex([1 2 3; 2 4 6]);
asserts.istrue(isreal(Y));
asserts.isapprox(Y, [0 3 / 14], 1e-14);
Y = [1 2] / complex([2 1; 4 3]);
asserts.istrue(isreal(Y));
%=============================================================================
X = sparse(complex([2 1; 4 3])) \ complex([1; 2]);
asserts.istrue(isreal(X));
asserts.isfalse(issparse(X));
X = complex([2 1; 4 3]) \ sparse(complex([1; 2]));
asserts.istrue(isreal(X));
X = sparse(complex([1 2; 3 4; 5 7])) \ complex([1; 1; 1]);
asserts.istrue(isreal(X));
asserts.isapprox(X, [-8 / 7; 1], 1e-12);
%=============================================================================
% A non-zero or NaN imaginary part keeps the result complex.
X = [2 1i; 1 3] \ [1; 2];
asserts.isfalse(isreal(X));
X = [1i 0 1; 0 1i 0; 0 0 1] \ [1i; 1i; 1];
asserts.isfalse(isreal(X));
asserts.isapprox(X, [1 + 1i; 1; 1], 1e-14);
lastwarn('', '');
X = complex([1 2; 2 4]) \ complex([1; 2]);
asserts.isfalse(isreal(X));
asserts.istrue(all(isnan(real(X))) && all(isnan(imag(X))));
%=============================================================================
% A sparse numerator over a dense divisor factors the divisor itself.
lastwarn('', '');
Y = sparse([1 2 3]) / [1 2 3; 4 5 6; 7 8 9];
[msg, id] = lastwarn();
asserts.isfalse(issparse(Y));
asserts.isequal(Y, [1 0 0]);
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
asserts.isequal(Y, [1 2 3] / [1 2 3; 4 5 6; 7 8 9]);
%=============================================================================
lastwarn('', '');
Y = sparse([1 1]) / [1 2; 2 4];
[msg, id] = lastwarn();
asserts.istrue(isequaln(Y, [-Inf Inf]));
asserts.isequal(id, 'Nelson:singularMatrix');
Y = sparse([1 2]) / [1 2; 2 4];
asserts.istrue(isequaln(Y, [NaN NaN]));
Y = sparse([1 2]) / [2 1; 0 0];
asserts.istrue(isequaln(Y, [0.5 Inf]));
Y = sparse([1 2]) / [2 0; 1 0];
asserts.istrue(isequaln(Y, [-Inf Inf]));
%=============================================================================
lastwarn('', '');
Y = sparse([1 2]) / [1 0; -1 2e-16];
[msg, id] = lastwarn();
asserts.isapprox(Y, [1e16 1e16], 1e-12);
asserts.isequal(id, 'Nelson:nearlySingularMatrix');
%=============================================================================
Y = sparse([1 2; 3 4]) / [2 1; 4 3];
asserts.isfalse(issparse(Y));
asserts.isapprox(Y, [-2.5 1.5; -3.5 2.5], 1e-14);
Y = sparse([1 2 3]) / single([1 2 3; 4 5 6; 7 8 9]);
asserts.isequal(class(Y), 'single');
Y = sparse([1 2]) / complex([1 2; 2 4]);
asserts.isfalse(isreal(Y));
asserts.istrue(all(isnan(real(Y))) && all(isnan(imag(Y))));
%=============================================================================

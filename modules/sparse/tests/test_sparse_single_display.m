%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% a sparse single array displays its nonzeros like a sparse double array.
%=============================================================================
m = sparse(single([1 0 2; 0 3 0]));
R = evalc('m');
asserts.istrue(contains(R, 'sparse single matrix (3 nonzeros)'));
asserts.istrue(contains(R, '(1,1)        1'));
asserts.istrue(contains(R, '(2,2)        3'));
asserts.istrue(contains(R, '(1,3)        2'));
%=============================================================================
x = [single(1), sparse(1)];
y = x(:);
R = evalc('y');
asserts.istrue(contains(R, 'sparse single column vector (2 nonzeros)'));
%=============================================================================
c = sparse(single([1+2i 0 3]));
R = evalc('c');
asserts.istrue(contains(R, 'sparse complex single row vector (2 nonzeros)'));
asserts.istrue(contains(R, '(1,1)      1.0000 + 2.0000i'));
asserts.istrue(contains(R, '(1,3)      3.0000 + 0.0000i'));
%=============================================================================
r = sparse(single(pi));
R = evalc('r');
asserts.istrue(contains(R, 'sparse single'));
asserts.istrue(contains(R, '(1,1)       3.1416'));
%=============================================================================
e = sparse(single(zeros(2, 2)));
R = evalc('e');
asserts.istrue(contains(R, 'sparse single matrix'));
asserts.istrue(contains(R, 'All zero'));
%=============================================================================

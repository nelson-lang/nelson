%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = sparse(zeros(3, 0));
R = A';
asserts.isequal(size(R), [0 3]);
%=============================================================================
A = sparse(eye(3, 3) * i);
R = A';
REF = sparse(eye(3, 3) * -i);
asserts.isequal(R, REF);
%=============================================================================
As = sparse(single([1 0 2; 0 3 0]));
Rs = As';
asserts.istrue(issparse(Rs));
asserts.isequal(class(Rs), 'single');
asserts.isequal(size(Rs), [3 2]);
asserts.isapprox(full(Rs), single([1 0; 0 3; 2 0]), 1e-6);
%=============================================================================
Cs = sparse(single([1 + 2i 0; 3 - 4i 5]));
Rcs = Cs';
asserts.istrue(issparse(Rcs));
asserts.isequal(class(Rcs), 'single');
asserts.isequal(size(Rcs), [2 2]);
asserts.isapprox(full(Rcs), single([1 - 2i 3 + 4i; 0 5]), 1e-6);
%=============================================================================
Es = sparse(single(zeros(3, 0)));
Res = Es';
asserts.istrue(issparse(Res));
asserts.isequal(class(Res), 'single');
asserts.isequal(size(Res), [0 3]);
%=============================================================================

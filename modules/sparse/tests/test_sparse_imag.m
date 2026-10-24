%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [1 0 0 0;
  0 2 0 0;
  0 0 3 0;
  0 0 0 4];
CX = sparse(complex(A));
R = imag(CX);
REF = sparse(zeros(4, 4));
asserts.isequal(R, REF);
%=============================================================================
S = sparse(single([1 + 2i 0; 0 3 - 4i]));
R = imag(S);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), single([2 0; 0 -4]));
%=============================================================================
SR = sparse(single([1 0; 0 3]));
R = imag(SR);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), single(zeros(2, 2)));
%=============================================================================

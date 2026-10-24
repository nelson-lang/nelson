%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('issparse'), 1);
asserts.isequal(nargout('issparse'), 1);
%=============================================================================
A = 1;
res = issparse(A);
asserts.isfalse(res);
%=============================================================================
A = sparse(1);
res = issparse(A);
asserts.istrue(res);
%=============================================================================
A = sparse(true);
res = issparse(A);
asserts.istrue(res);
%=============================================================================
asserts.istrue(issparse(sparse([1 + 2i 0; 0 0])));
asserts.istrue(issparse(sparse(single([1 0; 0 2]))));
asserts.istrue(issparse(sparse(single([1 + 2i 0; 0 0]))));
asserts.istrue(issparse(sparse(zeros(0, 3))));
asserts.istrue(issparse(sparse(zeros(3, 0))));
%=============================================================================
asserts.isfalse(issparse(single([1 0; 0 2])));
asserts.isfalse(issparse(single([1 + 2i 0; 0 0])));
%=============================================================================
asserts.isfalse(issparse([]));
asserts.isfalse(issparse({sparse(1)}));
%=============================================================================

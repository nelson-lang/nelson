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
asserts.isequal(nargin('isa'), 2);
asserts.isequal(nargout('isa'), 1);
%=============================================================================
A = 3;
res = isa(A, 'double');
asserts.istrue(res);
%=============================================================================
B = {'NelSon', 3, true};
res = isa(B, 'cell');
asserts.istrue(res);
%=============================================================================
B = {'NelSon', 3, true};
res = isa(B, 'cell');
asserts.istrue(res);
%=============================================================================
B = {'NelSon', 3, true};
res = isa(B, 'float');
asserts.isfalse(res);
%=============================================================================
B = {'NelSon', 3, true};
res = isa(B, 'numeric');
asserts.isfalse(res);
%=============================================================================
B = {'NelSon', 3, true};
res = isa(B, 'integer');
asserts.isfalse(res);
%=============================================================================
A = sparse([2 0 3]);
asserts.istrue(isa(A, 'double'));
asserts.isfalse(isa(A, 'sparsedouble'));
asserts.istrue(issparse(A));
%=============================================================================
A = logical(sparse([1 0 1]));
asserts.istrue(isa(A, 'logical'));
asserts.isfalse(isa(A, 'sparselogical'));
asserts.istrue(issparse(A));
%=============================================================================

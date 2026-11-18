%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% an empty sparse matrix stays sparse, has no stored values and densifies to []
S = sparse([]);
asserts.istrue(issparse(S));
asserts.isequal(size(S), [0 0]);
asserts.isequal(nnz(S), 0);
asserts.isequal(full(sparse(0, 0)), []);
%=============================================================================
% a zero-order sparse identity is the empty matrix
asserts.isequal(size(speye(0)), [0 0]);
%=============================================================================
% NaN and Inf are legitimate stored values: they are kept and counted
T = sparse([1 2], [1 2], [NaN Inf]);
asserts.isequal(nnz(T), 2);
F = full(T);
asserts.istrue(isnan(F(1, 1)));
asserts.isequal(F(1, 2), 0);
asserts.isequal(F(2, 1), 0);
asserts.istrue(isinf(F(2, 2)) && F(2, 2) > 0);
nz = nonzeros(T);
asserts.istrue(isnan(nz(1)));
asserts.istrue(isinf(nz(2)) && nz(2) > 0);
%=============================================================================
% spones replaces every stored value by 1, NaN and Inf included
asserts.isequal(nnz(spones(sparse(1, 1, NaN))), 1);
asserts.isequal(full(spones(T)), [1 0; 0 1]);
%=============================================================================

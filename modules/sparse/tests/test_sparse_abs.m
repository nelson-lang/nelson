%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
M = sparse([]);
R = abs(M);
asserts.isequal(R, M);
%=============================================================================
M = logical(sparse(1));
R = abs(M);
REF = double(sparse(true));
asserts.isequal(R, REF);
%=============================================================================
M = sparse(single([-2 0; 0 3]));
R = abs(M);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isequal(full(R), single([2 0; 0 3]));
%=============================================================================
M = sparse(single([1 + 2i 0; -3i 4]));
R = abs(M);
asserts.istrue(issparse(R));
asserts.isequal(class(R), 'single');
asserts.isapprox(full(R), single([sqrt(5) 0; 3 4]), 1e-6);
%=============================================================================

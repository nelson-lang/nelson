%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
asserts.isequal(nargin('svds'), 2);
asserts.isequal(nargout('svds'), 3);
%=============================================================================
A = sparse([4 1 0; 1 3 0; 0 0 2]);
try
  s = svds(A, 2);
catch ex
  skip_testsuite(strcmp(ex.message, _('ARPACK support is not available.')), ex.message);
  rethrow(ex);
end
%=============================================================================
REF = [4.618033988749895; 2.381966011250105];
asserts.isapprox(s, REF, 1e-12);
[U, S, V] = svds(A, 2);
asserts.isapprox(diag(S), REF, 1e-12);
asserts.isapprox(A * V, U * S, 1e-12);
%=============================================================================

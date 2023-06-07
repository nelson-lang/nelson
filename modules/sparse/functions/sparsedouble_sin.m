%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = sparsedouble_sin(a)
  [I, J, V, m, n, nz] = IJV(a);
  r = sparse(I, J, sin(V), m, n, nz);
end

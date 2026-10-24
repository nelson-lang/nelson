%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Regression: JuliaObjectHandle::invoke boxed the call arguments into a bare
% array with no GC root, so a collection while boxing a later argument could free
% an earlier one (dangling pointer passed to jl_call). The collector is now
% disabled around boxing + the call. Hammer multi-argument method calls so any
% missing root would surface as a crash or a wrong result.
%=============================================================================
juliaRequireEngineForTest();
%=============================================================================
R = jlrun('A = Base', 'A');
for k = 1:10
  asserts.isequal(R.max(1.0, 2.0), 2);
  asserts.isapprox(R.hypot(3.0, 4.0), 5, 1e-9);
  asserts.isequal(R.min(7.0, 4.0), 4);
end
%=============================================================================

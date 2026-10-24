%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--JULIA ENVIRONMENT REQUIRED-->
%=============================================================================
juliaRequireEngineForTest();
%=============================================================================
% A table maps to a DataFrames.DataFrame when the package is available,
% otherwise it falls back to the historical Dict representation.
hasDataFrames = false;
try
  jlrun('import DataFrames');
  hasDataFrames = true;
catch
  hasDataFrames = false;
end
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, 'RowNames', {'r1', 'r2'});
R = jlrun('', 'T', 'T', T);
if hasDataFrames
  asserts.istrue(endsWith(R.typeof(), 'DataFrame'));
else
  asserts.isequal(R.typeof(), 'Dict{Any, Any}');
end
%=============================================================================

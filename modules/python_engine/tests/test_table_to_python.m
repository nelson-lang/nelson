%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--PYTHON ENVIRONMENT REQUIRED-->
%=============================================================================
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'}, 'RowNames', {'r1', 'r2'});
R = pyrun("R = T", "R", "T", T);
havePandas = true;
try
  pyrun('import pandas as pd');
catch
  havePandas = false;
end
%=============================================================================
if havePandas
  % When pandas is available a table converts to a pandas DataFrame,
  % preserving the variable names (columns) and row names (index).
  isDataFrame = pyrun("import pandas as pd; tf = bool(isinstance(R, pd.DataFrame))", "tf", "R", R);
  asserts.istrue(logical(isDataFrame));
  columns = pyrun("cols = list(R.columns)", "cols", "R", R);
  asserts.isequal(cellfun(@char, cell(columns), 'UniformOutput', false), {'A', 'B'});
  asserts.isequal(double(R.A.to_numpy()), [1 2]);
  asserts.isequal(double(R.B.to_numpy()), [3 4]);
  indexValues = pyrun("idx = [str(v) for v in R.index]", "idx", "R", R);
  asserts.isequal(cellfun(@char, cell(indexValues), 'UniformOutput', false), {'r1', 'r2'});
else
  % Without pandas, a table falls back to a dictionary representation.
  asserts.isequal(class(R), 'py.dict');
  S = R.struct();
  asserts.isequal(fieldnames(S), {'data'; 'Properties'});
  data = S.data.struct();
  asserts.isequal(fieldnames(data), {'A'; 'B'});
  asserts.isequal(data.A.double(), [1, 2]);
  asserts.isequal(data.B.double(), [3, 4]);
  props = S.Properties.struct();
  variableNames = props.VariableNames.cell();
  asserts.isequal(variableNames{1}.char(), 'A');
  asserts.isequal(variableNames{2}.char(), 'B');
  rowNames = props.RowNames.cell();
  asserts.isequal(rowNames{1}.char(), 'r1');
  asserts.isequal(rowNames{2}.char(), 'r2');
end
%=============================================================================

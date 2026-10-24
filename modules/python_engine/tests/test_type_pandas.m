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
env = pyenv();
try
  pyrun('import pandas as pd');
catch
  skip_testsuite(true, 'pandas not available')
end
%=============================================================================
% DataFrame -> table: variable names, values and column types.
df = pyrun("import pandas as pd; df = pd.DataFrame({'a': [1, 2, 3], 'b': [10.5, 20.5, 30.5]})", "df");
T = table(df);
asserts.isequal(class(T), 'table');
asserts.isequal(T.Properties.VariableNames, {'a', 'b'});
asserts.isequal(double(T.a), [1; 2; 3]);
asserts.isequal(T.b, [10.5; 20.5; 30.5]);
asserts.istrue(isempty(T.Properties.RowNames));
%=============================================================================
% DataFrame with a non-default index: the index becomes a leading 'index'
% variable (reset_index semantics), the table keeps no row names.
df = pyrun("import pandas as pd; df = pd.DataFrame({'v': [1.0, 2.0]}, index=['r1', 'r2'])", "df");
T = table(df);
asserts.isequal(T.Properties.VariableNames, {'index', 'v'});
asserts.isequal(T.index, ["r1"; "r2"]);
asserts.isequal(T.v, [1.0; 2.0]);
asserts.istrue(isempty(T.Properties.RowNames));
%=============================================================================
% DataFrame with a default RangeIndex: no 'index' column, no row names.
df = pyrun("import pandas as pd; df = pd.DataFrame({'v': [1.0, 2.0, 3.0]})", "df");
T = table(df);
asserts.isequal(T.Properties.VariableNames, {'v'});
asserts.istrue(isempty(T.Properties.RowNames));
%=============================================================================
% Series with a name -> single table variable keeping the name.
s = pyrun("import pandas as pd; s = pd.Series([1.0, 2.0, 3.0], name='myname')", "s");
T = table(s);
asserts.isequal(T.Properties.VariableNames, {'myname'});
asserts.isequal(T.myname, [1.0; 2.0; 3.0]);
%=============================================================================
% Series without a name -> column labelled 0, sanitized to a valid name.
s = pyrun("import pandas as pd; s = pd.Series([1.0, 2.0, 3.0])", "s");
T = table(s);
asserts.isequal(T.Properties.VariableNames, {'Var0'});
asserts.isequal(T.Properties.VariableDescriptions, {'0'});
%=============================================================================
% Textual column -> Nelson string column.
df = pyrun("import pandas as pd; df = pd.DataFrame({'name': ['x', 'y', 'z'], 'val': [1.0, 2.0, 3.0]})", "df");
T = table(df);
asserts.isequal(class(T.name), 'string');
asserts.isequal(T.name, ["x"; "y"; "z"]);
asserts.isequal(T.val, [1.0; 2.0; 3.0]);
%=============================================================================
% Column name that is not a valid identifier is sanitized; the original label
% is preserved in VariableDescriptions.
df = pyrun("import pandas as pd; df = pd.DataFrame({'my col': [1.0, 2.0]})", "df");
T = table(df);
asserts.isequal(T.Properties.VariableNames, {'my_col'});
asserts.isequal(T.Properties.VariableDescriptions, {'my col'});
%=============================================================================
% Empty DataFrame must not crash.
df = pyrun("import pandas as pd; df = pd.DataFrame()", "df");
T = table(df);
asserts.isequal(class(T), 'table');
asserts.istrue(isempty(T));
%=============================================================================
% table -> DataFrame: an implicit conversion when passed to Python.
T = table([1; 2; 3], [4.5; 5.5; 6.5], 'VariableNames', {'x', 'y'});
R = pyrun("R = T", "R", "T", T);
isDataFrame = pyrun("import pandas as pd; tf = bool(isinstance(R, pd.DataFrame))", "tf", "R", R);
asserts.istrue(logical(isDataFrame));
columns = pyrun("cols = list(R.columns)", "cols", "R", R);
asserts.isequal(cellfun(@char, cell(columns), 'UniformOutput', false), {'x', 'y'});
asserts.isequal(double(R.x.to_numpy()), [1 2 3]);
%=============================================================================
% table -> DataFrame -> table round-trip preserves names and values.
T = table([1; 2; 3], [4; 5; 6], 'VariableNames', {'aa', 'bb'});
R = pyrun("R = T", "R", "T", T);
T2 = table(R);
asserts.isequal(T2.Properties.VariableNames, {'aa', 'bb'});
asserts.isequal(double(T2.aa), [1; 2; 3]);
asserts.isequal(double(T2.bb), [4; 5; 6]);
%=============================================================================
% table -> DataFrame keeps the row names as the DataFrame index.
T = table([1; 2], 'VariableNames', {'z'}, 'RowNames', {'r1', 'r2'});
R = pyrun("R = T", "R", "T", T);
indexValues = pyrun("idx = [str(v) for v in R.index]", "idx", "R", R);
asserts.isequal(cellfun(@char, cell(indexValues), 'UniformOutput', false), {'r1', 'r2'});
%=============================================================================

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
% The DataFrame <-> table conversions require the DataFrames.jl package.
hasDataFrames = false;
try
  jlrun('import DataFrames');
  hasDataFrames = true;
catch
  hasDataFrames = false;
end
if ~hasDataFrames
  skip_testsuite(true, 'DataFrames.jl not available')
end
%=============================================================================
% DataFrame -> table: variable names, values and column types.
df = jlrun('import DataFrames; df = DataFrames.DataFrame(a = [1.0, 2.0, 3.0], b = [10.5, 20.5, 30.5])', 'df');
T = table(df);
asserts.isequal(class(T), 'table');
asserts.isequal(T.Properties.VariableNames, {'a', 'b'});
asserts.isequal(T.a, [1.0; 2.0; 3.0]);
asserts.isequal(T.b, [10.5; 20.5; 30.5]);
asserts.istrue(isempty(T.Properties.RowNames));
%=============================================================================
% Integer column keeps an integer Nelson type.
df = jlrun('import DataFrames; df = DataFrames.DataFrame(v = [1, 2, 3])', 'df');
T = table(df);
asserts.isequal(class(T.v), 'int64');
asserts.isequal(double(T.v), [1; 2; 3]);
%=============================================================================
% Boolean column keeps a logical Nelson type.
df = jlrun('import DataFrames; df = DataFrames.DataFrame(flag = [true, false, true])', 'df');
T = table(df);
asserts.isequal(class(T.flag), 'logical');
asserts.isequal(T.flag, [true; false; true]);
%=============================================================================
% Complex column keeps a complex Nelson type.
df = jlrun('import DataFrames; df = DataFrames.DataFrame(z = [1.0 + 2.0im, 3.0 - 4.0im])', 'df');
T = table(df);
asserts.isequal(T.z, [1.0 + 2.0i; 3.0 - 4.0i]);
%=============================================================================
% Numeric column with a missing value -> double column with NaN.
df = jlrun('import DataFrames; df = DataFrames.DataFrame(v = [1.0, missing, 3.0])', 'df');
T = table(df);
asserts.isequal(class(T.v), 'double');
asserts.isequal(T.v(1), 1.0);
asserts.istrue(isnan(T.v(2)));
asserts.isequal(T.v(3), 3.0);
%=============================================================================
% Integer column with a missing value is promoted to double with NaN.
df = jlrun('import DataFrames; df = DataFrames.DataFrame(v = [1, missing, 3])', 'df');
T = table(df);
asserts.isequal(class(T.v), 'double');
asserts.istrue(isnan(T.v(2)));
%=============================================================================
% Text column with a missing value -> string column with <missing>.
df = jlrun('import DataFrames; df = DataFrames.DataFrame(s = ["a", missing, "c"])', 'df');
T = table(df);
asserts.isequal(class(T.s), 'string');
asserts.isequal(T.s(1), "a");
asserts.istrue(ismissing(T.s(2)));
asserts.isequal(T.s(3), "c");
%=============================================================================
% Textual column -> Nelson string column.
df = jlrun('import DataFrames; df = DataFrames.DataFrame(name = ["x", "y", "z"], val = [1.0, 2.0, 3.0])', 'df');
T = table(df);
asserts.isequal(class(T.name), 'string');
asserts.isequal(T.name, ["x"; "y"; "z"]);
asserts.isequal(T.val, [1.0; 2.0; 3.0]);
%=============================================================================
% Column label that is not a valid identifier is sanitized; the original label
% is preserved in VariableDescriptions.
df = jlrun('import DataFrames; df = DataFrames.DataFrame("my col" => [1.0, 2.0])', 'df');
T = table(df);
asserts.isequal(T.Properties.VariableNames, {'my_col'});
asserts.isequal(T.Properties.VariableDescriptions, {'my col'});
%=============================================================================
% Empty DataFrame must not crash.
df = jlrun('import DataFrames; df = DataFrames.DataFrame()', 'df');
T = table(df);
asserts.isequal(class(T), 'table');
asserts.istrue(isempty(T));
%=============================================================================
% table -> DataFrame: an implicit conversion when passed to Julia.
T = table([1; 2; 3], [4.5; 5.5; 6.5], 'VariableNames', {'x', 'y'});
R = jlrun('', 'T', 'T', T);
asserts.istrue(endsWith(R.typeof(), 'DataFrame'));
% Verify the DataFrame keeps the column names and values by converting it back.
Tback = table(R);
asserts.isequal(Tback.Properties.VariableNames, {'x', 'y'});
asserts.isequal(Tback.x, [1; 2; 3]);
asserts.isequal(Tback.y, [4.5; 5.5; 6.5]);
%=============================================================================
% table -> DataFrame -> table round-trip preserves names and values.
T = table([1; 2; 3], [4; 5; 6], 'VariableNames', {'aa', 'bb'});
R = jlrun('', 'T', 'T', T);
T2 = table(R);
asserts.isequal(T2.Properties.VariableNames, {'aa', 'bb'});
asserts.isequal(double(T2.aa), [1; 2; 3]);
asserts.isequal(double(T2.bb), [4; 5; 6]);
%=============================================================================

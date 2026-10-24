%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The internal storage of tables and timetables (data, Table, Version) is not
% reachable by dot indexing: these names are ordinary variable names.
%=============================================================================
TT = timetable(seconds([1; 2]), [5; 6], 'VariableNames', {'Table'});
assert_isequal(TT.Table, [5; 6]);
TT = timetable(seconds([1; 2]), [5; 6], 'VariableNames', {'Version'});
assert_isequal(TT.Version, [5; 6]);
%=============================================================================
T = table([1; 2]);
names = {'data', 'DATA', 'Version', 'version', 'Data'};
for k = 1:numel(names)
  name = names{k};
  errorId = '';
  try
    value = T.(name);
  catch e
    errorId = e.identifier;
  end
  assert_isequal(errorId, 'Nelson:table:unrecognizedVariable');
end
%=============================================================================
T = table([1; 2]);
T.Version = [3; 4];
assert_isequal(T.Properties.VariableNames, {'Var1', 'Version'});
assert_isequal(T.Version, [3; 4]);
assert_isequal(T.Var1, [1; 2]);
%=============================================================================
T = table([1; 2]);
T.data = [7; 8];
assert_isequal(T.Properties.VariableNames, {'Var1', 'data'});
assert_isequal(T.data, [7; 8]);
assert_isequal(T.Var1, [1; 2]);
%=============================================================================
TT = timetable(seconds([1; 2]), [5; 6]);
TT.Table = [1; 2];
assert_isequal(TT.Properties.VariableNames, {'Var1', 'Table'});
assert_isequal(TT.Table, [1; 2]);
assert_isequal(TT.Var1, [5; 6]);
TT.Version = [9; 9];
assert_isequal(TT.Properties.VariableNames, {'Var1', 'Table', 'Version'});
assert_isequal(TT.Version, [9; 9]);
%=============================================================================
% Properties stays reachable, for reading and writing
T = table([1; 2]);
T.Properties.VariableNames = {'a'};
assert_isequal(T.Properties.VariableNames, {'a'});
assert_isequal(T.a, [1; 2]);
T.Properties.Description = 'desc';
assert_isequal(T.Properties.Description, 'desc');
P = T.Properties;
assert_isequal(P.Description, 'desc');
TT = timetable(seconds([1; 2]), [5; 6]);
TT.Properties.VariableNames = {'b'};
assert_isequal(TT.b, [5; 6]);
%=============================================================================

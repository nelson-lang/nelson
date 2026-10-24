%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% T.name, T.name(k) and T.name = value on tables and timetables give the same
% object as the subsref and subsasgn methods called explicitly, hidden
% storage included.
%=============================================================================
function test_tabular_dot_fast_paths()
  x = [1; 2; 3];
  y = x;
  z = x;
  T = table(x, [4; 5; 6], 'VariableNames', {'Var1', 'Var2'});
  checkAssign(T, 'Var1', [7; 8; 9]);
  checkAssign(T, 'Var1', single([7; 8; 9]));
  checkAssign(T, 'Var2', int8([7; 8; 9]));
  checkAssign(T, 'Var2', [true; false; true]);
  checkAssign(T, 'Var2', ["a"; "b"; "c"]);
  checkAssign(T, 'Var2', {1; 'b'; [1 2]});
  checkAssign(T, 'Var1', []);
  checkAssign(T, 'NewVar', [7; 8; 9]);
  checkAssign(T, 'Var1', [1 2; 3 4; 5 6]);
  checkAssign(T, 'Var1', [1; 2]);
  checkAssign(T, 'Var1', 5);
  checkRead(T, 'Var1');
  checkRead(T, 'Var2');
  checkRead(T, 'Properties');
  checkRead(T, 'Unknown');
  checkIndex(T, 'Var1', {2});
  checkIndex(T, 'Var2', {[3 1], 1});
  checkIndex(T, 'Var1', {4});
  checkIndexedAssign(T, 'Var1', 3, 5);
  checkIndexedAssign(T, 'Var2', 5, 1);
  checkIndexedAssign(T, 'Var1', 2, int8(1));
  %=============================================================================
  % variable classes and matrices
  T = table([1 2; 3 4], ['ab'; 'cd'], {1; 'z'}, ["u"; "v"], [true; false]);
  checkAssign(T, 'Var1', [5 6; 7 8]);
  checkAssign(T, 'Var2', ['xy'; 'zt']);
  checkAssign(T, 'Var3', {2; 3});
  checkAssign(T, 'Var4', ["p"; "q"]);
  checkAssign(T, 'Var5', [0; 1]);
  checkIndex(T, 'Var1', {2, ':'});
  checkIndex(T, 'Var3', {1});
  checkIndex(T, 'Var2', {':'});
  %=============================================================================
  % row names, metadata and a table without rows
  T = table(x, y, 'VariableNames', {'a', 'b'}, 'RowNames', {'r1', 'r2', 'r3'});
  T.Properties.VariableUnits = {'m', 's'};
  T.Properties.Description = 'desc';
  checkAssign(T, 'a', [7; 8; 9]);
  checkAssign(T, 'b', int16([7; 8; 9]));
  T = table(zeros(0, 1), 'VariableNames', {'a'});
  checkAssign(T, 'a', zeros(0, 1));
  checkAssign(T, 'a', single(zeros(0, 1)));
  %=============================================================================
  % requested variable types not applied yet
  T = table(x, y);
  T.Properties.VariableTypes = ["single", "double"];
  checkAssign(T, 'y', [7; 8; 9]);
  checkRead(T, 'x');
  %=============================================================================
  % variables named like the hidden storage
  T = table(x, y, z, 'VariableNames', {'data', 'Version', 'Data'});
  checkAssign(T, 'data', [7; 8; 9]);
  checkAssign(T, 'Version', single([7; 8; 9]));
  checkAssign(T, 'Data', [7; 8; 9]);
  checkRead(T, 'data');
  checkRead(T, 'Version');
  checkIndex(T, 'data', {2});
  %=============================================================================
  % datetime and duration variables
  T = table(seconds(x), datetime(2020, 1, x), x);
  checkAssign(T, 'Var1', seconds([4; 5; 6]));
  checkAssign(T, 'Var3', [4; 5; 6]);
  checkRead(T, 'Var2');
  %=============================================================================
  % timetables
  TT = timetable(seconds(x), x, [4; 5; 6], 'VariableNames', {'Var1', 'Var2'});
  checkAssign(TT, 'Var1', [7; 8; 9]);
  checkAssign(TT, 'Var2', single([7; 8; 9]));
  checkAssign(TT, 'Var2', {1; 2; 3});
  checkAssign(TT, 'Var1', []);
  checkAssign(TT, 'NewVar', [7; 8; 9]);
  checkAssign(TT, 'Time', seconds([4; 5; 6]));
  checkAssign(TT, 'Var1', [1; 2]);
  checkRead(TT, 'Var1');
  checkRead(TT, 'Var2');
  checkRead(TT, 'Time');
  checkRead(TT, 'Variables');
  checkRead(TT, 'Properties');
  checkRead(TT, 'Unknown');
  checkIndex(TT, 'Var1', {2});
  checkIndex(TT, 'Var2', {[3 1]});
  checkIndex(TT, 'Var1', {5});
  checkIndexedAssign(TT, 'Var1', 3, 5);
  checkIndexedAssign(TT, 'Var2', 5, 1);
  TT = timetable(seconds(x), x, y, z, 'VariableNames', {'Table', 'Version', 'Row'});
  checkAssign(TT, 'Table', [7; 8; 9]);
  checkAssign(TT, 'Version', int32([7; 8; 9]));
  checkAssign(TT, 'Row', [7; 8; 9]);
  checkRead(TT, 'Table');
  checkRead(TT, 'Version');
  TT = timetable(seconds(x), x, 'DimensionNames', {'When', 'Data'});
  checkRead(TT, 'When');
  checkRead(TT, 'Data');
  checkAssign(TT, 'x', [7; 8; 9]);
  %=============================================================================
  % a table subclass keeps its own indexing
  ET = eventtable(seconds(x), 'EventLabels', ["a"; "b"; "c"]);
  checkRead(ET, 'EventLabels');
  checkAssign(ET, 'EventLabels', ["d"; "e"; "f"]);
  %=============================================================================
  % T.Properties = P, Description and UserData edits included
  T = table(x, y, 'RowNames', {'r1', 'r2', 'r3'});
  checkPropertiesEdit(T, 'Description', 'desc');
  checkPropertiesEdit(T, 'Description', 5);
  checkPropertiesEdit(T, 'UserData', {1, 'a'});
  checkPropertiesEdit(T, 'VariableNames', {'x', 'y'});
  checkPropertiesEdit(T, 'VariableNames', {'a', 'b'});
  checkPropertiesEdit(T, 'RowNames', {'r1'});
  checkPropertiesEdit(T, 'RowNames', {'a', 'b', 'c'});
  checkPropertiesEdit(table(x), 'Description', 'desc');
  checkPropertiesEdit(TT, 'Description', 'desc');
  checkPropertiesEdit(TT, 'UserData', 7);
  checkPropertiesEdit(TT, 'RowTimes', seconds([7; 8; 9]));
  checkPropertiesEdit(TT, 'RowTimes', seconds(1));
  checkPropertiesEdit(ET, 'Description', 'desc');
end
%=============================================================================
function checkPropertiesEdit(obj, name, value)
  % obj.Properties.(name) = value against the overloaded subsasgn, with the
  % other fields shared with the stored Properties or rebuilt
  P = obj.Properties;
  P.(name) = value;
  checkPropertiesAssign(obj, P);
  P.Description = [P.Description, ''];
  names = fieldnames(P);
  Q = struct();
  for k = 1:length(names)
    Q.(names{k}) = P.(names{k});
  end
  checkPropertiesAssign(obj, Q);
end
%=============================================================================
function checkPropertiesAssign(obj, P)
  [expected, expectedError] = tryCall(@() subsasgn(obj, substruct('.', 'Properties'), P));
  [actual, actualError] = tryCall(@() propertiesAssign(obj, P));
  assert_isequal(actualError, expectedError);
  if isempty(expectedError)
    assert_isequal(struct(actual), struct(expected));
  end
end
%=============================================================================
function obj = propertiesAssign(obj, P)
  obj.Properties = P;
end
%=============================================================================
function checkAssign(obj, name, value)
  % obj.(name) = value, dot syntax against the overloaded subsasgn
  [expected, expectedError] = tryCall(@() subsasgn(obj, substruct('.', name), value));
  [actual, actualError] = tryCall(@() dotAssign(obj, name, value));
  assert_isequal(actualError, expectedError);
  if isempty(expectedError)
    assert_isequal(class(actual), class(expected));
    assert_isequal(struct(actual), struct(expected));
  end
end
%=============================================================================
function checkIndexedAssign(obj, name, index, value)
  % obj.(name)(index) = value against the overloaded subsasgn, which
  % receives both subscripts (rows past the height grow the table)
  [expected, expectedError] = tryCall(@() subsasgn(obj, substruct('.', name, '()', {index}), value));
  [actual, actualError] = tryCall(@() dotIndexAssign(obj, name, index, value));
  assert_isequal(actualError, expectedError);
  if isempty(expectedError)
    assert_istrue(isequaln(struct(actual), struct(expected))); % NaN row times of added rows
  end
end
%=============================================================================
function checkRead(obj, name)
  [expected, expectedError] = tryCall(@() subsref(obj, substruct('.', name)));
  [actual, actualError] = tryCall(@() dotRead(obj, name));
  assert_isequal(actualError, expectedError);
  if isempty(expectedError)
    assert_isequal(actual, expected);
  end
end
%=============================================================================
function checkIndex(obj, name, indices)
  [expected, expectedError] = tryCall(@() subsref(obj, substruct('.', name, '()', indices)));
  [actual, actualError] = tryCall(@() dotIndex(obj, name, indices));
  assert_isequal(actualError, expectedError);
  if isempty(expectedError)
    assert_isequal(actual, expected);
  end
end
%=============================================================================
function [R, identifier] = tryCall(f)
  R = [];
  identifier = '';
  try
    R = f();
  catch e
    identifier = e.identifier;
  end
end
%=============================================================================
function obj = dotAssign(obj, name, value)
  switch name
    case 'Var1'
      obj.Var1 = value;
    case 'Var2'
      obj.Var2 = value;
    case 'Var3'
      obj.Var3 = value;
    case 'Var4'
      obj.Var4 = value;
    case 'Var5'
      obj.Var5 = value;
    case 'NewVar'
      obj.NewVar = value;
    case 'a'
      obj.a = value;
    case 'b'
      obj.b = value;
    case 'x'
      obj.x = value;
    case 'y'
      obj.y = value;
    case 'data'
      obj.data = value;
    case 'Data'
      obj.Data = value;
    case 'Version'
      obj.Version = value;
    case 'Table'
      obj.Table = value;
    case 'Row'
      obj.Row = value;
    case 'Time'
      obj.Time = value;
    case 'EventLabels'
      obj.EventLabels = value;
    otherwise
      obj.(name) = value;
  end
end
%=============================================================================
function obj = dotIndexAssign(obj, name, index, value)
  switch name
    case 'Var1'
      obj.Var1(index) = value;
    case 'Var2'
      obj.Var2(index) = value;
    otherwise
      obj.(name)(index) = value;
  end
end
%=============================================================================
function R = dotRead(obj, name)
  switch name
    case 'Var1'
      R = obj.Var1;
    case 'Var2'
      R = obj.Var2;
    case 'x'
      R = obj.x;
    case 'data'
      R = obj.data;
    case 'Version'
      R = obj.Version;
    case 'Table'
      R = obj.Table;
    case 'Time'
      R = obj.Time;
    case 'When'
      R = obj.When;
    case 'Data'
      R = obj.Data;
    case 'Variables'
      R = obj.Variables;
    case 'Properties'
      R = obj.Properties;
    case 'Unknown'
      R = obj.Unknown;
    case 'EventLabels'
      R = obj.EventLabels;
    otherwise
      R = obj.(name);
  end
end
%=============================================================================
function R = dotIndex(obj, name, indices)
  switch name
    case 'Var1'
      R = obj.Var1(indices{:});
    case 'Var2'
      R = obj.Var2(indices{:});
    case 'Var3'
      R = obj.Var3(indices{:});
    case 'data'
      R = obj.data(indices{:});
    otherwise
      R = obj.(name)(indices{:});
  end
end
%=============================================================================

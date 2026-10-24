%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% An array of flat objects is read from the columns decoded by
% __jsondecode_columns__; a VariableUnitsSelector pointing nowhere forces the
% generic jsondecode path: both must give the same table.
%=============================================================================
function filename = writeJsonFile(txt)
  filename = [tempname(), '.json'];
  fid = fopen(filename, 'w', 'n', 'UTF-8');
  fprintf(fid, '%s', txt);
  fclose(fid);
end
%=============================================================================
function checkSamePaths(filename, varargin)
  T = readtable(filename, varargin{:});
  R = readtable(filename, varargin{:}, 'VariableUnitsSelector', '/noUnits');
  asserts.istrue(isequaln(T, R));
  asserts.isequal(T.Properties.VariableNames, R.Properties.VariableNames);
  for k = 1:width(T)
    name = T.Properties.VariableNames{k};
    asserts.isequal(class(T.(name)), class(R.(name)));
    if isnumeric(T.(name))
      asserts.isequal(1 ./ T.(name), 1 ./ R.(name));
    end
  end
end
%=============================================================================
documents = {
  '[{"id": 1, "name": "a", "value": 1.5, "flag": true}, {"id": 2, "name": "b", "value": -2, "flag": false}]'
  '[{"id": 1, "name": "a"}, {"name": "b", "id": 2}]'
  '[{"id": 1, "name": "a"}, {"id": 2}, {"id": 3, "extra": "z"}]'
  '[{"x": null, "t": null}, {"x": 2, "t": "b"}, {"x": 3, "t": null}]'
  '[{"x": true, "y": 1}, {"x": 2, "y": false}, {"x": 0, "y": true}]'
  '[{"x": true}, {"x": false}, {"x": null}]'
  '[{"x": 1}, {"x": "two"}, {"x": true}]'
  '[{"a b": 1, "a_b": 2, "1x": 3, "_y": 4}, {"a b": 5, "a_b": 6, "1x": 7, "_y": 8}]'
  '[{"k": 1, "k": 2}, {"k": 3, "k": 4}]'
  '[{"s": "caf\u00e9 \"quoted\" \\ /"}, {"s": ""}, {"s": "\u65e5\u672c"}]'
  '[{"n": 12345678901234567890, "e": 1e-300}, {"n": -0, "e": 3.5E+2}]'
  '[{"n": -0.0, "e": -0}, {"n": 0, "e": 1}]'
  };
for k = 1:numel(documents)
  filename = writeJsonFile(documents{k});
  checkSamePaths(filename);
  checkSamePaths(filename, 'TextType', 'char');
  checkSamePaths(filename, 'MissingRule', 'omitrow');
  checkSamePaths(filename, 'ImportErrorRule', 'omitvar');
  opts = detectImportOptions(filename);
  R = detectImportOptions(filename, 'VariableUnitsSelector', '/noUnits');
  asserts.isequal(opts.VariableNames, R.VariableNames);
  asserts.isequal(opts.VariableTypes, R.VariableTypes);
  asserts.isequal(opts.VariableSelectors, R.VariableSelectors);
  asserts.isequal(opts.TableSelector, R.TableSelector);
end
%=============================================================================
filename = writeJsonFile('[{"id": 3, "name": "c", "flag": true}, {"id": 4, "name": "d", "flag": false}]');
T = readtable(filename);
asserts.isequal(T.id, [3; 4]);
asserts.isequal(T.name, ["c"; "d"]);
asserts.isequal(T.flag, [true; false]);
checkSamePaths(filename, 'RowNamesSelector', '/name');
checkSamePaths(filename, 'VariableSelectors', {'/flag', '/missing', '/id'});
checkSamePaths(filename, 'VariableSelectors', {'Keys', '/name'});
%=============================================================================
% import options: selected variables and types
opts = detectImportOptions(filename);
opts.SelectedVariableNames = {'name', 'id'};
opts.VariableTypes = {'int32', 'categorical', 'double'};
T = readtable(filename, opts);
opts.VariableUnitsSelector = '/noUnits';
R = readtable(filename, opts);
asserts.istrue(isequaln(T, R));
asserts.isequal(class(T.id), 'int32');
asserts.isequal(class(T.name), 'categorical');
opts = detectImportOptions(filename);
opts.VariableTypes = {'string', 'char', 'logical'};
T = readtable(filename, opts);
opts.VariableUnitsSelector = '/noUnits';
R = readtable(filename, opts);
asserts.istrue(isequaln(T, R));
asserts.isequal(T.name, {'c'; 'd'});
%=============================================================================
% text converted to numbers
filename = writeJsonFile('[{"v": "1.5"}, {"v": "x"}, {"v": "NaN"}]');
opts = detectImportOptions(filename);
opts.VariableTypes = {'double'};
T = readtable(filename, opts);
opts.VariableUnitsSelector = '/noUnits';
R = readtable(filename, opts);
asserts.istrue(isequaln(T, R));
asserts.isequal(T.v, [1.5; NaN; NaN]);
opts.ImportErrorRule = 'omitrow';
opts.VariableUnitsSelector = '';
T = readtable(filename, opts);
asserts.isequal(T.v, [1.5; NaN]);
%=============================================================================
% relative file name
[folder, name, ext] = fileparts(filename);
previous = cd(folder);
T = readtable([name, ext]);
cd(previous);
asserts.isequal(T.v, ["1.5"; "x"; "NaN"]);
%=============================================================================
% readtimetable
filename = writeJsonFile('[{"t": 1, "v": 10}, {"t": 2, "v": 20}]');
TT = readtimetable(filename, 'RowTimes', seconds([1; 2]));
RT = readtimetable(filename, 'RowTimes', seconds([1; 2]), 'VariableUnitsSelector', '/noUnits');
asserts.istrue(isequaln(TT, RT));
%=============================================================================
% table node inside an object: automatic search, TableNodeName, TableSelector
rows = '[{"id": 1, "name": "a"}, {"id": 2, "name": "b"}, {"id": 3, "name": null}]';
documents = {
  ['{"meta": {"n": 3, "tags": [1, 2]}, "data": ', rows, '}']
  ['{"a": {"b": 1, "c": {"my rows": ', rows, '}}, "z": [{"q": 1}, {"q": 2}]}']
  ['{"mixed": [{"x": 1}, 2], "empty": [], "data": ', rows, '}']
  ['{"single": [{"x": 1}], "data": ', rows, '}']
  ['{"nested": [[{"x": 1}], [{"x": 2}]], "data": ', rows, '}']
  ['{"k": 1, "k": {"rows": ', rows, '}}']
  '{"id": 1, "name": "flat"}'
  };
for k = 1:numel(documents)
  filename = writeJsonFile(documents{k});
  checkSamePaths(filename);
  checkSamePaths(filename, 'TextType', 'char');
  opts = detectImportOptions(filename);
  R = detectImportOptions(filename, 'VariableUnitsSelector', '/noUnits');
  asserts.isequal(opts.TableSelector, R.TableSelector);
  asserts.isequal(opts.VariableNames, R.VariableNames);
  asserts.isequal(opts.VariableTypes, R.VariableTypes);
end
filename = writeJsonFile(documents{2});
T = readtable(filename);
asserts.isequal(T.id, [1; 2; 3]);
opts = detectImportOptions(filename);
asserts.isequal(opts.TableSelector, '/a/c/my_rows');
T = readtable(filename, opts);
asserts.isequal(T.id, [1; 2; 3]);
for args = {{'TableNodeName', 'my rows'}, {'TableNodeName', 'z'}, {'TableNodeName', 'c'}, ...
    {'TableSelector', '/a/c/my rows'}, {'TableSelector', '/a/c/my_rows'}, {'TableSelector', '/z'}, ...
    {'TableSelector', ''}, {'TableSelector', '/a/c'}, {'TableSelector', '/z/0'}}
  checkSamePaths(filename, args{1}{:});
  opts = detectImportOptions(filename, args{1}{:});
  R = detectImportOptions(filename, args{1}{:}, 'VariableUnitsSelector', '/noUnits');
  asserts.isequal(opts.TableSelector, R.TableSelector);
  asserts.isequal(opts.VariableNames, R.VariableNames);
end
T = readtable(filename, 'TableSelector', '/a/c/my rows');
asserts.isequal(T.id, [1; 2; 3]);
asserts.checkerror('readtable(filename, ''TableNodeName'', ''nothing'')', 'JSON key not found: nothing');
asserts.checkerror('readtable(filename, ''TableSelector'', ''/a/nothing'')', 'JSON Pointer not found: /a/nothing');
%=============================================================================
% documents left to jsondecode: nested values, lenient JSON, not an array
filename = writeJsonFile('[{"a": 1, "b": [1, 2]}, {"a": 2, "b": [3, 4]}]');
T = readtable(filename);
asserts.isequal(T.b, [1 2; 3 4]);
filename = writeJsonFile('[{"a": 1, "b": NaN}, {"a": 2, "b": Infinity}]');
T = readtable(filename, 'AllowInfAndNaN', true);
asserts.isequal(T.b, [NaN; Inf]);
filename = writeJsonFile(sprintf('[{"a": 1}, // comment\n{"a": 2},]'));
T = readtable(filename, 'AllowComments', true, 'AllowTrailingCommas', true);
asserts.isequal(T.a, [1; 2]);
asserts.isequal(__jsondecode_columns__(filename), false);
%=============================================================================

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
function filename = writeJsonFile(txt)
  filename = [tempname(), '.json'];
  fid = fopen(filename, 'w', 'n', 'UTF-8');
  fprintf(fid, '%s', txt);
  fclose(fid);
end
%=============================================================================
students = ['[', ...
  '{"ID": "S11305", "Name": {"FirstName": "Priya", "LastName": "Thompson"}, "Age": 18.0, ', ...
  '"Address": {"Street": "591 Spring Lane", "City": "Natick", "State": "MA"}},', ...
  '{"ID": "S23451", "Name": {"FirstName": "Conor", "LastName": "Cole"}, "Age": 18.0, ', ...
  '"Address": {"Street": "4641 Pearl Street", "City": "San Francisco", "State": "CA"}},', ...
  '{"ID": "S51234", "Name": {"FirstName": "Morgan", "LastName": "Green"}, "Age": 19.0, ', ...
  '"Address": {"Street": "30 Highland Road", "City": "Detroit", "State": "MI"}}]'];
filename = writeJsonFile(students);
%=============================================================================
% array of objects: one row per object, nested objects flattened
T = readtable(filename);
asserts.isequal(T.Properties.VariableNames, {'ID', 'FirstName', 'LastName', 'Age', 'Street', 'City', 'State'});
asserts.isequal(T.ID, ["S11305"; "S23451"; "S51234"]);
asserts.isequal(T.Age, [18; 18; 19]);
asserts.isequal(T.City, ["Natick"; "San Francisco"; "Detroit"]);
%=============================================================================
% documented example: detectImportOptions with VariableSelectors
opts = detectImportOptions(filename, 'VariableSelectors', '/Address/Street');
asserts.isequal(class(opts), 'nelson.io.json.JSONImportOptions');
asserts.isequal(opts.VariableNames, {'Street'});
asserts.isequal(opts.VariableTypes, {'string'});
T = readtable(filename, opts);
asserts.isequal(size(T), [3 1]);
asserts.isequal(T.Street, ["591 Spring Lane"; "4641 Pearl Street"; "30 Highland Road"]);
%=============================================================================
% documented example: jsonImportOptions with VariableSelectors and TableSelector
opts = jsonImportOptions('VariableSelectors', "/Name/FirstName", 'TableSelector', "");
T = readtable(filename, opts);
asserts.isequal(T.Properties.VariableNames, {'Var1'});
asserts.isequal(T.Var1, {'Priya'; 'Conor'; 'Morgan'});
%=============================================================================
% several selectors, FileType on a file without .json extension
txtname = [tempname(), '.txt'];
copyfile(filename, txtname);
T = readtable(txtname, 'FileType', 'json', 'VariableSelectors', ["/ID", "/Name/LastName"]);
asserts.isequal(T.Properties.VariableNames, {'ID', 'LastName'});
asserts.isequal(T.LastName, ["Thompson"; "Cole"; "Green"]);
T = readtable(filename, 'TextType', 'char', 'VariableSelectors', "/ID");
asserts.isequal(T.ID, {'S11305'; 'S23451'; 'S51234'});
%=============================================================================
% RowNamesSelector
T = readtable(filename, 'RowNamesSelector', '/ID', 'VariableSelectors', "/Age");
asserts.isequal(T.Properties.RowNames, {'S11305'; 'S23451'; 'S51234'});
asserts.isequal(T.Age, [18; 18; 19]);
%=============================================================================
% type detection, null and absent keys are missing values
filename = writeJsonFile('[{"a": 1, "b": "x", "c": true}, {"a": null, "b": null, "c": false}, {"b": "z", "c": true}]');
T = readtable(filename);
asserts.isequal(T.Properties.VariableNames, {'a', 'b', 'c'});
asserts.isequal(T.a, [1; NaN; NaN]);
asserts.isequal(T.b, ["x"; string(missing); "z"]);
asserts.isequal(T.c, [true; false; true]);
T = readtable(filename, 'MissingRule', 'omitrow');
asserts.isequal(T.b, "x");
T = readtable(filename, 'MissingRule', 'omitvar');
asserts.isequal(T.Properties.VariableNames, {'c'});
asserts.checkerror('readtable(filename, ''MissingRule'', ''error'')', _('Unable to import variable ''a'': missing value found.'));
asserts.checkerror('readtable(filename, ''MissingRule'', ''error'')', _('Unable to import variable ''a'': missing value found.'), 'Nelson:spreadsheet:jsonImportRule');
%=============================================================================
% repeated nodes (JSON arrays)
filename = writeJsonFile('[{"id": 1, "v": [1, 2]}, {"id": 2, "v": [3, 4]}]');
T = readtable(filename);
asserts.isequal(T.v, [1 2; 3 4]);
T = readtable(filename, 'RepeatedNodeRule', 'ignore');
asserts.isequal(T.v, [1; 3]);
asserts.checkerror('readtable(filename, ''RepeatedNodeRule'', ''error'')', _('Repeated JSON nodes found (RepeatedNodeRule is ''error'').'));
%=============================================================================
% table inside an object: automatic detection, TableSelector and TableNodeName
txt = '{"meta": {"units": {"temp": "degC", "rpm": "1/min"}}, "records": [{"temp": 20.5, "rpm": 900}, {"temp": 21, "rpm": 950}]}';
filename = writeJsonFile(txt);
T = readtable(filename);
asserts.isequal(T.temp, [20.5; 21]);
asserts.isequal(T.rpm, [900; 950]);
T = readtable(filename, 'TableSelector', '/records', 'VariableUnitsSelector', '/meta/units');
asserts.isequal(T.Properties.VariableUnits, {'degC', '1/min'});
T = readtable(filename, 'TableNodeName', 'records');
asserts.isequal(height(T), 2);
T = readtable(filename, 'TableSelector', '/records', 'VariableSelectors', "/temp", ...
  'RepeatedNodeRule', 'addcol', 'ParsingMode', 'lenient', 'MissingRule', 'fill', ...
  'ImportErrorRule', 'fill', 'TextType', 'string', 'VariableNamingRule', 'preserve');
asserts.isequal(T.temp, [20.5; 21]);
opts = detectImportOptions(filename);
asserts.isequal(opts.TableSelector, '/records');
asserts.isequal(opts.VariableSelectors, {'/temp', '/rpm'});
asserts.isequal(opts.VariableTypes, {'double', 'double'});
%=============================================================================
% object of objects: one row per key, keys read with "Keys"
filename = writeJsonFile('{"engine1": {"temp": 80, "ok": true}, "engine2": {"temp": 95, "ok": false}}');
T = readtable(filename, 'VariableSelectors', ["Keys", "/temp"]);
asserts.isequal(T.Properties.VariableNames, {'Keys', 'temp'});
asserts.isequal(T.Keys, ["engine1"; "engine2"]);
asserts.isequal(T.temp, [80; 95]);
%=============================================================================
% lenient parsing (default) and strict parsing
txt = sprintf('[\n// comment\n{"a": 1, "b": NaN},\n/* block */ {"a": Infinity, "b": -Inf},\n]');
filename = writeJsonFile(txt);
T = readtable(filename);
asserts.isequal(T.a, [1; Inf]);
asserts.isequal(T.b, [NaN; -Inf]);
asserts.checkerror('readtable(filename, ''ParsingMode'', ''strict'')', _('Comments are not allowed in the JSON file (AllowComments is false).'));
asserts.checkerror('readtable(filename, ''AllowTrailingCommas'', false)', _('Trailing commas are not allowed in the JSON file (AllowTrailingCommas is false).'));
asserts.checkerror('readtable(filename, ''AllowInfAndNaN'', false)', _('Inf and NaN values are not allowed in the JSON file (AllowInfAndNaN is false).'));
filename = writeJsonFile('[{"url": "http://a.b/c", "s": "NaN, // not a comment,]"}]');
T = readtable(filename, 'ParsingMode', 'strict');
asserts.isequal(T.url, "http://a.b/c");
asserts.isequal(T.s, "NaN, // not a comment,]");
%=============================================================================
% date and time text becomes datetime
filename = writeJsonFile('[{"t": "2024-01-01 10:00:00", "x": 1}, {"t": "2024-01-02 10:00:00", "x": 2}]');
T = readtable(filename);
asserts.isequal(class(T.t), 'datetime');
asserts.isequal(datenum(T.t), datenum([2024 1 1 10 0 0; 2024 1 2 10 0 0]));
%=============================================================================
asserts.checkerror('readtable(filename, ''Sheet'', 1)', _('Property name not supported.'));
asserts.checkerror('readtable(filename, ''Sheet'', 1)', _('Property name not supported.'), 'Nelson:spreadsheet:propertyNotSupported');
%=============================================================================

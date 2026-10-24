%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function filename = writeJsonFile(txt)
  filename = [tempname(), '.json'];
  fid = fopen(filename, 'w', 'n', 'UTF-8');
  fprintf(fid, '%s', txt);
  fclose(fid);
end
%=============================================================================
filename = writeJsonFile(['[{"n": 1, "b": true, "s": "a", "m": 1, "z": null}, ', ...
  '{"n": 2.5, "b": false, "s": "", "m": "x", "z": null, "late": 7}, ', ...
  '{"b": true, "n": -3, "s": "\u00e9", "m": false, "z": null}]']);
[ok, names, columns] = __jsondecode_columns__(filename);
asserts.istrue(ok);
asserts.isequal(names, {'n', 'b', 's', 'm', 'z', 'late'});
asserts.isequal(columns{1}, [1; 2.5; -3]);
asserts.isequal(columns{2}, [true; false; true]);
asserts.isequal(columns{3}, ["a"; ""; char(233)]);
asserts.isequal(columns{4}, {1; 'x'; false});
asserts.isequal(columns{5}, {[]; []; []});
asserts.isequal(columns{6}, {[]; 7; []});
s = jsondecode(fileread(filename));
asserts.isequal(s{2}.m, columns{4}{2});
%=============================================================================
% numbers and booleans mixed: double column
[ok, names, columns] = __jsondecode_columns__(writeJsonFile('[{"x": true}, {"x": 2}]'));
asserts.istrue(ok);
asserts.isequal(columns{1}, [1; 2]);
%=============================================================================
% field names as given by jsondecode
filename = writeJsonFile('[{"a b": 1, "a_b": 2, "a b": 3, "1x": 4}, {"a b": 5, "a_b": 6, "a b": 7, "1x": 8}]');
[ok, names, columns] = __jsondecode_columns__(filename);
asserts.istrue(ok);
asserts.isequal(names, fieldnames(jsondecode(fileread(filename)))');
asserts.isequal(columns{3}, [3; 7]);
%=============================================================================
% table node inside an object
rows = '[{"id": 1}, {"id": -0.0}]';
filename = writeJsonFile(['{"meta": {"n": 2, "tag": "t"}, "x y": {"data": ', rows, '}}']);
[ok, names, columns, selector] = __jsondecode_columns__(filename, 'auto');
asserts.istrue(ok);
asserts.isequal(selector, '/x_y/data');
asserts.isequal(names, {'id'});
s = jsondecode(fileread(filename));
asserts.isequal(1 ./ columns{1}, 1 ./ [s.x_y.data.id]');
[ok, names, columns, selector] = __jsondecode_columns__(filename, 'name', 'data');
asserts.istrue(ok);
asserts.isequal(selector, '/x_y/data');
[ok, names, columns, selector] = __jsondecode_columns__(filename, 'pointer', {'x y', 'data'});
asserts.istrue(ok);
asserts.isequal(selector, '/x_y/data');
[ok, names, columns, selector] = __jsondecode_columns__(filename, 'pointer', {'meta'});
asserts.istrue(ok);
asserts.isequal(size(columns{1}), [1 1]);
asserts.isequal(columns{2}, "t");
%=============================================================================
% other documents are left to jsondecode
documents = {'[]', '[1, 2]', '[{"a": 1}, 2]', '[{}]', '[{"a": [1]}]', ...
  '[{"a": {"b": 1}}]', '[{"a": NaN}]', '[{"a": 1},]', '[{"a": 1}] x', '[{"a": 1}', ''};
for k = 1:numel(documents)
  [ok, names, columns] = __jsondecode_columns__(writeJsonFile(documents{k}));
  asserts.isfalse(ok);
  asserts.istrue(isempty(names));
  asserts.istrue(isempty(columns));
end
asserts.isfalse(__jsondecode_columns__([tempname(), '.json']));
% a flat object alone is a single row
[ok, names, columns] = __jsondecode_columns__(writeJsonFile('{"a": 1, "b": "x"}'));
asserts.istrue(ok);
asserts.isequal(columns, {1, "x"});
documents = {'{"one": [{"data": [{"a": 1}, {"a": 2}]}]}', '{"a": {"b": 1}}', '[{"a": 1}]'};
for k = 1:numel(documents)
  asserts.isfalse(__jsondecode_columns__(writeJsonFile(documents{k}), 'name', 'data'));
end
asserts.isfalse(__jsondecode_columns__(writeJsonFile('{"one": [{"a": 1}], "data": [{"a": 1}, {"a": 2}]}'), 'auto'));
asserts.isfalse(__jsondecode_columns__(writeJsonFile('{"n": [[{"a": 1}], [{"a": 2}]], "data": [{"a": 1}, {"a": 2}]}'), 'auto'));
asserts.isfalse(__jsondecode_columns__(writeJsonFile('{"a": [{"b": 1}, {"b": 2}]}'), 'pointer', {'a', '0'}));
% the integer -0 is read as 0 by the parser used here
asserts.isfalse(__jsondecode_columns__(writeJsonFile('[{"a": 1}, {"a": -0}]')));
asserts.istrue(__jsondecode_columns__(writeJsonFile('[{"a": -0.5}, {"a": "2024-01-05"}]')));
asserts.checkerror('__jsondecode_columns__(filename, ''everywhere'')', 'Wrong value for #2 argument.');
%=============================================================================

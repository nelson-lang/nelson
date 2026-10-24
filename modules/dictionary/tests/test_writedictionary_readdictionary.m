%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempname(), '.json'];
%=============================================================================
d = dictionary(string({'a', 'b'}), [1 2]);
writedictionary(d, filename);
asserts.istrue(isfile(filename));
txt = fileread(filename);
asserts.istrue(contains(txt, '"a"'));
asserts.istrue(contains(txt, '"b"'));
%=============================================================================
R = readdictionary(filename);
asserts.isequal(class(R), 'dictionary');
asserts.isequal(keys(R), ["a"; "b"]);
asserts.isequal(values(R), [1; 2]);
[keyType, valueType] = types(R);
asserts.isequal(keyType, "string");
asserts.isequal(valueType, "double");
%=============================================================================
R = readdictionary(filename, 'ValueType', 'single');
asserts.isequal(class(values(R)), 'single');
%=============================================================================
d = dictionary([1 2], [3 4]);
writedictionary(d, filename, 'PrettyPrint', false);
txt = fileread(filename);
asserts.isfalse(contains(txt, newline()));
asserts.istrue(contains(txt, '"1":3'));
R = readdictionary(filename);
asserts.isequal(keys(R), ["1"; "2"]);
asserts.isequal(values(R), [3; 4]);
[keyType, valueType] = types(R);
asserts.isequal(keyType, "string");
asserts.isequal(valueType, "double");
%=============================================================================
d = dictionary([true false], [10 20]);
writedictionary(d, filename);
txt = fileread(filename);
asserts.istrue(contains(txt, '"true"'));
asserts.istrue(contains(txt, '"false"'));
R = readdictionary(filename);
asserts.isequal(keys(R), ["true"; "false"]);
asserts.isequal(values(R), [10; 20]);
%=============================================================================
d = dictionary(string({'yes', 'no'}), [true false]);
writedictionary(d, filename);
R = readdictionary(filename);
asserts.isequal(values(R), [true; false]);
asserts.isequal(class(values(R)), 'logical');
%=============================================================================
d = dictionary(string({'quote"key', 'plain'}), string({'value"one', 'two'}));
writedictionary(d, filename);
txt = fileread(filename);
asserts.istrue(contains(txt, 'quote\"key'));
R = readdictionary(filename);
asserts.isequal(lookup(R, "quote""key"), "value""one");
asserts.isequal(lookup(R, "plain"), "two");
%=============================================================================
d = dictionary(string({'nan', 'posinf', 'neginf'}), [NaN Inf -Inf]);
writedictionary(d, filename);
txt = fileread(filename);
asserts.istrue(contains(txt, '"nan": NaN'));
asserts.istrue(contains(txt, '"posinf": Infinity'));
asserts.istrue(contains(txt, '"neginf": -Infinity'));
R = readdictionary(filename);
asserts.istrue(isnan(R("nan")));
asserts.isequal(R("posinf"), Inf);
asserts.isequal(R("neginf"), -Inf);
writedictionary(d, filename, 'PreserveInfAndNaN', false);
txt = fileread(filename);
asserts.istrue(contains(txt, '"nan": null'));
%=============================================================================
d = dictionary(["vector", "data"], {[1 NaN Inf]});
writedictionary(d, filename, 'PrettyPrint', false);
R = readdictionary(filename);
asserts.istrue(iscell(R{"vector"}));
asserts.isequal(numel(R{"vector"}), 3);
asserts.isequal(R{"vector"}{1}, 1);
asserts.istrue(isnan(R{"vector"}{2}));
asserts.isequal(R{"vector"}{3}, Inf);
asserts.isequal(d{"vector"}, [1 NaN Inf]);
%=============================================================================
d = dictionary(string({'array', 'matrix', 'cell'}), {[1 NaN Inf -Inf], [1 NaN; Inf -Inf], {NaN, [Inf -Inf]}});
writedictionary(d, filename, 'PrettyPrint', false);
txt = fileread(filename);
asserts.istrue(contains(txt, '"array":[1,NaN,Infinity,-Infinity]'));
asserts.istrue(contains(txt, '"matrix":[[1,NaN],[Infinity,-Infinity]]'));
R = readdictionary(filename);
asserts.istrue(iscell(R{"array"}));
asserts.isequal(R{"array"}{1}, 1);
asserts.istrue(isnan(R{"array"}{2}));
asserts.isequal(R{"array"}{3}, Inf);
asserts.isequal(R{"array"}{4}, -Inf);
asserts.istrue(iscell(R{"matrix"}));
asserts.isequal(R{"matrix"}{1}{1}, 1);
asserts.istrue(isnan(R{"matrix"}{1}{2}));
asserts.isequal(R{"matrix"}{2}, {Inf; -Inf});
asserts.istrue(iscell(R{"cell"}));
asserts.istrue(isnan(R{"cell"}{1}));
asserts.isequal(R{"cell"}{2}, {Inf; -Inf});
%=============================================================================
d = configureDictionary("string", "double");
writedictionary(d, filename, 'PrettyPrint', false);
asserts.isequal(fileread(filename), '{}');
R = readdictionary(filename);
asserts.isequal(class(R), 'dictionary');
asserts.isfalse(isConfigured(R));
%=============================================================================
d = dictionary(string({'scalar', 'vector'}), {1, [2 3]});
writedictionary(d, filename);
txt = fileread(filename);
asserts.istrue(contains(txt, '"scalar": 1'));
asserts.istrue(contains(txt, '"vector": [2,3]'));
R = readdictionary(filename);
asserts.isequal(keys(R), ["scalar"; "vector"]);
asserts.isequal(R{"scalar"}, 1);
asserts.isequal(R{"vector"}, {2; 3});
[keyType, valueType] = types(R);
asserts.isequal(keyType, "string");
asserts.isequal(valueType, "cell");
%=============================================================================
filewrite(filename, '{"a": 1, "b": [2, 3], "c": [4]}');
R = readdictionary(filename);
asserts.isequal(R{"a"}, 1);
asserts.isequal(R{"b"}, {2; 3});
asserts.isequal(R{"c"}, {4});
[keyType, valueType] = types(R);
asserts.isequal(keyType, "string");
asserts.isequal(valueType, "cell");
%=============================================================================
jsonObjectText = '{"scalar": 1, "object": {"nested": [NaN, Infinity], "text": "ok"}, "objects": [{"x": -Infinity}, {"x": 3}]}';
filewrite(filename, jsonObjectText);
R = readdictionary(filename);
asserts.isequal(R{"scalar"}, 1);
asserts.istrue(isnan(R{"object"}.nested{1}));
asserts.isequal(R{"object"}.nested{2}, Inf);
asserts.isequal(R{"object"}.text, "ok");
asserts.istrue(iscell(R{"objects"}));
asserts.isequal(R{"objects"}{1}.x, -Inf);
asserts.isequal(R{"objects"}{2}.x, 3);
[keyType, valueType] = types(R);
asserts.isequal(keyType, "string");
asserts.isequal(valueType, "cell");
asserts.checkerror('readdictionary(filename, "AllowInfAndNaN", false)', _('JSON object expected.'));
asserts.checkerror('readdictionary(filename, "AllowInfAndNaN", false)', _('JSON object expected.'), 'Nelson:readdictionary:jsonObjectExpected');
%=============================================================================
s.payload = [NaN Inf];
s.label = "ok";
items(1).x = -Inf;
items(2).x = 3;
d = dictionary(string({'object', 'objects'}), {s, items});
writedictionary(d, filename, 'PrettyPrint', false);
txt = fileread(filename);
asserts.istrue(contains(txt, '"object":{"payload":[NaN,Infinity],"label":"ok"}'));
asserts.istrue(contains(txt, '"objects":[{"x":-Infinity},{"x":3}]'));
R = readdictionary(filename);
asserts.istrue(isnan(R{"object"}.payload{1}));
asserts.isequal(R{"object"}.payload{2}, Inf);
asserts.isequal(R{"object"}.label, "ok");
asserts.istrue(iscell(R{"objects"}));
asserts.isequal(R{"objects"}{1}.x, -Inf);
asserts.isequal(R{"objects"}{2}.x, 3);
%=============================================================================
asserts.checkerror('writedictionary(dictionary(), filename)', _('Unable to write a dictionary when the key and value types are not set.'));
asserts.checkerror('writedictionary(dictionary(), filename)', _('Unable to write a dictionary when the key and value types are not set.'), 'Nelson:writedictionary:typesNotSet');
asserts.checkerror('writedictionary(dictionary("a", 1), filename, "FileType", "xml")', _('Only JSON dictionary files are supported.'));
asserts.checkerror('writedictionary(dictionary("a", 1), filename, "FileType", "xml")', _('Only JSON dictionary files are supported.'), 'Nelson:dictionary:onlyJsonSupported');
asserts.checkerror('readdictionary(filename, "FileType", "xml")', _('Only JSON dictionary files are supported.'));
asserts.checkerror('readdictionary(filename, "FileType", "xml")', _('Only JSON dictionary files are supported.'), 'Nelson:dictionary:onlyJsonSupported');
asserts.checkerror('readdictionary(filename, "DictionaryNodeName", "root")', _('DictionaryNodeName and DictionarySelector are not supported for JSON dictionary files.'));
asserts.checkerror('readdictionary(filename, "DictionaryNodeName", "root")', _('DictionaryNodeName and DictionarySelector are not supported for JSON dictionary files.'), 'Nelson:readdictionary:optionNotSupportedForJson');
%=============================================================================
filewrite(filename, '{"a": 1,}');
R = readdictionary(filename);
asserts.isequal(R("a"), 1);
asserts.checkerror('readdictionary(filename, "AllowTrailingCommas", false)', _('JSON object expected.'));
filewrite(filename, sprintf('{\n // keep a\n "a": 1,\n /* keep b */ "b": 2\n}'));
R = readdictionary(filename);
asserts.isequal(values(R), [1; 2]);
asserts.checkerror('readdictionary(filename, "AllowComments", false)', _('JSON string expected.'));
%=============================================================================
rmfile(filename);
%=============================================================================

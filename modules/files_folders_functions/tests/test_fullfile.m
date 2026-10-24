%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('fullfile'), -1);
asserts.isequal(nargout('fullfile'), 1);
%=============================================================================
R = fullfile([]);
REF = '';
asserts.isequal(R, REF);
%=============================================================================
R = fullfile({});
REF = {};
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('');
REF = '';
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('.');
REF = '.';
asserts.isequal(R, REF);
%=============================================================================
R = fullfile("rr");
REF = "rr";
asserts.isequal(R, REF);
%=============================================================================
R = fullfile(["rr", string(NaN)]);
REF = ["rr", ""];
asserts.isequal(R, REF);
%=============================================================================
R = fullfile(string(NaN));
REF = "";
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('./');
if ispc()
  REF = '.\';
else
  REF = './';
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('/a/./');
if ispc()
  REF = '\a\';
else
  REF = '/a/';
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile(nelsonroot, 'toolbox', 'nelson', 'general', 'Contents.m');
REF = fullfile(nelsonroot, 'toolbox', 'nelson', 'general', 'Contents.m');
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', {'data', 'data2'}, 'fff');
if ispc()
  REF = {'\home\username\data\fff', '\home\username\data2\fff'};
else
  REF = {'/home/username/data/fff', '/home/username/data2/fff'};
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', {'data'; 'data2'}, 'fff');
if ispc()
  REF = {'\home\username\data\fff'; '\home\username\data2\fff'};
else
  REF = {'/home/username/data/fff'; '/home/username/data2/fff'};
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', {'data1', 'data2'; 'data3', 'data4'}, 'fff');
if ispc()
  REF = {'\home\username\data1\fff', '\home\username\data2\fff'; '\home\username\data3\fff', '\home\username\data4\fff'};
else
  REF = {'/home/username/data1/fff', '/home/username/data2/fff'; '/home/username/data3/fff', '/home/username/data4/fff'};
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('\\\\home/username', {'data1', 'data2'; 'data3', 'data4'}, 'fff');
if ispc()
  REF = {'\\home\username\data1\fff', '\\home\username\data2\fff'; '\\home\username\data3\fff', '\\home\username\data4\fff'};
else
  REF = {'/home/username/data1/fff', '/home/username/data2/fff'; '/home/username/data3/fff', '/home/username/data4/fff'};
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile({'\\\/B:\//', 'A://c', '\\\C:/g/h/i/j\/'});
if ispc()
  REF = {'\\B:\', 'A:\c', '\\C:\g\h\i\j\'};
else
  REF = {'/B:/', 'A:/c', '/C:/g/h/i/j/'};
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('\/\/\//A:/\/\', 'x/', '/', '/', 'y', '/', '/');
if ispc()
  REF = '\A:\x\y\';
else
  REF = '/A:/x/y/';
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', {'data'; 'data2'});
if ispc()
  REF = {'\home\username\data'; '\home\username\data2'};
else
  REF = {'/home/username/data'; '/home/username/data2'};
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile({'data', 'data2'}, '/home/username');
if ispc()
  REF = {'data\home\username', 'data2\home\username'};
else
  REF = {'data/home/username', 'data2/home/username'};
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile({'data'; 'data2'}, '/home/username');
if ispc()
  REF = {'data\home\username'; 'data2\home\username'};
else
  REF = {'data/home/username'; 'data2/home/username'};
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', 'data', {'f1.csv', 'f2.csv', 'f3.csv'});
if ispc()
  REF = {'\home\username\data\f1.csv', '\home\username\data\f2.csv', '\home\username\data\f3.csv'};
else
  REF = {'/home/username/data/f1.csv', '/home/username/data/f2.csv', '/home/username/data/f3.csv'};
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile("/home/username", "data", {'f1.csv', 'f2.csv', 'f3.csv'});
if ispc()
  REF = ["\home\username\data\f1.csv", "\home\username\data\f2.csv", "\home\username\data\f3.csv"];
else
  REF = ["/home/username/data/f1.csv", "/home/username/data/f2.csv", "/home/username/data/f3.csv"];
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', ["data", "data2"], {'f1.csv'});
if ispc()
  REF = ["\home\username\data\f1.csv", "\home\username\data2\f1.csv"];
else
  REF = ["/home/username/data/f1.csv", "/home/username/data2/f1.csv"];
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', ["data"; "data2"], {'f1.csv'});
if ispc()
  REF = ["\home\username\data\f1.csv"; "\home\username\data2\f1.csv"];
else
  REF = ["/home/username/data/f1.csv"; "/home/username/data2/f1.csv"];
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', {'data'; 'data2'}, "f1.csv");
if ispc()
  REF = ["\home\username\data\f1.csv"; "\home\username\data2\f1.csv"];
else
  REF = ["/home/username/data/f1.csv"; "/home/username/data2/f1.csv"];
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', {'data'; 'data2'}, 'data2', {'f1.csv'; 'data2'});
if ispc()
  REF = {'\home\username\data\data2\f1.csv'; '\home\username\data2\data2\data2'};
else
  REF = {'/home/username/data/data2/f1.csv'; '/home/username/data2/data2/data2'};
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', {'data', 'f'; 'data2', 'f'}, {'f1.csv'});
if ispc()
  REF = {'\home\username\data\f1.csv', '\home\username\f\f1.csv'; '\home\username\data2\f1.csv', '\home\username\f\f1.csv'};
else
  REF = {'/home/username/data/f1.csv', '/home/username/f/f1.csv'; '/home/username/data2/f1.csv', '/home/username/f/f1.csv'};
end
asserts.isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', [string(NaN); "data2"], {'f1.csv'});
if ispc()
  REF = ["\home\username\f1.csv"; "\home\username\data2\f1.csv"];
else
  REF = ["/home/username/f1.csv"; "/home/username/data2/f1.csv"];
end
asserts.isequal(R, REF);
%=============================================================================
% error
CMD = 'fullfile( {''data''; ''data2''}, {''f1.csv'', ''data2''})';
MSG = _('All string and cell array inputs must be the same size or scalars.');
asserts.checkerror(CMD, MSG);
%=============================================================================
CMD = 'fullfile( {''data''; ''data2''}, ''data2'', {''f1.csv'', ''data2''})';
MSG = _('All string and cell array inputs must be the same size or scalars.');
asserts.checkerror(CMD, MSG);
%=============================================================================
CMD = 'fullfile(''/home/username'', {''data''; ''data2''}, ''data2'', {''f1.csv'', ''data2''})';
MSG = _('All string and cell array inputs must be the same size or scalars.');
asserts.checkerror(CMD, MSG);
%=============================================================================
CMD = 'fullfile(''/home/username'', {''d'', ''f''; ''d'', ''f''}, {''f'', ''d''})';
MSG = _('All string and cell array inputs must be the same size or scalars.');
asserts.checkerror(CMD, MSG);
%=============================================================================
rootTemp = fullfile(tempdir(), ['compatibility_file_behaviors_', createGUID()]);
[status, msg] = mkdir(rootTemp);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.istrue(isdir(rootTemp));
nestedDir = fullfile(rootTemp, 'level1', 'level2');
[status, msg] = mkdir(nestedDir);
asserts.istrue(status);
asserts.isequal(msg, '');
textFile = fullfile(nestedDir, 'sample.txt');
filewrite(textFile, ["line one"; "line two"]);
asserts.istrue(isfile(textFile));
%=============================================================================
[pathPart, namePart, extensionPart] = fileparts(textFile);
asserts.isequal(pathPart, nestedDir);
asserts.isequal(namePart, 'sample');
asserts.isequal(extensionPart, '.txt');
asserts.isequal(fileparts(textFile, 'path'), nestedDir);
asserts.isequal(fileparts(textFile, 'filename'), 'sample');
asserts.isequal(fileparts(textFile, 'extension'), '.txt');
[pathPart, namePart, extensionPart] = fileparts(fullfile(nestedDir, 'README'));
asserts.isequal(pathPart, nestedDir);
asserts.isequal(namePart, 'README');
asserts.isequal(extensionPart, '');
combinedPaths = fullfile({'alpha', 'beta'}, 'gamma');
asserts.isequal(combinedPaths, {['alpha', filesep(), 'gamma'], ['beta', filesep(), 'gamma']});
combinedPaths = fullfile('alpha', {'beta'; 'gamma'});
asserts.isequal(combinedPaths, {['alpha', filesep(), 'beta']; ['alpha', filesep(), 'gamma']});
asserts.isequal(fullfile('alpha', ''), ['alpha', filesep()]);
%=============================================================================
% leading empty component(s) must not inject a parasitic separator
asserts.isequal(fullfile('', 'plotly'), 'plotly');
asserts.isequal(fullfile('', 'a', 'b'), ['a', filesep(), 'b']);
asserts.isequal(fullfile('', ''), '');
asserts.isequal(fullfile('', 'x', '', 'y'), ['x', filesep(), 'y']);
% empty component in the middle collapses double separators
asserts.isequal(fullfile('a', '', 'b'), ['a', filesep(), 'b']);
% empty component in tail keeps trailing separator
asserts.isequal(fullfile('a', 'b', ''), ['a', filesep(), 'b', filesep()]);
%=============================================================================
normalizedTextFile = replace(textFile, '\', '/');
asserts.isequal(fullpath(textFile), normalizedTextFile);
asserts.isequal(fullpath({textFile; textFile}), {normalizedTextFile; normalizedTextFile});
asserts.isequal(relativepath(rootTemp, textFile), './level1/level2/sample.txt');
siblingDir = fullfile(rootTemp, 'sibling');
mkdir(siblingDir);
siblingFile = fullfile(siblingDir, 'sample.txt');
filewrite(siblingFile, 'sibling');
asserts.isequal(relativepath(nestedDir, siblingFile), '../../sibling/sample.txt');
%=============================================================================
emptyDir = fullfile(rootTemp, 'empty_dir');
[status, msg] = mkdir(emptyDir);
asserts.istrue(status);
asserts.isequal(msg, '');
[status, msg] = rmdir(emptyDir);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.isfalse(isdir(emptyDir));
%=============================================================================
copyFile = fullfile(rootTemp, 'copy.txt');
[status, msg] = copyfile(textFile, copyFile);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.istrue(isfile(copyFile));
asserts.isequal(fileread(copyFile), fileread(textFile));
asserts.isequal(diff_file(textFile, copyFile), '');
filewrite(copyFile, ["line one"; "line three"]);
diffText = diff_file(textFile, copyFile, false);
asserts.istrue(contains(diffText, '-line two'));
asserts.istrue(contains(diffText, '+line three'));
%=============================================================================
createdName = tempname(rootTemp);
asserts.istrue(startsWith(createdName, rootTemp));
filewrite(createdName, 'temporary');
asserts.istrue(isfile(createdName));
[status, msg] = rmfile(createdName);
asserts.istrue(status);
asserts.isequal(msg, '');
asserts.isfalse(isfile(createdName));
[status, msg] = rmfile(createdName);
asserts.isfalse(status);
asserts.isequal(msg, _('an existing file expected.'));
%=============================================================================
asserts.checkerror('fullfile()', _('Not enough input arguments.'));
asserts.checkerror('fileparts()', _('Not enough input arguments.'));
tempnameMessage = [sprintf(_('Invalid input argument at position %d.'), 1), newline, ...
  _('Value must be a character vector or string scalar.')];
asserts.checkerror('tempname(1)', tempnameMessage);
%=============================================================================
rmdir(rootTemp, 's');
asserts.isfalse(isdir(rootTemp));
%=============================================================================

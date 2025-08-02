%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('fullfile'), -1);
assert_isequal(nargout('fullfile'), 1);
%=============================================================================
R = fullfile([]);
REF = '';
assert_isequal(R, REF);
%=============================================================================
R = fullfile({});
REF = {};
assert_isequal(R, REF);
%=============================================================================
R = fullfile('');
REF = '';
assert_isequal(R, REF);
%=============================================================================
R = fullfile('.');
REF = '.';
assert_isequal(R, REF);
%=============================================================================
R = fullfile("rr");
REF = "rr";
assert_isequal(R, REF);
%=============================================================================
R = fullfile(["rr", string(NaN)]);
REF = ["rr", ""];
assert_isequal(R, REF);
%=============================================================================
R = fullfile(string(NaN));
REF = "";
assert_isequal(R, REF);
%=============================================================================
R = fullfile('./');
if ispc()
  REF = '.\';
else
  REF = './';
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile('/a/./');
if ispc()
  REF = '\a\';
else
  REF = '/a/';
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile(nelsonroot,'toolbox','nelson','general','Contents.m');
if ispc()
  REF = [strrep(nelsonroot, '/', '\'), '\', 'toolbox', '\', 'nelson', '\', 'general', '\', 'Contents.m'];
else
  REF = [strrep(nelsonroot, '\', '/'), '/', 'toolbox', '/', 'nelson', '/', 'general', '/', 'Contents.m'];
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', {'data', 'data2'}, 'fff');
if ispc()
  REF = {'\home\username\data\fff', '\home\username\data2\fff'};
else
  REF = {'/home/username/data/fff', '/home/username/data2/fff'};
end
assert_isequal(R, REF);
%=============================================================================
R =  fullfile('/home/username', {'data'; 'data2'}, 'fff');
if ispc()
  REF = {'\home\username\data\fff'; '\home\username\data2\fff'};
else
  REF = {'/home/username/data/fff'; '/home/username/data2/fff'};
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', {'data1', 'data2' ; 'data3', 'data4'}, 'fff');
if ispc()
  REF = {'\home\username\data1\fff', '\home\username\data2\fff'; '\home\username\data3\fff', '\home\username\data4\fff'};
else
  REF = {'/home/username/data1/fff', '/home/username/data2/fff'; '/home/username/data3/fff', '/home/username/data4/fff'};
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile('\\\\home/username', {'data1', 'data2' ; 'data3', 'data4'}, 'fff');
if ispc()
  REF = {'\\home\username\data1\fff', '\\home\username\data2\fff'; '\\home\username\data3\fff',  '\\home\username\data4\fff'};
else
  REF = {'/home/username/data1/fff', '/home/username/data2/fff'; '/home/username/data3/fff',  '/home/username/data4/fff'};
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile ({'\\\/B:\//', 'A://c', '\\\C:/g/h/i/j\/'});
if ispc()
  REF =  {'\\B:\', 'A:\c', '\\C:\g\h\i\j\'};
else
  REF =  {'/B:/', 'A:/c', '/C:/g/h/i/j/'};
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile ('\/\/\//A:/\/\', 'x/', '/', '/', 'y', '/', '/');
if ispc()
  REF = '\A:\x\y\';
else
  REF = '/A:/x/y/';
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', {'data'; 'data2'});
if ispc()
  REF =  {'\home\username\data'; '\home\username\data2'};
else
  REF =  {'/home/username/data'; '/home/username/data2'};
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile( {'data', 'data2'}, '/home/username');
if ispc()
  REF = {'data\home\username', 'data2\home\username'};
else
  REF = {'data/home/username', 'data2/home/username'};
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile( {'data'; 'data2'}, '/home/username');
if ispc()
  REF = {'data\home\username'; 'data2\home\username'};
else
  REF = {'data/home/username'; 'data2/home/username'};
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', 'data', {'f1.csv', 'f2.csv', 'f3.csv'});
if ispc()
  REF = {'\home\username\data\f1.csv', '\home\username\data\f2.csv', '\home\username\data\f3.csv'};
else
  REF = {'/home/username/data/f1.csv', '/home/username/data/f2.csv', '/home/username/data/f3.csv'};
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile("/home/username", "data", {'f1.csv', 'f2.csv', 'f3.csv'});
if ispc()
  REF = ["\home\username\data\f1.csv", "\home\username\data\f2.csv", "\home\username\data\f3.csv"];
else
  REF = ["/home/username/data/f1.csv", "/home/username/data/f2.csv", "/home/username/data/f3.csv"];
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', ["data", "data2"], {'f1.csv'});
if ispc()
  REF = ["\home\username\data\f1.csv", "\home\username\data2\f1.csv"];
else
  REF = ["/home/username/data/f1.csv", "/home/username/data2/f1.csv"];
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', ["data"; "data2"], {'f1.csv'});
if ispc()
  REF = ["\home\username\data\f1.csv"; "\home\username\data2\f1.csv"];
else
  REF = ["/home/username/data/f1.csv"; "/home/username/data2/f1.csv"];
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', {'data'; 'data2'}, "f1.csv");
if ispc()
  REF = ["\home\username\data\f1.csv"; "\home\username\data2\f1.csv"];
else
  REF = ["/home/username/data/f1.csv"; "/home/username/data2/f1.csv"];
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', {'data'; 'data2'}, 'data2', {'f1.csv'; 'data2'});
if ispc()
  REF = {'\home\username\data\data2\f1.csv'; '\home\username\data2\data2\data2'};
else
  REF = {'/home/username/data/data2/f1.csv'; '/home/username/data2/data2/data2'};
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', {'data','f'; 'data2','f'}, {'f1.csv'});
if ispc()
  REF = {'\home\username\data\f1.csv', '\home\username\f\f1.csv'; '\home\username\data2\f1.csv', '\home\username\f\f1.csv'};
else
  REF = {'/home/username/data/f1.csv', '/home/username/f/f1.csv'; '/home/username/data2/f1.csv', '/home/username/f/f1.csv'};
end
assert_isequal(R, REF);
%=============================================================================
R = fullfile('/home/username', [string(NaN); "data2"], {'f1.csv'});
if ispc()
  REF = ["\home\username\f1.csv";  "\home\username\data2\f1.csv"];
else
  REF = ["/home/username/f1.csv";  "/home/username/data2/f1.csv"];
end
assert_isequal(R, REF);
%=============================================================================
% error
CMD = 'fullfile( {''data''; ''data2''}, {''f1.csv'', ''data2''})';
MSG = _('All string and cell array inputs must be the same size or scalars.');
assert_checkerror(CMD, MSG);
%=============================================================================
CMD = 'fullfile( {''data''; ''data2''}, ''data2'', {''f1.csv'', ''data2''})';
MSG = _('All string and cell array inputs must be the same size or scalars.');
assert_checkerror(CMD, MSG);
%=============================================================================
CMD = 'fullfile(''/home/username'', {''data''; ''data2''}, ''data2'', {''f1.csv'', ''data2''})';
MSG = _('All string and cell array inputs must be the same size or scalars.');
assert_checkerror(CMD, MSG);
%=============================================================================
CMD = 'fullfile(''/home/username'', {''d'', ''f''; ''d'', ''f''}, {''f'', ''d''})';
MSG = _('All string and cell array inputs must be the same size or scalars.');
assert_checkerror(CMD, MSG);
%=============================================================================

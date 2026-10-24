%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% s.f{I} = B where s is a struct and s.f is neither a cell nor a string array:
% the brace level is the one that fails, not the field assignment.
%=============================================================================
addpath([modulepath('interpreter', 'tests'), '/classdef']);
%=============================================================================
msg = _('Unable to perform assignment because brace indexing is not supported for variables of this type.');
%=============================================================================
s.a = [1 2];
ME = [];
try
  s.a{2} = 1;
catch ME
end
asserts.isfalse(isempty(ME));
asserts.isequal(ME.message, msg);
asserts.isequal(ME.identifier, 'Nelson:cellAssToNonCell');
asserts.isequal(s.a, [1 2]);
%=============================================================================
commands = {'s.a = ''ab''; s.a{2} = 1;', ...
's.a = true; s.a{2} = 1;', ...
's.a = int8(1); s.a{2} = 1;', ...
's.a = struct(''b'', 1); s.a{2} = 1;', ...
's.a = [1 2]; s.a{:} = 1;', ...
's.a.b = 3; s.a.b{1} = 1;', ...
's(1).a = [1 2]; s(1).a{2} = 1;', ...
's(2).a = [1 2]; s(2).a{2} = 1;', ...
's = struct(''a'', [1 2], ''b'', 3); s.a{2} = 1;', ...
'o = ClassdefBenchPoint(4); o.Id{2} = 1;', ...
'assignBracesToStructField();'};
for k = 1:numel(commands)
  clear('s', 'o');
  ME = [];
  try
    eval(commands{k});
  catch ME
  end
  asserts.isfalse(isempty(ME));
  asserts.isequal(ME.message, msg);
  asserts.isequal(ME.identifier, 'Nelson:cellAssToNonCell');
end
%=============================================================================
% an empty or cell field still takes the brace assignment
clear('s');
s.a = [];
s.a{2} = 1;
asserts.isequal(s.a, {[], 1});
%=============================================================================
s.a = {1};
s.a{2} = 2;
asserts.isequal(s.a, {1, 2});
%=============================================================================
s = struct();
s.a{2} = 1;
asserts.isequal(s.a, {[], 1});
%=============================================================================
function assignBracesToStructField()
  s.a = [1 2];
  s.a{2} = 1;
end
%=============================================================================

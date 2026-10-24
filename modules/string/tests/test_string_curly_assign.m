%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% S{I} = B on a string array accepts a character row vector, '' or [] only
%=============================================================================
msg = _('Curly brace assignment into a string expects a character vector.');
commands = {'x = "ab"; x{1} = 1;', ...
'x = "ab"; x{1} = "cd";', ...
'x = "ab"; x{1} = {''a''};', ...
'x = "ab"; x{1} = [''ab''; ''cd''];', ...
'x = "ab"; x{1} = true;', ...
'x = "ab"; x{1} = zeros(1, 0);', ...
'x = "ab"; x{1} = char(zeros(0, 3));', ...
'x = "ab"; x{1, 1} = char(zeros(0, 3));', ...
'x = "ab"; x{1} = ''cd''.'';', ...
'x = "ab"; x{1} = int8(3);', ...
'x = ["a" "b"]; x{1, 2} = 3;', ...
'x = ["a" "b"]; [x{1:2}] = deal(''c'', 2);', ...
'x = ["a" "b"]; [x{1:2}] = deal(1, ''d'');', ...
'x = ["a" "b"]; [x{1, 1:2}] = deal(''c'', 2);', ...
's.a = "str"; s.a{2} = 1;'};
for k = 1:numel(commands)
  ME = [];
  try
    eval(commands{k});
  catch ME
  end
  asserts.isfalse(isempty(ME));
  asserts.isequal(ME.message, msg);
  asserts.isequal(ME.identifier, 'Nelson:string:UnsupportedCurlyAsgnValue');
end
%=============================================================================
x = ["a" "b"];
ME = [];
try
  [x{1:2}] = deal('c', 2);
catch ME
end
asserts.isfalse(isempty(ME));
asserts.isequal(x, ["a" "b"]);
%=============================================================================
x = "ab";
x{1} = '';
asserts.isequal(x, "");
%=============================================================================
x = "ab";
x{1, 1} = '';
asserts.isequal(x, "");
%=============================================================================
x = "ab";
x{1} = char(zeros(1, 0));
asserts.isequal(strlength(x), 0);
%=============================================================================
x = "ab";
x{1} = [];
asserts.istrue(ismissing(x));
%=============================================================================
x = "ab";
x{1} = 'cd';
asserts.isequal(x, "cd");
%=============================================================================
x = "ab";
x{3} = 'z';
asserts.isequal(size(x), [1 3]);
asserts.isequal(x([1 3]), ["ab" "z"]);
asserts.istrue(ismissing(x(2)));
%=============================================================================
x = ["a" "b"];
[x{1:2}] = deal('c', 'd');
asserts.isequal(x, ["c" "d"]);
%=============================================================================
x = "ab";
x{1}(1) = 'z';
asserts.isequal(x, "zb");
%=============================================================================
s.a = "str";
s.a{2} = 'q';
asserts.isequal(s.a, ["str" "q"]);
%=============================================================================

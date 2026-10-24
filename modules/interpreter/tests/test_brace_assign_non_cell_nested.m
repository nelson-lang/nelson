%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A{I}.f = B, A{I}(J) = B, A{I}{J} = B on a non-empty value that is neither a
% cell nor a string array
%=============================================================================
msg = _('Unable to perform assignment because brace indexing is not supported for variables of this type.');
%=============================================================================
x = [1 2];
ME = [];
try
  x{2}.a = 1;
catch ME
end
asserts.isfalse(isempty(ME));
asserts.isequal(ME.message, msg);
asserts.isequal(ME.identifier, 'Nelson:cellAssToNonCell');
asserts.isequal(x, [1 2]);
%=============================================================================
x = [1 2];
ME = [];
try
  x{2}(1) = 1;
catch ME
end
asserts.isfalse(isempty(ME));
asserts.isequal(ME.message, msg);
asserts.isequal(ME.identifier, 'Nelson:cellAssToNonCell');
%=============================================================================
x = [1 2];
ME = [];
try
  x{2}{1} = 1;
catch ME
end
asserts.isfalse(isempty(ME));
asserts.isequal(ME.identifier, 'Nelson:cellAssToNonCell');
%=============================================================================
x = [1 2];
ME = [];
try
  x{2}.a.b = 1;
catch ME
end
asserts.isfalse(isempty(ME));
asserts.isequal(ME.identifier, 'Nelson:cellAssToNonCell');
%=============================================================================
x = [1 2];
ME = [];
try
  x{1, 2}.a = 1;
catch ME
end
asserts.isfalse(isempty(ME));
asserts.isequal(ME.identifier, 'Nelson:cellAssToNonCell');
%=============================================================================
x = [1 2];
ME = [];
try
  x{5}.a = 1;
catch ME
end
asserts.isfalse(isempty(ME));
asserts.isequal(ME.identifier, 'Nelson:cellAssToNonCell');
%=============================================================================
commands = {'x = 5; x{1}.a = 1;', ...
'x = struct(''a'', 1); x{2}.b = 1;', ...
'x = struct(''a'', 1); x{1}(2) = 1;', ...
'x = ''ab''; x{2}(1) = ''c'';', ...
'x = true(1, 2); x{2}.a = 1;', ...
'x = int8([1 2]); x{2}(1) = 1;', ...
'x = @sin; x{1}.a = 1;', ...
's.f = [1 2]; s.f{2}.a = 1;', ...
's.f = [1 2]; s.f{2}(1) = 1;', ...
'T = table([1; 2], [3; 4], ''VariableNames'', {''A'', ''B''}); T.B{2}.a = 1;', ...
'T = table([1; 2], [3; 4], ''VariableNames'', {''A'', ''B''}); T.B{2}(1) = 1;', ...
'c = {[1 2]}; c{1}{2}.a = 1;', ...
'assignNestedBracesToDouble();'};
for k = 1:numel(commands)
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
% an empty value becomes a cell array
x = [];
x{2}.a = 1;
asserts.isequal(class(x), 'cell');
asserts.isequal(size(x), [1 2]);
asserts.isequal(x{2}.a, 1);
%=============================================================================
x = [];
x{2}(3) = 1;
asserts.isequal(x, {[], [0 0 1]});
%=============================================================================
x = zeros(0, 3);
x{2}.a = 1;
asserts.isequal(class(x), 'cell');
asserts.isequal(size(x), [1 2]);
%=============================================================================
x = '';
x{2}.a = 1;
asserts.isequal(class(x), 'cell');
%=============================================================================
x = ["a", "b"];
x{2}(1) = 'c';
asserts.isequal(x, ["a", "c"]);
%=============================================================================
c = {1};
c{3}.a = 2;
asserts.isequal(c{3}.a, 2);
%=============================================================================
% plain reads keep their own error
x = [1 2];
asserts.checkerror('x{2}', _('Attempt to apply contents-indexing to non cell-array object.'));
%=============================================================================
function assignNestedBracesToDouble()
  x = [1 2];
  x{2}.a = 1;
end
%=============================================================================

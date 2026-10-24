%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A{I} = B on a non-empty value that is neither a cell nor a string array
%=============================================================================
msg = _('Unable to perform assignment because brace indexing is not supported for variables of this type.');
commands = {'x = [1 2]; x{2} = 1;', ...
'x = 5; x{1} = 1;', ...
'x = 1:3; x{end} = 1;', ...
'x = [1 2]; x{2, 1} = 1;', ...
'x = [1 2]; x{:} = 1;', ...
'x = [1 2]; [x{1:2}] = deal(1, 2);', ...
'x = ''ab''; x{1} = 1;', ...
'x = true; x{1} = 1;', ...
'x = int8(1); x{1} = 1;', ...
's.a = 1; s{1} = 2;', ...
's = struct(); s{1} = 2;', ...
'c = {[1 2]}; c{1}{2} = 3;', ...
'f = @sin; f{1} = 1;', ...
'f = str2func(''cos''); f{2} = 1;', ...
'f = @(x) x + 1; f{:} = 1;', ...
'f = @sin; [f{1:2}] = deal(1, 2);', ...
's.f = @sin; s.f{1} = 1;', ...
'assignBracesToFunctionHandle();'};
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
x = [];
x{2} = 1;
asserts.isequal(x, {[], 1});
%=============================================================================
x = zeros(1, 0);
x{1} = 1;
asserts.isequal(x, {1});
%=============================================================================
x = ["ab", "cd"];
x{2} = 'ef';
asserts.isequal(x, ["ab", "ef"]);
%=============================================================================
function assignBracesToFunctionHandle()
  f = @sin;
  f{1} = 1;
end
%=============================================================================

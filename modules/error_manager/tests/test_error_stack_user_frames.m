%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Error stacks list the frames of user code only: no frame for error, throw,
% a failing builtin or a builtin calling user code.
%=============================================================================
builtinFrames = {'error', 'throw', 'zeros', 'cellfun', 'rethrow'};
%=============================================================================
e = caught(@raise_in_local);
asserts.isequal(e.stack(1).name, 'raise_in_local');
asserts.isfalse(any(ismember({e.stack.name}, builtinFrames)));
%=============================================================================
e = caught(@builtin_fails_in_local);
asserts.isequal(e.stack(1).name, 'builtin_fails_in_local');
asserts.isfalse(any(ismember({e.stack.name}, builtinFrames)));
%=============================================================================
e = caught(@throw_in_local);
asserts.isequal(e.stack(1).name, 'throw_in_local');
asserts.isfalse(any(ismember({e.stack.name}, builtinFrames)));
%=============================================================================
e = caught(@cellfun_calls_local);
asserts.isequal(e.stack(1).name, 'raise_with_argument');
asserts.isequal(e.stack(2).name, 'cellfun_calls_local');
asserts.isfalse(any(ismember({e.stack.name}, builtinFrames)));
%=============================================================================
e = caught(@raise_struct_in_local);
asserts.isequal(e.stack(1).name, 'raise_struct_in_local');
%=============================================================================
try
  error('stack:frames', 'at the script level');
catch e
end
asserts.isfalse(any(ismember({e.stack.name}, builtinFrames)));
asserts.istrue(isempty(e.stack) || ~strcmp(e.stack(1).name, 'error'));
%=============================================================================
last = lasterror();
asserts.isfalse(any(ismember({last.stack.name}, builtinFrames)));
%=============================================================================
function e = caught(f)
  e = [];
  try
    f();
  catch e
  end
end
%=============================================================================
function raise_in_local()
  error('stack:frames', 'raised in a local function');
end
%=============================================================================
function builtin_fails_in_local()
  x = zeros('not a class name'); %#ok<NLS0004>
end
%=============================================================================
function throw_in_local()
  throw(MException('stack:frames', 'thrown'));
end
%=============================================================================
function cellfun_calls_local()
  cellfun(@raise_with_argument, {1});
end
%=============================================================================
function raise_with_argument(x)
  error('stack:frames', 'raised for %d', x);
end
%=============================================================================
function raise_struct_in_local()
  error(struct('message', 'from a structure', 'identifier', 'stack:frames'));
end
%=============================================================================

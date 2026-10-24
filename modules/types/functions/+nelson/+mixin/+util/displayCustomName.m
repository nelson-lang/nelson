%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Prints the "name = " assignment echo (objects keep a trailing blank) that precedes a CustomDisplay body when
% an object is shown through display (not disp). Kept in .m so the exact
% spacing can evolve without rebuilding the interpreter.
%=============================================================================
function displayCustomName(name)
  if nargin < 1 || isempty(name)
    return;
  end
  fprintf('\n');
  fprintf('%s = \n', char(name));
  fprintf('\n');
end
%=============================================================================

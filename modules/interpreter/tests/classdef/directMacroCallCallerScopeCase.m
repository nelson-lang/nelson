%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = directMacroCallCallerScopeCase()
  % a leaf function called on the direct bytecode path pushes no scope of
  % its own: the context scope of this function must survive the call
  % (varunlock errors if the variable vanished from the scope)
  keep = 1;
  varlock('local', 'keep');
  r = directMacroCallLeaf(41);
  varunlock('local', 'keep');
  r = r + keep;
end

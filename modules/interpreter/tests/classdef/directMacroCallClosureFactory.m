%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = directMacroCallClosureFactory(seed)
  % the handle runs with a scope of its own that the call reads back once
  % the body ended: the closure state updated by the nested function must
  % still be there
  base = seed;
  h = @(delta) directMacroCallClosureStep(delta);
  function y = directMacroCallClosureStep(delta)
    base = base + delta;
    y = base;
  end
end

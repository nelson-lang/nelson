%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function h = anonymousNestedWrapperFactory(seed)
  % the handle calls a nested function, which reads this workspace: the
  % whole workspace is captured, not only the names of the body
  base = seed;
  h = @(delta) anonymousNestedWrapperStep(delta);
  function y = anonymousNestedWrapperStep(delta)
    y = base + delta;
  end
end

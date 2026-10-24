%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [parentAfter, innerResult, sharedAfter] = nested_param_shadow()
  f = 'PARENT_STRING';
  shared = 10;
  innerResult = inner(42);
  parentAfter = f;
  sharedAfter = shared;
  function r = inner(f)
    % f is a parameter: local to the nested function, must not clobber parent f.
    % shared is not a parameter here: it is shared with the parent.
    shared = shared + 1;
    r = f + 1;
  end
end
%=============================================================================

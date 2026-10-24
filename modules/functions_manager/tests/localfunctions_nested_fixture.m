%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = localfunctions_nested_fixture()
  function y = hidden_nested(x)
    y = x + 1;
  end
  R = localfunctions();
end
%=============================================================================
function y = visible_local(x)
  y = x - 1;
end
%=============================================================================

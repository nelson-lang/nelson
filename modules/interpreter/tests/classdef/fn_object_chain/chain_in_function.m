%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [a, b, c] = chain_in_function()
  a = chainroot.Inner.Scale(3);
  b = chainroot.inner.Scale(4);
  c = chainroot.Inner.add(5);
end

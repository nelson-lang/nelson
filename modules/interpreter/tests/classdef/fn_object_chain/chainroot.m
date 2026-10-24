%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = chainroot()
  % A zero-argument function returning an object whose members are reached
  % through a dotted chain: chainroot.Inner.Scale(x).
  r = struct('Inner', ChainLeaf(), 'inner', ChainLeaf());
end

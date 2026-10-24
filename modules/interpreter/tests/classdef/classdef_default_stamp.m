%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = classdef_default_stamp()
  % counts how many times classdef property default expressions ran
  persistent n
  if isempty(n)
    n = 0;
  end
  n = n + 1;
  r = n;
end

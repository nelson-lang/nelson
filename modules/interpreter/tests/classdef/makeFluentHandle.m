%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = makeFluentHandle(v)
  if nargin == 0
    v = 0;
  end
  obj = ClassdefFluentHandle(v);
end
%=============================================================================

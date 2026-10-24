%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_wait_for_windows__()
  while ~isempty(findall(groot(), '-depth', 1, 'Type', 'figure', 'Visible', 'on'))
    pause(0.01);
  end
end

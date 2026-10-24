%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function data = graphicsAppendColumn(data, value)
  value = value(:);
  if isempty(data)
    data = value;
  else
    data = [data, value];
  end
end
%=============================================================================

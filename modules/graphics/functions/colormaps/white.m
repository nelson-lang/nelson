%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function map = white(m)
  if nargin < 1
    f = get(groot(), 'CurrentFigure');
    if isempty(f)
      m = 256;
    else
      m = size(f.Colormap,1);
    end
  end
  map = ones(m, 3);
end
%=============================================================================

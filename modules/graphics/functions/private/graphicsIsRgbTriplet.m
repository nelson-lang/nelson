%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = graphicsIsRgbTriplet(value)
  tf = isnumeric(value) && isvector(value) && numel(value) == 3 ...
    && all(value >= 0) && all(value <= 1);
end
%=============================================================================

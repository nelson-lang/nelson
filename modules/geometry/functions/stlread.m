%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [TR, fileformat, attributes, solidID] = stlread(filename)
  narginchk(1, 1);
  nargoutchk(0, 4);
  [connectivity, points, fileformat, attributes, solidID] = __stlread__(filename);
  TR = triangulation(connectivity, points);
end
%=============================================================================

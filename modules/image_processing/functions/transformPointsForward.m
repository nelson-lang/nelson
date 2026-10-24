%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = transformPointsForward(varargin)
  narginchk(2, 4);
  [T, dim, isProjective] = ip_tform_points_parse(varargin{1});
  varargout = ip_transform_points(T, dim, isProjective, varargin(2:end));
end
%=============================================================================

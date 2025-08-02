%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function res = rotx(varargin)
  narginchk(1, 1);
  angle_degree = varargin{1};
  if ~isscalar(angle_degree)
    error('Nelson:rotx:kNonScalar', _('Input argument #1: scalar expected.'));
  end
  res = [1 0 0;
  0, cosd(angle_degree), -sind(angle_degree);
  0, sind(angle_degree), cosd(angle_degree)];
end
%=============================================================================

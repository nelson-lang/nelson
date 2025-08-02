%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = rad2deg(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  x = varargin{1};
  if isfloat(x)
    y = (180 / pi) * x;
  else
    error('Nelson:rad2deg:nonFloatInput', _('Input must be single or double.'));
  end
end
%=============================================================================

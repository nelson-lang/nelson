%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = nanstd(x, varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  [flag, args] = parseNanstdInputsLocal(varargin);
  y = __nan_statistics__(x, 'std', flag, args{:});
end
%=============================================================================
function [flag, args] = parseNanstdInputsLocal(args)
  flag = 0;
  if isempty(args)
    return
  end
  flag = args{1};
  if isempty(flag)
    flag = 0;
  end
  if ~(isnumeric(flag) && isreal(flag) && isscalar(flag) && (flag == 0 || flag == 1))
    error('Nelson:statistics:invalidFlag', '%s must be 0 or 1.', 'Flag');
  end
  args = args(2:end);
end
%=============================================================================

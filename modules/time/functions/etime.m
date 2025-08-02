%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function t = etime(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  t1 = varargin{1};
  t0 = varargin{2};
  allReal = isreal(t0) && isreal(t1); 
  if allReal
    t = 86400 * (datenum(t1(:,1:3)) - datenum(t0(:, 1:3))) +  (t1(:, 4:6) - t0(:, 4:6)) * [3600; 60; 1];
  else
    error('Nelson:etime:InputMustBeReal', _('Numeric input data must be real.'));
  end
end
%=============================================================================

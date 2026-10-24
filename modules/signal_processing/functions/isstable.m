%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = isstable(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if nargin == 1
    denominator = varargin{1};
    if ismatrix(denominator) && size(denominator, 2) == 6
      [~, denominator] = sos2tf(denominator);
    end
  else
    denominator = varargin{2};
  end
  poles = roots(denominator);
  tf = isempty(poles) || all(abs(poles) < 1);
end
%=============================================================================

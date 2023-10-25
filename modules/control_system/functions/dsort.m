%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = dsort(varargin)
  narginchk(1, 1);
  nargoutchk(0, 2);
  p = varargin{1};
  p = p(:);
  [result, index] = sort(p, 'descend');
  varargout{1} = p(index);
  if (nargout == 2)
    varargout{2} = index;
  end 
end
%=============================================================================

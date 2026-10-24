%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = __interp1_generic__(varargin)
  narginchk(2, 5);
  nargoutchk(0, 1);

  [x, v, xq, method, extrapMode, extrapValue, returnPP] = interp_parse1(varargin{:});
  if returnPP
    varargout{1} = interp1_pp(x, v, method);
  else
    varargout{1} = interp1_eval(x, v, xq, method, extrapMode, extrapValue);
  end
end
%=============================================================================

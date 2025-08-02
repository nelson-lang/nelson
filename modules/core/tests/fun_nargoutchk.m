%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = fun_nargoutchk(varargin)
  nargoutchk(2, 3)
  for n = 1:nargout
    varargout{n} = n;
  end
end
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = polaraxes(varargin)
  nargoutchk(0, 1);
  inputArguments = convertStringToCharArgs(varargin);
  ax = __polaraxes__(inputArguments{:});
  [fig, previousDrawLater] = polarBeginDrawLater(ax);
  try
    ax = polarInitializeAxes(ax);
    polarEndDrawLater(fig, previousDrawLater);
  catch exception
    polarEndDrawLater(fig, previousDrawLater);
    error(exception.message);
  end
  if nargout == 1
    varargout{1} = ax;
  end
end
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = odeCallFcn(fcn, t, y, options, yp)
  if nargin < 5
    args = {t, y};
  else
    args = {t, y, yp};
  end
  if isfield(options, 'Parameters') && ~isempty(options.Parameters)
    args = odeAppendParameters(args, options.Parameters);
  end
  if nargout == 0
    feval(fcn, args{:});
  else
    [varargout{1:nargout}] = feval(fcn, args{:});
  end
end
%=============================================================================

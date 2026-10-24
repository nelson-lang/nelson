%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = campos(varargin)
  % campos, campos(ax)
  % campos([x y z]), campos(ax, [x y z])
  % campos('auto'|'manual'|'mode'), campos(ax, ...)
  narginchk(0, 2);
  nargoutchk(0, 1);
  [ax, args] = __camAxesArgs__(varargin);
  out = __camGetSetProp__(ax, 'CameraPosition', 'CameraPositionMode', args, nargout);
  if (nargout > 0 || isempty(args))
    varargout{1} = out;
  else
    varargout = {};
  end
end
%=============================================================================

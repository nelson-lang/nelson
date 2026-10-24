%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = camva(varargin)
  % camva, camva(ax)
  % camva(angle), camva(ax, angle)
  % camva('auto'|'manual'|'mode'), camva(ax, ...)
  narginchk(0, 2);
  nargoutchk(0, 1);
  [ax, args] = __camAxesArgs__(varargin);
  if (~isempty(args) && ~(ischar(args{1}) || isStringScalar(args{1})))
    if (~(isnumeric(args{1}) && isscalar(args{1}) && isreal(args{1}) && args{1} > 0 && args{1} < 180))
      error('Nelson:graphics:invalidCamViewAngle', ...
        'Camera view angle must be a real scalar in the range (0, 180).');
    end
  end
  out = __camGetSetProp__(ax, 'CameraViewAngle', 'CameraViewAngleMode', args, nargout);
  if (nargout > 0 || isempty(args))
    varargout{1} = out;
  else
    varargout = {};
  end
end
%=============================================================================

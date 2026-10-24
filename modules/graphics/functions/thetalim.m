%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = thetalim(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = polarAxesForFunction(args);
  state = polarGetState(ax, isempty(args));
  if isempty(args)
    varargout{1} = state.ThetaLim;
    return
  end
  value = args{1};
  if ischar(value)
    switch lower(value)
      case 'mode'
        varargout{1} = state.ThetaLimMode;
        return
      case 'auto'
        state.ThetaLim = defaultThetaLim(state.ThetaAxisUnits);
        state.ThetaLimMode = 'auto';
      case 'manual'
        state.ThetaLimMode = 'manual';
      otherwise
        error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
    end
  elseif isnumeric(value) && numel(value) == 2 && value(2) > value(1)
    state.ThetaLim = value(:)';
    state.ThetaLimMode = 'manual';
  else
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  if nargout == 1
    error('Nelson:graphics:invalidOutputArgument', 'Invalid output argument.');
  end
  polarSetStateAndRefresh(ax, state);
end
%=============================================================================
function value = defaultThetaLim(thetaAxisUnits)
  if strcmp(thetaAxisUnits, 'radians')
    value = [0 2 * pi];
  else
    value = [0 360];
  end
end
%=============================================================================

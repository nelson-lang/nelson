%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = thetaticks(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = polarAxesForFunction(args);
  state = polarGetState(ax, isempty(args));
  if isempty(args)
    varargout{1} = state.ThetaTick;
    return
  end
  value = args{1};
  if ischar(value)
    switch lower(value)
      case 'mode'
        varargout{1} = state.ThetaTickMode;
        return
      case 'auto'
        state.ThetaTickMode = 'auto';
        state.ThetaTickLabelMode = 'auto';
      case 'manual'
        state.ThetaTickMode = 'manual';
      otherwise
        error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
    end
  elseif isnumeric(value)
    state.ThetaTick = value(:)';
    state.ThetaTickMode = 'manual';
    if strcmp(state.ThetaTickLabelMode, 'auto')
      state.ThetaTickLabel = polarThetaTickLabels(state.ThetaTick, state.ThetaLim, state.ThetaAxisUnits);
    end
  else
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  if nargout == 1
    error('Nelson:graphics:invalidOutputArgument', 'Invalid output argument.');
  end
  polarSetStateAndRefresh(ax, state);
end
%=============================================================================

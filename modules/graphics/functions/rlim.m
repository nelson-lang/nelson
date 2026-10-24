%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = rlim(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = polarAxesForFunction(args);
  state = polarGetState(ax, isempty(args));
  previousState = state;
  if isempty(args)
    varargout{1} = state.RLim;
    return
  end
  value = args{1};
  if ischar(value)
    switch lower(value)
      case 'mode'
        varargout{1} = state.RLimMode;
        return
      case 'auto'
        state.RLimMode = 'auto';
      case 'manual'
        state.RLimMode = 'manual';
      otherwise
        error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
    end
  elseif isnumeric(value) && numel(value) == 2 && value(2) > value(1)
    state.RLim = value(:)';
    state.RLimMode = 'manual';
  else
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
  if nargout == 1
    error('Nelson:graphics:invalidOutputArgument', 'Invalid output argument.');
  end
  if rlimNeedsRefresh(previousState, state)
    polarSetStateAndRefresh(ax, state);
  else
    polarSetState(ax, state);
  end
end
%=============================================================================
function tf = rlimNeedsRefresh(previousState, state)
  tf = ~isequal(previousState.RLim, state.RLim) ...
    || ~isequal(previousState.RTick, state.RTick) ...
    || ~isequal(previousState.RTickLabel, state.RTickLabel);
end
%=============================================================================

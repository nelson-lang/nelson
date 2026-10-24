%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = rticks(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = polarAxesForFunction(args);
  state = polarGetState(ax, isempty(args));
  if isempty(args)
    varargout{1} = state.RTick;
    return
  end
  value = args{1};
  if ischar(value)
    switch lower(value)
      case 'mode'
        varargout{1} = state.RTickMode;
        return
      case 'auto'
        state.RTickMode = 'auto';
        state.RTickLabelMode = 'auto';
      case 'manual'
        state.RTickMode = 'manual';
      otherwise
        error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
    end
  elseif isnumeric(value)
    state.RTick = value(:)';
    state.RTickMode = 'manual';
    if strcmp(state.RTickLabelMode, 'auto')
      state.RTickLabel = polarFormatTickLabels(state.RTick, false);
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

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = rticklabels(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = polarAxesForFunction(args);
  state = polarGetState(ax, isempty(args));
  if isempty(args)
    varargout{1} = state.RTickLabel;
    return
  end
  value = args{1};
  if ischar(value) && any(strcmp(lower(value), {'auto', 'manual', 'mode'}))
    switch lower(value)
      case 'mode'
        varargout{1} = state.RTickLabelMode;
        return
      case 'auto'
        state.RTickLabelMode = 'auto';
      case 'manual'
        state.RTickLabelMode = 'manual';
    end
  else
    state.RTickLabel = polarNormalizeLabels(value);
    state.RTickLabelMode = 'manual';
  end
  if nargout == 1
    error('Nelson:graphics:invalidOutputArgument', 'Invalid output argument.');
  end
  polarSetStateAndRefresh(ax, state);
end
%=============================================================================

%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = zticklabels(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseAxis(args);
  if isempty(args)
    varargout{1} = ax.ZTickLabel;
    return
  end
  value = args{1};
  if ischar(value)
    switch lower(value)
      case 'mode'
        varargout{1} = ax.ZTickLabelMode;
        return
      case 'auto'
        ax.ZTickLabelMode = 'auto';
      case 'manual'
        ax.ZTickLabelMode = 'manual';
      otherwise
        ax.ZTickLabel = {value};
        ax.ZTickLabelMode = 'manual';
    end
  else
    ax.ZTickLabel = normalizeTickLabels(value);
    ax.ZTickLabelMode = 'manual';
  end
  if nargout == 1
    error('Nelson:graphics:invalidOutputArgument', 'Invalid output argument.');
  end
end
%=============================================================================
function [ax, args] = parseAxis(args)
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = args{1};
    args = args(2:end);
  else
    ax = gca();
  end
end
%=============================================================================
function labels = normalizeTickLabels(value)
  if isnumeric(value) && isempty(value)
    % zticklabels([]) clears the tick labels.
    labels = {};
  elseif isstring(value)
    labels = cellstr(value(:));
  elseif iscellstr(value)
    labels = value(:);
  elseif iscell(value)
    labels = cell(size(value(:)));
    flat = value(:);
    for k = 1:numel(flat)
      labels{k} = char(flat{k});
    end
  else
    error('Nelson:graphics:invalidInputArguments', 'Invalid input arguments.');
  end
end
%=============================================================================

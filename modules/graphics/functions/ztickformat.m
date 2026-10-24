%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ztickformat(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  [ax, args] = parseAxis(args);
  if isempty(args)
    varargout{1} = ax.ZTickLabelFormat;
    return
  end
  value = args{1};
  if ~ischar(value) || (~isempty(value) && ~isrow(value))
    error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Format');
  end
  ax.ZTickLabelFormat = value;
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

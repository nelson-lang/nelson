%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = caxis(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  args = convertStringToCharArgs(varargin);
  if nargout == 0
    clim(args{:});
    varargout = {};
  else
    if ((nargin > 0) && isscalar(args{1}) && isgraphics(args{1}))
      handle = args{1};
      args(1) = [];
    else
      handle = gca();
    end
    if isempty(args)
      varargout{1} = handle.CLim;
    elseif ischar(args{1}) && strcmp(args{1}, 'mode')
      varargout{1} = handle.CLimMode;
    else
      clim(varargin{:});
      varargout{1} = handle.CLim;
    end
  end
end
%=============================================================================

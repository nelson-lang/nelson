%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = xlim(varargin)
  narginchk(0, 2);
  nargoutchk(0, 1);
  value = [];
  if nargin == 0
    ax = gca();
    varargout{1} = ax.XLim;
    return
  end 
  if nargin == 1
    if isgraphics(varargin{1})
      ax = varargin{1};
    else
      if ischar(varargin{1})
        if (strcmp(varargin{1}, 'mode'))
          res = xlim(gca(), varargin{1});
          varargout{1} = res;
          return;
        end
        if (strcmp(varargin{1}, 'auto'))
          xlim(gca(), varargin{1});
          return;
        end
        if (strcmp(varargin{1}, 'manual'))
          xlim(gca(), varargin{1});
          return;
        end
        error(_('Invalid input arguments.'));
      else
        ax = gca();
        value = varargin{1};
      end
    end
  else
    % nargin == 2
    ax = varargin{1};
    if ~isgraphics(ax, 'axes')
      error(_('Invalid input arguments.'));
    end
    value = varargin{2};
  end
  if ischar(value)
    if (strcmp(value, 'mode'))
      res = ax.XLimMode;
      varargout{1} = res;
      return;
    end
    if (strcmp(value, 'auto'))
      ax.XLimMode = 'auto';
      if nargout == 1
        error(_('Invalid output argument.'));
      end
      return;
    end
    if (strcmp(value, 'manual'))
      ax.XLimMode = 'manual';
      if nargout == 1
        error(_('Invalid output argument.'));
      end
      return;
    end
    error(_('Invalid input arguments.'));
  else if (isnumeric(value) && numel(value) == 2)
      ax.XLim = value;
    else
      error(_('Invalid input arguments.'));
    end
    if nargout == 1
      error(_('Invalid output argument.'));
    end
  end
end
%=============================================================================

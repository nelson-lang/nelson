%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function hold(varargin)
  narginchk(0, 2);
  switch nargin()
    case 0
      ax = gca();
      nargs = 0;
    case 1
      ax = gca();
      opt_hold_state = varargin{1};
      nargs = 1;
    case 2
      ax = varargin{1};
      opt_hold_state = varargin{2};
      nargs = 1;
    otherwise
      error(_('Too many input arguments.'));
    end
    if ~isgraphics(ax)
      error(_('First argument must be an axes object.'));
    end
    fig = ax.Parent;
    nexta = ax.NextPlot;
    nextf = fig.NextPlot;
    hold_state = strcmp(nextf, 'add') && strcmp(nexta, 'add');
    replace_state = 'replace';
    add_state = 'add';
    
    if(nargs == 0)
      if(hold_state)
        ax.NextPlot = replace_state;
      else
        fig.NextPlot = add_state;
        ax.NextPlot = add_state;
      end
    elseif(strcmpi(opt_hold_state, 'on'))
      fig.NextPlot = add_state;
      ax.NextPlot = add_state;
    elseif(strcmpi(opt_hold_state, 'off'))
      ax.NextPlot = replace_state;
    elseif(strcmpi(opt_hold_state, 'all'))
      fig.NextPlot = add_state;
      ax.NextPlot = add_state;
    else
      error(_('Command option must be ''on'' or ''off''.'));
    end
  end
  %=============================================================================

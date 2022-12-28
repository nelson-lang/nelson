%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [linespec, colorspec, markerspec, msg] = colstyle (varargin)
  style = varargin{1};
  if nargin == 2
    raiseError = varargin{2};
  else
    raiseError = true;
  end
  if ~ischar(style) && ~isStringScalar(style)
    if raiseError
      error(_('Requires a string scalar or a character vector argument.'));
    else
      colorspec = '';
      markerspec = '';
      linespec = '';
      msg.message = _('Invalid LineSpec string.');
      msg.Identifier = 'Nelson:colstyle:InvalidLinespec';
      return
    end  
  end
  style = convertStringsToChars(style);
  giveup = 0;
  colorspec = '';
  markerspec = '';
  linespec = '';
  msg = [];
  if isempty(style)
    return;
  end
  orig_t = style;
  while (~giveup && length(style)>0)
    giveup = 1;
    if (matchit(style,colorset()))
      [colorspec, style] = parseit(style,colorset);
      giveup = 0;
    end;
    if (matchit(style,markerset()))
      [markerspec, style] = parseit(style,markerset);
      giveup = 0;
    end
    if (matchit(style,styleset()))
      [linespec, style] = parseit(style,styleset);
      giveup = 0;
    end
  end
  if (strcmp(markerspec, '') && strcmp(linespec, ''))
    linespec = '-';
  end
  if giveup
    colorspec = '';
    markerspec = '';
    linespec = '';
    msg.message = _('Invalid LineSpec string.');
    msg.Identifier = 'Nelson:colstyle:InvalidLinespec';
  end
end
%=============================================================================
function b = matchit(t,dictionary)
  b = any(stcmp(dictionary, t));
end
%=============================================================================
function [b, t] = parseit(t, dictionary)
  n = stcmp(dictionary, t);
  b = dictionary{min(find(n))};
  t(1:length(b)) = [];
end
%=============================================================================
function c = colorset()
  c = {'y','m','c','r','g','b','w','k'};
end
%=============================================================================
function c = styleset()
  c = {'--',':','-.','-'};
end
%=============================================================================
function c = markerset()
  c = {'+','o','*','.','x','square','s','diamond','d','^','v','>','<'};
end
%=============================================================================
function b = stcmp(source,pattern)
  b = zeros(size(source), 'logical');
  for i=1:numel(source)
    b(i) = strncmp(source{i},pattern,length(source{i}));
  end
end
%=============================================================================

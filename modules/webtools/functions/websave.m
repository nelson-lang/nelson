%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = websave(varargin)
  if nargin < 2
    error(_('Wrong number of input arguments.'));
  end
  filename = getArgumentAsCharactersVector(varargin{1});
  url = getArgumentAsCharactersVector(varargin{2});
  options = [];
  options_position = -1;
  for k = 2:nargin
    p = varargin{k};
    if isWebOptions(p)
      options = p;
      options_position = k;
      break;
    end
  end
  if isempty(options)
    options = weboptions();
    options_position = -1;
  end
  if ~(options_position == -1 || options_position == nargin)
    error(_('weboptions must be last argument.'));
  end
  if options_position == -1
    last = nargin;
  else
    last = nargin - 1;
  end
  st = struct();
  for k = 3:2:last
    st.(varargin{k}) = varargin{k + 1};
  end
  if strcmp(options.RequestMethod, 'auto')
    options.RequestMethod = 'get';
  end
  if strcmp(options.MediaType, 'auto') == 1
    options.MediaType = 'application/x-www-form-urlencoded';
  end
  out = webREST(url, [], filename, st, options);
end
%=============================================================================
function str = getArgumentAsCharactersVector(arg)
  str = arg;
  supported = (isstring(str) && isscalar(str)) || (ischar(str) && (size(str, 1) == 1));
  if (~supported)
    error(_('Invalid type for #1 input argument: ''char'' or ''string'' expected.'));
  end
  str = convertStringsToChars(str);
end
%=============================================================================
function r = isWebOptions(arg)
  r = strcmp(class(arg), 'weboptions');
end
%=============================================================================

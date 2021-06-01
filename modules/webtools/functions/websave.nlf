%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
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

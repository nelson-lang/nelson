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
function varargout = nmm(varargin)
    if nargin == 0
        error(_('Wrong number of input arguments.'));
    end
    varargout = {};
    firstParameter = varargin{1};
    if ~isSupportedFirstParameter(firstParameter)
        error('#1 value or type not managed.');
    end
    addpath([modulepath(nelsonroot(), 'modules_manager', 'root'), '/functions/internal']);
    try
        switch firstParameter
            case 'list'
                if nargin ~= 1
                    error(_('Wrong number of input arguments.'));
                end
                varargout{1} = nmm_list();
            case 'load'
                if nargout ~= 0
                    error(_('Wrong number of output arguments.'));
                end
                nmm_load(varargin{2:end});
            case 'autoload'
                varargout{1} = nmm_autoload(varargin{2:end});
            case 'uninstall'
                nmm_uninstall(varargin{2:end});
            case 'install'
                nmm_install(varargin{2:end});
            case 'package'
                varargout{1} = nmm_package(varargin{2:end});
            otherwise
                error('#1 value or type not managed.')
        end
        rmpath([modulepath(nelsonroot(), 'modules_manager', 'root'), '/functions/internal']);
    catch
        e = lasterror();
        rmpath([modulepath(nelsonroot(), 'modules_manager', 'root'), '/functions/internal']);
        error(e);
    end
end
%=============================================================================
function r = isSupportedFirstParameter(param)
    if ischar(param)
        supported = ["list", "load", "autoload", "install", "uninstall", "package"];
        r = any(contains(supported, param));
    else
        r = false;
    end
end
%=============================================================================

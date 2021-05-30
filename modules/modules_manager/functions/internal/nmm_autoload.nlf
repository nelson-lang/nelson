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
% r = nmm('autoload', module_name)
% r = nmm('autoload', path_name)
% r = nmm('autoload', module_name, true_false)
% r = nmm('autoload', path_name, true_false)
function res = nmm_autoload(varargin)
    res = false;
    switch length(varargin)
        case 1
            module_or_directory = varargin{1};
            if ~ischar(module_or_directory)
                error(_('#2 argument: characters expected.'));
            end
            new_state = [];
        case 2
            module_or_directory = varargin{1};
            if ~ischar(module_or_directory)
                error(_('#2 argument: characters expected.'));
            end
            new_state = varargin{2};
            if ~(islogical(new_state) && isscalar(new_state))
                error(_('#3 argument: Logical scalar expected.'));
            end
        otherwise
            error(_('Wrong number of input arguments.'));
    end

    [module_name, module_data] = nmm_find_installed_module(module_or_directory);
    if isempty(module_data)
        error(_('module name or existing directory installed is expected.'))
    end
    res = module_data.load;
    if ~isempty(new_state)
        st = nmm_list();
        module_data.load = new_state;
        st = setfield(st, module_name, module_data);
        p = usermodulesdir();
        modules_json_path = [p, 'modules.json'];
        txt = jsonprettyprint(jsonencode(st));
        filewrite(modules_json_path, txt);
    end
 end
%=============================================================================

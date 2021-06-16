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
function package_file = nmm_package(varargin)
    if (length(varargin) ~= 2)
      error(_('Wrong number of input arguments.'));
    end
    module_name = varargin{1};
    destination_dir = varargin{2};
    package_file = [];
    if ~nmm_is_installed(module_name)
      error(_('an valid module name expected.'))
    end
    if ~isdir(destination_dir)
      error(_('an existing directory expected.'))
    end
    modules = nmm_list();
    module_info = getfield(modules, module_name);
    module_path = module_info.path;
    info = nmm_read_module_json(module_path);
    package_file = buildDestinationFilename(destination_dir, module_name, info);
    zip(package_file, module_path);
end
%=============================================================================
function package_file = buildDestinationFilename(destination_dir, module_name, info)
    package_file_extension = 'nmz';
    universal = false;
    if info.builtin
        arch = computer('arch');
    else
        platforms = info.platforms;
        if ischar(platforms)
            universal = strcmp(info.platforms, 'all');
        end
    end
    if universal
        arch = 'all';
    else
        arch = computer('arch');
    end
    version = info.version;
    if endsWith(fullpath(destination_dir), '/')
      buffer_format = '%s%s-%s-%s.%s';
    else
      buffer_format = '%s/%s-%s-%s.%s';
    end
    package_file = sprintf(buffer_format, fullpath(destination_dir), module_name, arch, version, package_file_extension);
end
%=============================================================================

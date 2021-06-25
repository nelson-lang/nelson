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
function nmm_load(varargin)
   if nargin ~= 1
      error(_('Wrong number of input arguments.'));
   end
   [module_name, module_data] = nmm_find_installed_module(varargin{1});

   if isempty(module_data)
      error(_('module name or existing directory installed is expected.'))
   end
   if ~isfile([module_data.path, 'loader.m'])
      error(_('loader.m is missing.'));
   end
   if ~isfile([module_data.path, 'module.json'])
      error(_('module.json is missing.'));
   end
   if ~ismodule(module_name)
      run([module_data.path, 'loader.m']);
   else
      warning(sprintf(_('%s already loaded.'), module_name));
   end
end
%=============================================================================

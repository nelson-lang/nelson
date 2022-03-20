%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
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
% <--GUI MODE-->
% <--NO USER MODULES-->
%=============================================================================
modules_loaded = getmodules();
run([nelsonroot(), '/modules/modules.m']);
if ~ispc()
  modules_list(strcmp(modules_list,'com_engine')) = [];
end
if ~any(strcmp(getnelsonmode(), ["ADVANCED_SIO_CLIENT", "BASIC_SIO_CLIENT"]))
  modules_list(strcmp(modules_list, 'sio_client')) = [];
end
if ~ismodule('slicot')
  modules_list(strcmp(modules_list, 'slicot')) = [];
end
if ~ismodule('fftw')
  modules_list(strcmp(modules_list, 'fftw')) = [];
end
if ~ismodule('mpi')
  modules_list(strcmp(modules_list, 'mpi')) = [];
end
if ~ismodule('ipc')
  modules_list(strcmp(modules_list, 'ipc')) = [];
end
assert_isequal(modules_loaded, cellstr(modules_list));
%=============================================================================


%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
% <--NO USER MODULES-->
%=============================================================================
run([nelsonroot(), '/modules/modules.m']);
funcList = str2func('@(x) x{1}');
list = string(cellfun(funcList, modules_list, 'UniformOutput', false));
funcAvailable = str2func('@(x) x{2}');
available = cellfun(funcAvailable, modules_list, 'UniformOutput', true);
list(~available) = [];
modules_list = list;
modules_loaded = getmodules();
%=============================================================================
if ~ispc()
  modules_list(strcmp(modules_list,'com_engine')) = [];
end
if ~any(strcmp(getnelsonmode(), ["ADVANCED_SIO_CLIENT", "BASIC_SIO_CLIENT"]))
  modules_list(strcmp(modules_list, 'sio_client')) = [];
end
if ~ismodule('audio')
  modules_list(strcmp(modules_list, 'audio')) = [];
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
if ~ismodule('python_engine')
  modules_list(strcmp(modules_list, 'python_engine')) = [];
end
if ~ismodule('julia_engine')
  modules_list(strcmp(modules_list, 'julia_engine')) = [];
end
assert_isequal(modules_loaded, cellstr(modules_list));
%=============================================================================


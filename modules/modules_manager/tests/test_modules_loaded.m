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
% The WebAssembly package registers modules statically and does not ship the
% native loader scripts; the loaded-module comparison below still applies.
if ~iswasm()
  for module_name = cellstr(modules_list(:))'
    loader_filename = [nelsonroot(), '/modules/', module_name{1}, '/loader.m'];
    asserts.istrue(isfile(loader_filename));
  end
end
%=============================================================================
if ~ispc()
  modules_list(strcmp(modules_list, 'com_engine')) = [];
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
if ~ismodule('webview')
  modules_list(strcmp(modules_list, 'webview')) = [];
end
if ~ismodule('python_engine')
  modules_list(strcmp(modules_list, 'python_engine')) = [];
end
if ~ismodule('julia_engine')
  modules_list(strcmp(modules_list, 'julia_engine')) = [];
end
if strcmp(getnelsonmode(), 'GUI') && any(strcmp(modules_list, 'gui'))
  asserts.istrue(ismodule('gui'));
end
moduleNames = cellstr(modules_list(:));
keepModule = false(size(moduleNames));
for moduleIndex = 1:numel(moduleNames)
  keepModule(moduleIndex) = ismodule(moduleNames{moduleIndex});
end
modules_list = string(moduleNames(keepModule));
if (strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  asserts.istrue(all(ismember(cellstr(modules_list), modules_loaded)));
else
  asserts.isequal(modules_loaded, cellstr(modules_list));
end
%=============================================================================

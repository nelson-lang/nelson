%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = dlgetnelsonincludes()
  % Modules whose public headers form the Nelson C++ SDK used to build a
  % builtin gateway (some, like commons and nelson_manager, are support
  % libraries rather than loadable modules, so their path is derived from the
  % modules root instead of modulepath()).
  sdkModules = {'interpreter', 'types', 'error_manager', 'i18n', ...
    'characters_encoding', 'stream_manager', 'overload', 'commons', ...
    'nelson_manager', 'display_format', 'handle'};
  if isdir([modulepath('interpreter'), '/src/include'])
    modulesDir = fullpath([modulepath('interpreter'), '/..']);
    c = {};
    for k = 1:numel(sdkModules)
      c = [c; [modulesDir, '/', sdkModules{k}, '/src/include']];
    end
    if (ismodule('f2c'))
      c = [c; [modulesDir, '/f2c/src/include']];
    end
    if (ismodule('validators'))
      c = [c; [modulesDir, '/validators/src/include']];
    end
  else
    if (isdir([modulepath('nelson', 'builtin'), '/../../include/Nelson/interpreter']))
      includeRootPath = fullpath([modulepath('nelson', 'builtin'), '/../../include/Nelson/']);
      c = {};
      for k = 1:numel(sdkModules)
        c = [c; [includeRootPath, '/', sdkModules{k}]];
      end
      if (ismodule('f2c'))
        c = [c; [includeRootPath, '/f2c']];
      end
      if (ismodule('validators'))
        c = [c; [includeRootPath, '/validators']];
      end
    end
  end
end
%=============================================================================

%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function test_suite = run_impl_module(...
    moduleName, option, nbWorkers, runtimeOptions, stoponfail, nelson_version_str)
  [restoreEnvironment, previousPlatform] = configure_module_test_environment(moduleName); %#ok<NLS0004>
  restoreEnvironmentState = @() run_impl_module_restore_environment(...
    restoreEnvironment, previousPlatform);
  classname = [nelson_version_str, '.', moduleName];
  tests_dir = run_impl_module_tests_dir(moduleName);
  try
    test_suite = process_test_cases_directory(...
      tests_dir, option, classname, moduleName, nbWorkers, runtimeOptions, stoponfail);
  catch ex
    restoreEnvironmentState();
    rethrow(ex);
  end
  restoreEnvironmentState();
end
%=============================================================================
function [restoreEnvironment, previousPlatform] = configure_module_test_environment(moduleName)
  restoreEnvironment = struct('platform', false, 'audio', false);
  previousPlatform = '';
  % The audio gateway is lazy: registering it costs nothing and no device is
  % enumerated until an audio builtin is actually called. Forcing AUDIODEV=null
  % here used to make the audio loader skip the module entirely, which hid the
  % file-I/O builtins (audioread/audiowrite/audioinfo) from any test that
  % legitimately requires them (AUDIO REQUIRED tag). Leave AUDIODEV untouched so
  % an externally provided value (e.g. Docker sets it to 'null') still wins.
  if ~(ismac() && strcmp(moduleName, 'qml_engine'))
    return
  end
  previousPlatform = getenv('QT_QPA_PLATFORM');
  if ~strcmp(previousPlatform, '')
    return
  end
  setenv('QT_QPA_PLATFORM', 'offscreen');
  restoreEnvironment.platform = true;
end
%=============================================================================

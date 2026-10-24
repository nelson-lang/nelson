%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--WINDOWS ONLY-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
if ispc()
  restoreCompilerConf = onCleanup(@() evalc('configuremsvc()'));
  [status, ~] = configuremsvc();
  skip_testsuite(~status)
end
skip_testsuite(~havecompiler())
asserts.istrue(havecompiler());
arch = computer('arch');
jsonfile = [prefdir(), '/compiler_', arch, '.json'];
removed = false;
watchdog = tic();
while toc(watchdog) < 60
  removed = removecompilerconf();
  if removed && ~isfile(jsonfile)
    removed = true;
    break
  end
  pause(0.5);
end
asserts.istrue(removed);
asserts.isfalse(isfile(jsonfile));
if ispc()
  [status, ~] = configuremsvc();
  asserts.istrue(status);
end

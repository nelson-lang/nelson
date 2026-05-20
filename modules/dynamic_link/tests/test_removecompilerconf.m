%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WINDOWS ONLY-->
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
if ispc() && ~havecompiler()
    configuremsvc()
  end
skip_testsuite(~havecompiler())
%=============================================================================
if ispc()
  restoreCompilerConf = onCleanup(@() evalc('configuremsvc()'));
end
assert_istrue(havecompiler());
arch = computer('arch');
jsonfile = [prefdir(), '/compiler_', arch, '.json'];
removed = false;
for k = 1:5
  removecompilerconf();
  if ~isfile(jsonfile)
    removed = true;
    break
  end
  pause(0.2);
end
assert_istrue(removed);
assert_isfalse(isfile(jsonfile));
if ispc()
  [status, ~] = configuremsvc();
  assert_istrue(status);
end

%=============================================================================
% Copyright (c) 2018 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('usermodulesdir'), 0);
asserts.isequal(nargout('usermodulesdir'), 1);
%=============================================================================
P = getenv('NELSON_EXTERNAL_MODULES_PATH');
if isdir(P)
  REF = fullpath(P);
  if ~endsWith(REF, '/')
    REF = [REF, '/'];
  end
else
  V = version('-number');
  if iswasm()
    REF = ['/user-modules/Nelson/', sprintf('%d.%d.%d', V(1), V(2), V(3)), '/modules/'];
  else
    REF = [userdir(), 'Nelson/', sprintf('%d.%d.%d', V(1), V(2), V(3)), '/modules/'];
  end
end
R = usermodulesdir();
asserts.isequal(R, REF);
%=============================================================================
previousExternalModulesPath = getenv('NELSON_EXTERNAL_MODULES_PATH');
dynamicPath = [tempdir(), 'test_usermodulesdir_dynamic/'];
if isdir(dynamicPath)
  rmdir(dynamicPath, 's');
end
try
  setenv('NELSON_EXTERNAL_MODULES_PATH', dynamicPath);
  R = usermodulesdir();
  asserts.isequal(R, dynamicPath);
  if isempty(previousExternalModulesPath)
    setenv('NELSON_EXTERNAL_MODULES_PATH');
  else
    setenv('NELSON_EXTERNAL_MODULES_PATH', previousExternalModulesPath);
  end
  rmdir(dynamicPath, 's');
catch e
  if isempty(previousExternalModulesPath)
    setenv('NELSON_EXTERNAL_MODULES_PATH');
  else
    setenv('NELSON_EXTERNAL_MODULES_PATH', previousExternalModulesPath);
  end
  if isdir(dynamicPath)
    rmdir(dynamicPath, 's');
  end
  rethrow(e);
end
%=============================================================================

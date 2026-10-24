%=============================================================================
% Copyright (c) 2018 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
p = which('clear');
if (strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  asserts.istrue(startsWith(p, 'static:'));
else
  asserts.istrue(contains(p, 'libnlsMemory_manager_builtin'));
end
%=============================================================================
p = modulepath('core', 'builtin');
asserts.istrue(contains(p, 'libnlsCore_builtin'));
%=============================================================================
p = modulepath('core');
rootPath = nelsonroot();
if endsWith(rootPath, '/')
  rootPath = rootPath(1:end - 1);
end
ref = [rootPath, '/modules/core'];
asserts.isequal(p, ref);
%=============================================================================
p = modulepath('core', 'etc');
ref = [rootPath, '/modules/core/etc'];
asserts.isequal(p, ref);
%=============================================================================
p = modulepath('nelson', 'builtin');
if (strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm())
  asserts.istrue(isdir(p));
else
  ref = fileparts(which('clear'));
  asserts.isequal(p, ref);
end
%=============================================================================
p = modulepath('core', 'root');
ref = [rootPath, '/modules/core'];
asserts.isequal(p, ref);
%=============================================================================
p = modulepath('core', 'tests');
ref = [rootPath, '/modules/core/tests'];
asserts.isequal(p, ref);
%=============================================================================

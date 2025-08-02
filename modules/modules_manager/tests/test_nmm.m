%=============================================================================
% Copyright (c) 2018 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
r = nmm('list');
assert_isequal(class(r), 'struct');
needToRestore = any(contains(fieldnames(r), 'module_skeleton_basic'));
if isempty(needToRestore)
  needToRestore = false;
end
if needToRestore
  nmm('uninstall', 'module_skeleton_basic')
end
%=============================================================================
nmm('install', 'https://github.com/nelson-lang/module_skeleton_basic.git#v3.0.0');
r = nmm('list');
r = any(contains(fieldnames(r), 'module_skeleton_basic'));
assert_istrue(r);
assert_istrue(ismodule('module_skeleton_basic'));
%=============================================================================
nmm('load', 'module_skeleton_basic');
assert_istrue(ismodule('module_skeleton_basic'));
r = nmm('list');
assert_istrue(isstruct(r.module_skeleton_basic));
assert_istrue(isdir(r.module_skeleton_basic.path));
assert_istrue(r.module_skeleton_basic.load);
%=============================================================================
R = nmm('autoload', 'module_skeleton_basic');
assert_istrue(R);
%=============================================================================
nmm('autoload', 'module_skeleton_basic', true);
r = nmm('list');
assert_istrue(r.module_skeleton_basic.load);
R = nmm('autoload', 'module_skeleton_basic');
assert_istrue(R);
%=============================================================================
filename = nmm('package','module_skeleton_basic', tempdir());
assert_istrue(isfile(filename));
%=============================================================================
nmm('uninstall','module_skeleton_basic');
r = nmm('list');
r = contains(fieldnames(r), 'module_skeleton_basic');
if isempty(r)
  r = false;
end
assert_isfalse(r);
%=============================================================================
nmm('install', [modulepath('modules_manager', 'tests'), '/module_skeleton_basic-all-3.0.0.nmz']);
r = nmm('list');
r = any(contains(fieldnames(r), 'module_skeleton_basic'));
assert_istrue(r);
nmm('uninstall','module_skeleton_basic');
r = nmm('list');
r = contains(fieldnames(r), 'module_skeleton_basic');
if isempty(r)
  r = false;
end
assert_isfalse(r);
%=============================================================================
if needToRestore
  nmm('install', 'https://github.com/nelson-lang/module_skeleton_basic.git#v3.0.0');
end
%=============================================================================

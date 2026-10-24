%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.istrue(ismodule('compiler'));
directory = modulepath('compiler');
asserts.isequal(fullpath(directory), fullpath(fullfile(nelsonroot(), 'modules', 'compiler')));
gateway = gatewayinfo(modulepath('compiler', 'builtin'));
asserts.isequal(gateway, 'compiler');
asserts.istrue(contains(which('__nelsonc_plan__'), 'libnlsCompiler_builtin'));
asserts.istrue(contains(which('__nelsonc_archive__'), 'libnlsCompiler_builtin'));
asserts.istrue(contains(which('__application_archive__'), 'libnlsFile_archiver_builtin'));
% The compiler is a startup module; deployed applications skip it in loader.m.
modules_list = startup_modules();
enabled = false;
for k = 1:numel(modules_list)
  item = modules_list{k};
  if strcmp(item{1}, 'compiler')
    enabled = logical(item{2});
  end
end
asserts.istrue(enabled);

function modules_list = startup_modules()
  run(fullfile(nelsonroot(), 'modules', 'modules.m'), false);
end

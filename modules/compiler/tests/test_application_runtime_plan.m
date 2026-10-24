%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
entry = fullfile(nelsonroot(), 'modules', 'main', 'tests', 'fixtures', 'nelsonc', 'app_entry.m');
application = jsondecode(__nelsonc_plan__('link', entry));
beforePath = path();
beforeDirectory = pwd();
runtime = __nelsonc_runtime_plan__(application, false);
asserts.isequal(path(), beforePath);
asserts.isequal(pwd(), beforeDirectory);
asserts.isequal(runtime.format, 1);
asserts.istrue(numel(runtime.files) > numel(runtime.libraries));
asserts.istrue(numel(runtime.inputs) >= numel(runtime.bootstrapFiles));
asserts.isequal(numel(runtime.moduleOrder), numel(runtime.modules));
asserts.isfalse(runtime.gui);
asserts.isfalse(any(strcmp(runtime.modules, 'graphics')));
asserts.isfalse(any(strcmp(runtime.modules, 'audio')));
filenames = {runtime.libraries.filename};
for name = {'Engine', 'Interpreter', 'Core_builtin', 'Modules_manager_builtin', 'String_builtin', ...
  'Interpreter_builtin', 'File_archiver_builtin'}
  asserts.istrue(any(strcmp(filenames, ['libnls', name{1}, getdynlibext()])));
end
asserts.isequal(numel(unique(filenames)), numel(filenames));
for k = 1:numel(runtime.modules)
  name = runtime.modules{k};
  if ismodule(name)
    gateway = modulepath(name, 'builtin');
    if isfile(gateway)
      [~, stem, extension] = fileparts(gateway);
      asserts.istrue(any(strcmp(filenames, [stem, extension])));
    end
  end
end
for k = 1:numel(runtime.libraries)
  item = runtime.libraries(k);
  asserts.istrue(isfile(item.source));
  asserts.isequal(sha256(item.source, '-file'), item.sha256);
  imports = [item.needed(:); item.delayed(:)];
  for j = 1:numel(imports)
    [~, stem, extension] = fileparts(imports{j});
    name = [stem, extension];
    if ispc()
      packaged = any(strcmpi(filenames, name));
      system = any(strcmpi(runtime.systemLibraries, imports{j}));
    else
      packaged = any(strcmp(filenames, name));
      system = any(strcmp(runtime.systemLibraries, imports{j}));
    end
    asserts.istrue(packaged || system);
  end
end

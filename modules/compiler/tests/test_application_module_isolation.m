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
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
entry = fullfile(directory, 'dynamic_entry.m');
filewrite(entry, 'function result = dynamic_entry(name); result = feval(name); end');
plan = jsondecode(__nelsonc_plan__('link', entry));
asserts.istrue(plan.dynamicFallback);
asserts.isfalse(any(strcmp({plan.modules.name}, 'compiler')));
asserts.isfalse(contains(jsonencode(plan.builtinCatalog), '__nelsonc_plan__'));
asserts.isfalse(contains(jsonencode(plan.builtinCatalog), '__nelsonc_executable__'));
runtime = jsondecode(__nelsonc_plan__('runtime', jsonencode(plan), true));
asserts.isfalse(any(strcmp(runtime.modules, 'compiler')));
asserts.isfalse(contains(lower(jsonencode(runtime.libraries)), 'libnlscompiler'));
for k = 1:numel(plan.files)
  asserts.isfalse(strcmp(plan.files(k).module, 'compiler'));
end
plan.modules(end + 1) = struct('name', 'compiler', 'complete', true);
assert_build_module_rejected(@() __nelsonc_plan__('runtime', jsonencode(plan), true));
filewrite(entry, 'function dynamic_entry(); __nelsonc_plan__(''link'', ''entry.m''); end');
assert_build_module_rejected(@() __nelsonc_plan__('link', entry));
filewrite(entry, 'function dynamic_entry(); ncc(''--help''); end');
assert_build_module_rejected(@() __nelsonc_plan__('link', entry));
filewrite(entry, 'function dynamic_entry(); __nelsonc_executable__(''validate-version'', ''1''); end');
assert_build_module_rejected(@() __nelsonc_plan__('link', entry));
clear cleanup;

function assert_build_module_rejected(operation)
  identifier = '';
  message = '';
  try
    operation();
  catch ex
    identifier = ex.identifier;
    message = ex.message;
  end
  asserts.isequal(identifier, 'Nelson:nelsonc:invalidPlan');
  asserts.istrue(contains(message, 'build-only compiler'));
end

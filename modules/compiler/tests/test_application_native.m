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
source = fullfile(directory, 'native_probe.m');
write_text(source, sprintf('function value = native_probe()\nvalue = 17;\nend\n'));
plan = __nelsonc_application_native__(jsondecode(__nelsonc_plan__('link', source, {})));
asserts.istrue(isempty(plan.nativeLibraries));
asserts.istrue(isempty(plan.nativeRuntimeLibraries));
if ispc()
  native = fullfile(directory, ['native_compute.', mexext()]);
  write_text(native, 'invalid native binary');
  write_text(fullfile(directory, 'native_compute.m'), 'error(''Source fallback must not be chosen.'');');
  write_text(source, sprintf('function value = native_probe()\nvalue = native_compute(7);\nend\n'));
  plan = jsondecode(__nelsonc_plan__('link', source, {}));
  asserts.istrue(plan.complete);
  index = find(strcmp({plan.files.representation}, 'native'), 1);
  asserts.istrue(~isempty(index));
  asserts.istrue(endsWith(plan.files(index).source, ['native_compute.', mexext()]));
  asserts.istrue(any(strcmp({plan.modules.name}, 'mex')));
  asserts.istrue(any(strcmp({plan.modules.name}, 'dynamic_link')));
  assert_rejected(@() __nelsonc_application_native__(plan), 'Nelson:nelsonc:nativeLibrary');
  library = fullfile(directory, 'native_probe.dll');
  copyfile(fullfile(modulepath('nelson', 'builtin'), 'libnlsCore.dll'), library);
  write_text(source, sprintf('function value = native_probe()\nvalue = dlopen(''native_probe.dll'');\nend\n'));
  plan = jsondecode(__nelsonc_plan__('link', source, {}));
  asserts.istrue(plan.complete);
  asserts.istrue(any(strcmp({plan.files.representation}, 'native')));
  disabled = jsondecode(__nelsonc_plan__('link', source, {}, false));
  asserts.istrue(disabled.complete);
  asserts.istrue(any(strcmp({disabled.files.representation}, 'native')));
  asserts.istrue(any(endsWith(string({disabled.files.source}), '/native_probe.dll')));
  missingSource = fullfile(directory, 'missing_native_entry.m');
  write_text(missingSource, 'function missing_native_entry(); dlopen(''missing_native.dll''); end');
  missing = jsondecode(__nelsonc_plan__('link', missingSource, {}, false));
  asserts.isfalse(missing.complete);
  asserts.istrue(any(strcmp({missing.diagnostics.name}, 'missing_native.dll')));
  frozen = __nelsonc_application_native__(plan);
  asserts.istrue(any(strcmp({frozen.nativeLibraries.filename}, 'native_probe.dll')));
  asserts.isfalse(isempty(frozen.nativeRuntimeLibraries));
  payload = fullfile(directory, 'payload');
  mkdir(payload);
  manifest = __nelsonc_stage_application__(frozen, payload, false);
  asserts.isequal(manifest.nativePaths, {'roots/1'});
  asserts.istrue(isfile(fullfile(payload, 'roots/1/native_probe.dll')));
  otherDirectory = fullfile(directory, 'duplicate');
  mkdir(otherDirectory);
  duplicate = fullfile(otherDirectory, 'native_probe.dll');
  copyfile(library, duplicate);
  % Identical copies share one entry; a differing copy under the same filename conflicts.
  fid = fopen(duplicate, 'ab');
  fwrite(fid, uint8(0));
  fclose(fid);
  libraryDirectory = modulepath('nelson', 'builtin');
  assert_rejected(@() __nelsonc_native_closure__({library, duplicate}, libraryDirectory), 'Nelson:nelsonc:nativeConflict:conflictingNativeLibraries');
  fid = fopen(duplicate, 'r+', 'ieee-le');
  fseek(fid, 60, 'bof');
  offset = fread(fid, 1, 'uint32');
  fseek(fid, offset + 4, 'bof');
  fwrite(fid, 332, 'uint16');
  fclose(fid);
  assert_rejected(@() __nelsonc_native_closure__({duplicate}, libraryDirectory), 'Nelson:nelsonc:architecture');
  write_text(library, 'changed native input');
  assert_rejected(@() __nelsonc_stage_application__(frozen, fullfile(directory, 'changed'), false), 'Nelson:nelsonc:sourceChanged');
end
clear cleanup;

function write_text(filename, text)
  fid = fopen(filename, 'wt');
  closer = onCleanup(@() fclose(fid));
  fprintf(fid, '%s', text);
  clear closer;
end

function assert_rejected(callback, expected)
  identifier = '';
  try
    callback();
  catch ex
    identifier = ex.identifier;
  end
  asserts.isequal(identifier, expected);
end

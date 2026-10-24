%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ispc(); skip_testsuite('Native icon resources require Windows.'); end
ncc('--help');
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
stub = fullfile(modulepath('nelson', 'bin'), 'nelson-app.exe');
target = fullfile(directory, 'launcher.exe');
copyfile(stub, target);
digest = sha256(target, '-file');
for images = {[], {}, {'missing.png'}, repmat({'missing.png'}, 1, 7), repmat({'missing.png'}, 1, 8)}
  rejected(@() __nelsonc_executable__('set-icon', target, images{1}));
  asserts.isequal(sha256(target, '-file'), digest);
end
__nelsonc_executable__('set-log-file', target, 'output.log');
digest = sha256(target, '-file');
rejected(@() __nelsonc_executable__('set-icon', target, repmat({'missing.png'}, 1, 7)));
rejected(@() __nelsonc_executable__('set-version', target, '2.0'));
asserts.isequal(sha256(target, '-file'), digest);
clear cleanup;

function rejected(callback)
  failed = false;
  try; callback(); catch; failed = true; end
  asserts.istrue(failed);
end

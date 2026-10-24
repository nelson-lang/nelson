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
runtime = struct('modules', {{'core'}});
asserts.isequal(__nelsonc_runtime_backends__(runtime, directory), {});
runtime.modules{end + 1} = 'fftw';
expect_missing(runtime, directory);
ext = getdynlibext();
first = fullfile(directory, ['libfftw3-3', ext]);
second = fullfile(directory, ['libfftw3f-3', ext]);
touch(first);
expect_missing(runtime, directory);
alternateFirst = fullfile(directory, ['libfftw3', ext]);
alternateSecond = fullfile(directory, ['libfftw3f', ext]);
touch(alternateSecond);
expect_missing(runtime, directory);
touch(alternateFirst);
asserts.isequal(__nelsonc_runtime_backends__(runtime, directory), {alternateFirst; alternateSecond});
touch(second);
asserts.isequal(__nelsonc_runtime_backends__(runtime, directory), {first; second});
clear cleanup;

function touch(filename)
  fid = fopen(filename, 'wt');
  asserts.istrue(fid >= 0);
  fprintf(fid, 'Synthetic backend fixture; never load this file.');
  fclose(fid);
end

function expect_missing(runtime, directory)
  failed = false;
  try
    __nelsonc_runtime_backends__(runtime, directory);
  catch ex
    failed = true;
    asserts.isequal(ex.identifier, 'Nelson:nelsonc:runtimeBackend');
  end
  asserts.istrue(failed);
end

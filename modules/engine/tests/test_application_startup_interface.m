%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()) || isempty(which('getdynlibext')), ...
  'Native dynamic-library application startup interface test is not available.');
filename = fullfile(modulepath('nelson', 'builtin'), ['libnlsEngine', getdynlibext()]);
library = dlopen(filename);
entry = dlsym(library, 'StartNelsonApplicationV1', 'int32', {'uint8Ptr'});
beforeDirectory = pwd();
beforeEnvironment = getenv('NELSON_ROOT_PATH');
asserts.isequal(dlcall(entry, uint8([])), int32(2));
% An invalid size must be rejected before reading the rest of the structure.
invalid = zeros(1, 4, 'uint8');
asserts.isequal(dlcall(entry, invalid), int32(2));
asserts.isequal(pwd(), beforeDirectory);
asserts.isequal(getenv('NELSON_ROOT_PATH'), beforeEnvironment);
delete(entry);
delete(library);

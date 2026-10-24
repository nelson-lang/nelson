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
previousEnvironment = getenv('LD_LIBRARY_PATH');
cleanup = onCleanup(@() setenv('LD_LIBRARY_PATH', previousEnvironment));
setenv('LD_LIBRARY_PATH', '');
origin = fullpath(tempdir());
source = fullfile(origin, 'libparent.so');
legacy = fullpath(fullfile(origin, 'legacy'));
modern = fullpath(fullfile(origin, 'modern'));
ancestor = fullpath(fullfile(origin, 'ancestor'));
info = struct('rpaths', {{'$ORIGIN/legacy'}}, 'runpaths', {{}});
[paths, child] = __nelsonc_elf_search_paths__(info, source, {ancestor});
asserts.isequal(paths, {legacy; ancestor});
asserts.isequal(child, paths);
info.runpaths = {'${ORIGIN}/modern'};
[paths, child] = __nelsonc_elf_search_paths__(info, source, {ancestor});
asserts.isequal(paths, {modern});
asserts.isequal(child, {ancestor});
info.runpaths = {''};
[paths, child] = __nelsonc_elf_search_paths__(info, source, {ancestor});
asserts.isequal(paths, {fullpath(pwd())});
asserts.isequal(child, {ancestor});
info.runpaths = {'$ORIGIN/modern::$ORIGIN/legacy'};
[paths, child] = __nelsonc_elf_search_paths__(info, source, {});
asserts.isequal(paths, {modern; fullpath(pwd()); legacy});
setenv('LD_LIBRARY_PATH', 'elf_env_a;elf_env_b');
environmentPaths = {fullpath('elf_env_a'); fullpath('elf_env_b')};
info.runpaths = {};
[paths, child] = __nelsonc_elf_search_paths__(info, source, {ancestor});
asserts.isequal(paths, [{legacy; ancestor}; environmentPaths]);
asserts.isequal(child, {legacy; ancestor});
info.runpaths = {'$ORIGIN/modern'};
[paths, child] = __nelsonc_elf_search_paths__(info, source, {ancestor});
asserts.isequal(paths, [environmentPaths; {modern}]);
asserts.isequal(child, {ancestor});
setenv('LD_LIBRARY_PATH', '$ORIGIN/env');
arguments = argv();
executableOrigin = fileparts(fullpath(arguments{1}));
[paths, child] = __nelsonc_elf_search_paths__(info, source, {});
asserts.isequal(paths{1}, fullpath(fullfile(executableOrigin, 'env')));
asserts.istrue(isempty(child));
asserts.isequal(__nelsonc_elf_expand_origin__('${ORIGIN}/lib', origin), [origin, '/lib']);
asserts.isequal(__nelsonc_elf_expand_origin__('$ORIGIN', origin), origin);
asserts.isequal(__nelsonc_elf_expand_origin__('$ORIGIN_SUFFIX/lib', origin), '$ORIGIN_SUFFIX/lib');
literalOrigin = [origin, '/literal-$ORIGIN/$LIB'];
asserts.isequal(__nelsonc_elf_expand_origin__('${ORIGIN}/lib', literalOrigin), [literalOrigin, '/lib']);
asserts.isequal(__nelsonc_elf_expand_origin__('${ORIGIN}/a/$ORIGIN', literalOrigin), ...
  [literalOrigin, '/a/', literalOrigin]);
for token = {'$LIB/lib.so', '${LIB}/lib.so', '$PLATFORM', '${PLATFORM}/lib.so'}
  identifier = '';
  try
    __nelsonc_elf_expand_origin__(token{1}, origin);
  catch ex
    identifier = ex.identifier;
  end
  asserts.isequal(identifier, 'Nelson:nelsonc:nativeToken');
end
setenv('LD_LIBRARY_PATH', '');
info.runpaths = {repmat(':', 1, 4096)};
identifier = '';
try
  __nelsonc_elf_search_paths__(info, source, {});
catch ex
  identifier = ex.identifier;
end
asserts.isequal(identifier, 'Nelson:nelsonc:nativeLimit');
clear cleanup;

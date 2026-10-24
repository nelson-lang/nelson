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
previousEnvironment = getenv('LD_LIBRARY_PATH');
restore = onCleanup(@() setenv('LD_LIBRARY_PATH', previousEnvironment));
setenv('LD_LIBRARY_PATH', '');
runtime = fullfile(directory, 'runtime');
application = fullfile(directory, 'application');
preferred = fullfile(application, 'preferred');
child = fullfile(runtime, 'child');
mkdir(runtime); mkdir(application); mkdir(preferred); mkdir(child);
engine = fullfile(runtime, ['libnlsEngine', getdynlibext()]);
write_elf(engine, {}, {}, {});
leafName = 'libnc_elf_fixture_leaf.so.1';
leaf = fullfile(preferred, leafName);
write_elf(leaf, {}, {}, {});
write_elf(fullfile(application, leafName), {'missing_decoy_dependency.so'}, {}, {});
write_elf(fullfile(runtime, leafName), {'missing_runtime_decoy.so'}, {}, {});
entry = fullfile(application, 'entry.so');
write_elf(entry, {leafName}, {}, {'$ORIGIN/preferred'});
libraries = __nelsonc_native_closure__({entry}, runtime);
assert_source(libraries, leafName, leaf);
asserts.isequal(numel(libraries), 2);
delete(fullfile(runtime, leafName));
middle = fullfile(runtime, 'middle.so');
write_elf(middle, {leafName}, {}, {});
write_elf(entry, {'middle.so'}, {'$ORIGIN/preferred'}, {});
libraries = __nelsonc_native_closure__({entry}, runtime);
assert_source(libraries, leafName, leaf);
asserts.isequal(numel(libraries), 3);
write_elf(entry, {'middle.so'}, {}, {'$ORIGIN/preferred'});
assert_identifier(@() __nelsonc_native_closure__({entry}, runtime), 'Nelson:nelsonc:nativeMissing');
write_elf(fullfile(child, leafName), {}, {}, {});
write_elf(middle, {leafName}, {}, {'$ORIGIN/child'});
write_elf(entry, {'middle.so'}, {'$ORIGIN/preferred'}, {});
libraries = __nelsonc_native_closure__({entry}, runtime);
assert_source(libraries, leafName, fullfile(child, leafName));
write_elf(entry, {['${ORIGIN}/preferred/', leafName]}, {}, {});
libraries = __nelsonc_native_closure__({entry}, runtime);
assert_source(libraries, leafName, leaf);
write_elf(fullfile(runtime, leafName), {}, {}, {});
write_elf(entry, {['$ORIGIN/absent/', leafName]}, {}, {});
assert_identifier(@() __nelsonc_native_closure__({entry}, runtime), 'Nelson:nelsonc:nativeMissing');
write_elf(entry, {'$LIB'}, {}, {});
assert_identifier(@() __nelsonc_native_closure__({entry}, runtime), 'Nelson:nelsonc:nativeToken');
clear restore;
clear cleanup;

function assert_source(libraries, name, source)
  index = find(strcmp({libraries.filename}, name));
  asserts.isequal(numel(index), 1);
  asserts.isequal(libraries(index).source, fullpath(source));
end

function assert_identifier(callback, expected)
  identifier = '';
  try
    callback();
  catch ex
    identifier = ex.identifier;
  end
  asserts.isequal(identifier, expected);
end

function write_elf(filename, needed, rpaths, runpaths)
  bytes = zeros(2048, 1, 'uint8');
  bytes(1:7) = uint8([127; 69; 76; 70; 2; 1; 1]);
  bytes = put_word(bytes, 16, 3, 2);
  bytes = put_word(bytes, 18, 62, 2);
  bytes = put_word(bytes, 32, 64, 8);
  bytes = put_word(bytes, 54, 56, 2);
  bytes = put_word(bytes, 56, 2, 2);
  bytes = put_word(bytes, 64, 1, 4);
  bytes = put_word(bytes, 80, 4096, 8);
  bytes = put_word(bytes, 96, numel(bytes), 8);
  bytes = put_word(bytes, 120, 2, 4);
  bytes = put_word(bytes, 128, 256, 8);
  bytes = put_word(bytes, 136, 4352, 8);
  tags = [5, 10];
  values = [5120, 1024];
  groups = {needed, rpaths, runpaths};
  kinds = [1, 15, 29];
  offset = 1;
  for k = 1:numel(groups)
    strings = groups{k};
    for j = 1:numel(strings)
      text = strings{j};
      tags(end + 1) = kinds(k);
      values(end + 1) = offset;
      bytes(1024 + offset + 1:1024 + offset + length(text)) = uint8(text)';
      offset = offset + length(text) + 1;
    end
  end
  tags(end + 1) = 0;
  values(end + 1) = 0;
  bytes = put_word(bytes, 152, numel(tags) * 16, 8);
  for k = 1:numel(tags)
    bytes = put_word(bytes, 256 + (k - 1) * 16, tags(k), 8);
    bytes = put_word(bytes, 264 + (k - 1) * 16, values(k), 8);
  end
  fid = fopen(filename, 'wb');
  closer = onCleanup(@() fclose(fid));
  fwrite(fid, bytes, 'uint8');
  clear closer;
end

function bytes = put_word(bytes, offset, value, width)
  for k = 0:width - 1
    bytes(offset + k + 1) = uint8(bitand(bitshift(uint64(value), -8 * k), uint64(255)));
  end
end

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================

directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
filename = fullfile(directory, 'library.bin');
for wide = [false, true]
  bytes = pe_image(wide);
  write_bytes(filename, bytes);
  info = jsondecode(__library_dependencies__(filename));
  asserts.isequal(info.format, 'PE');
  asserts.isequal(info.needed, {'fixture.dll'});
  asserts.isequal(info.delayed, {'delay.dll'});
end
for big = [false, true]
  bytes = elf_image(big);
  write_bytes(filename, bytes);
  info = jsondecode(__library_dependencies__(filename));
  asserts.isequal(info.format, 'ELF');
  asserts.isequal(info.needed, {'libfixture.so'});
  asserts.isequal(info.searchPaths, {'$ORIGIN'});
  asserts.isequal(info.identity, 'libself.so');
  asserts.istrue(isempty(info.rpaths));
  asserts.isequal(info.runpaths, {'$ORIGIN'});
  bytes = macho_image(big);
  write_bytes(filename, bytes);
  info = jsondecode(__library_dependencies__(filename));
  asserts.isequal(info.format, 'Mach-O');
  asserts.isequal(info.needed, {'@rpath/libfixture.dylib'});
  asserts.isequal(info.searchPaths, {'@loader_path'});
  asserts.isequal(info.identity, '@rpath/libself.dylib');
end
bytes = elf_image(false);
bytes = word(bytes, 152, 112, 8, false);
bytes = word(bytes, 336, 15, 8, false);
bytes = word(bytes, 344, 96, 8, false);
bytes = string_at(bytes, 608, '$ORIGIN/legacy');
write_bytes(filename, bytes);
info = jsondecode(__library_dependencies__(filename));
asserts.isequal(info.rpaths, {'$ORIGIN/legacy'});
asserts.isequal(info.runpaths, {'$ORIGIN'});
asserts.isequal(info.searchPaths, info.runpaths);
bytes(545) = uint8(0);
write_bytes(filename, bytes);
info = jsondecode(__library_dependencies__(filename));
asserts.isequal(info.runpaths, {''});
asserts.isequal(info.searchPaths, {''});
images = {pe_image(true), elf_image(false), macho_image(false)};
for k = 1:numel(images)
  bytes = images{k};
  for count = [0, 3, 16, 100]
    write_bytes(filename, bytes(1:count));
    assert_rejected(@() __library_dependencies__(filename));
  end
end
bytes = pe_image(true);
bytes = word(bytes, 60, 4294967295, 4, false);
write_bytes(filename, bytes);
assert_rejected(@() __library_dependencies__(filename));
bytes = elf_image(false);
bytes = word(bytes, 56, 65535, 2, false);
write_bytes(filename, bytes);
assert_rejected(@() __library_dependencies__(filename));
bytes = macho_image(false);
bytes = word(bytes, 36, 4294967295, 4, false);
write_bytes(filename, bytes);
assert_rejected(@() __library_dependencies__(filename));
native = fullfile(modulepath('nelson', 'builtin'), ['libnlsEngine', getdynlibext()]);
actual = jsondecode(__library_dependencies__(native));
asserts.istrue(any(strcmp(actual.architecture, {'x86_64', 'aarch64'})));
asserts.isfalse(isempty(actual.needed));
if ispc()
  asserts.istrue(any(strcmpi(actual.needed, 'libnlsCore.dll')));
end
clear cleanup;

function bytes = pe_image(wide)
  bytes = zeros(1024, 1, 'uint8');
  bytes = word(bytes, 0, 23117, 2, false);
  bytes = word(bytes, 60, 128, 4, false);
  bytes = word(bytes, 128, 17744, 4, false);
  if wide
    machine = 34404; magic = 523; table = 112;
  else
    machine = 332; magic = 267; table = 96;
  end
  bytes = word(bytes, 132, machine, 2, false);
  bytes = word(bytes, 134, 1, 2, false);
  bytes = word(bytes, 148, 240, 2, false);
  bytes = word(bytes, 152, magic, 2, false);
  bytes = word(bytes, 212, 512, 4, false);
  bytes = word(bytes, 152 + table - 4, 16, 4, false);
  bytes = word(bytes, 152 + table + 8, 4096, 4, false);
  bytes = word(bytes, 152 + table + 12, 40, 4, false);
  bytes = word(bytes, 152 + table + 104, 4352, 4, false);
  bytes = word(bytes, 152 + table + 108, 64, 4, false);
  bytes = word(bytes, 404, 4096, 4, false);
  bytes = word(bytes, 408, 512, 4, false);
  bytes = word(bytes, 412, 512, 4, false);
  bytes = word(bytes, 524, 4224, 4, false);
  bytes = string_at(bytes, 640, 'fixture.dll');
  bytes = word(bytes, 768, 1, 4, false);
  bytes = word(bytes, 772, 4480, 4, false);
  bytes = string_at(bytes, 896, 'delay.dll');
end

function bytes = elf_image(big)
  bytes = zeros(1024, 1, 'uint8');
  bytes(1:7) = uint8([127; 69; 76; 70; 2; 1 + big; 1]);
  bytes = word(bytes, 16, 3, 2, big);
  bytes = word(bytes, 18, 62, 2, big);
  bytes = word(bytes, 32, 64, 8, big);
  bytes = word(bytes, 54, 56, 2, big);
  bytes = word(bytes, 56, 2, 2, big);
  bytes = word(bytes, 64, 1, 4, big);
  bytes = word(bytes, 80, 4096, 8, big);
  bytes = word(bytes, 96, 1024, 8, big);
  bytes = word(bytes, 120, 2, 4, big);
  bytes = word(bytes, 128, 256, 8, big);
  bytes = word(bytes, 136, 4352, 8, big);
  bytes = word(bytes, 152, 96, 8, big);
  tags = [5, 10, 1, 29, 14, 0];
  values = [4608, 128, 1, 32, 64, 0];
  for k = 1:numel(tags)
    bytes = word(bytes, 256 + (k - 1) * 16, tags(k), 8, big);
    bytes = word(bytes, 264 + (k - 1) * 16, values(k), 8, big);
  end
  bytes = string_at(bytes, 513, 'libfixture.so');
  bytes = string_at(bytes, 544, '$ORIGIN');
  bytes = string_at(bytes, 576, 'libself.so');
end

function bytes = macho_image(big)
  bytes = zeros(512, 1, 'uint8');
  bytes = word(bytes, 0, 4277009103, 4, big);
  bytes = word(bytes, 4, 16777223, 4, big);
  bytes = word(bytes, 12, 6, 4, big);
  bytes = word(bytes, 16, 3, 4, big);
  bytes = word(bytes, 20, 160, 4, big);
  bytes = word(bytes, 32, 12, 4, big);
  bytes = word(bytes, 36, 64, 4, big);
  bytes = word(bytes, 40, 24, 4, big);
  bytes = string_at(bytes, 56, '@rpath/libfixture.dylib');
  bytes = word(bytes, 96, 2147483676, 4, big);
  bytes = word(bytes, 100, 32, 4, big);
  bytes = word(bytes, 104, 12, 4, big);
  bytes = string_at(bytes, 108, '@loader_path');
  bytes = word(bytes, 128, 13, 4, big);
  bytes = word(bytes, 132, 64, 4, big);
  bytes = word(bytes, 136, 24, 4, big);
  bytes = string_at(bytes, 152, '@rpath/libself.dylib');
end

function bytes = word(bytes, offset, value, width, big)
  for k = 0:width - 1
    order = k;
    if big
      order = width - k - 1;
    end
    bytes(offset + k + 1) = uint8(bitand(bitshift(uint64(value), -8 * order), uint64(255)));
  end
end

function bytes = string_at(bytes, offset, text)
  bytes(offset + 1:offset + length(text)) = uint8(text)';
end

function write_bytes(filename, bytes)
  fid = fopen(filename, 'wb');
  fwrite(fid, bytes, 'uint8');
  fclose(fid);
end

function assert_rejected(callback)
  rejected = false;
  try
    callback();
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
end

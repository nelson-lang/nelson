%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nelson.io.zstd: Zstandard data in memory and .zst files.
%=============================================================================
asserts.istrue(islogical(nelson.io.zstd.isavailable()));
asserts.istrue(isstring(nelson.io.zstd.version()));
if ~nelson.io.zstd.isavailable()
  asserts.isequal(nelson.io.zstd.version(), "");
  asserts.checkerror('nelson.io.zstd.compress(''x'')', 'Zstandard is not available in this build.', ...
    'Nelson:io:zstd:notAvailable');
  return
end
%=============================================================================
% In memory: text, bytes, levels, several frames.
text = repmat('Zstandard ', 1, 500);
packed = nelson.io.zstd.compress(text);
asserts.istrue(isa(packed, 'uint8'));
asserts.istrue(numel(packed) < numel(text));
asserts.isequal(char(nelson.io.zstd.decompress(packed)), text);
bytes = uint8(mod(0:9999, 251));
asserts.isequal(nelson.io.zstd.decompress(nelson.io.zstd.compress(bytes, 'Level', 19)), bytes);
asserts.isequal(nelson.io.zstd.decompress(nelson.io.zstd.compress(bytes, 'Level', -5)), bytes);
asserts.isequal(nelson.io.zstd.decompress(nelson.io.zstd.compress(uint8([]))), uint8(zeros(1, 0)));
frames = [nelson.io.zstd.compress('ab'), nelson.io.zstd.compress("cd")];
asserts.isequal(char(nelson.io.zstd.decompress(frames)), 'abcd');
% Text decoding.
asserts.isequal(nelson.io.zstd.decompresstext(nelson.io.zstd.compress('héllo')), 'héllo');
latin = nelson.io.zstd.compress(unicode2native('café', 'ISO-8859-1'));
asserts.isequal(nelson.io.zstd.decompresstext(latin, 'Encoding', 'ISO-8859-1'), 'café');
asserts.isequal(nelson.io.zstd.decompresstext(packed, 'MaxSize', numel(text)), text);
asserts.checkerror('nelson.io.zstd.decompresstext(packed, ''MaxSize'', 10)', ...
  'Decompressed data exceeds MaxSize.', 'Nelson:io:zstd:tooLarge');
%=============================================================================
% Limits and damaged data.
asserts.checkerror('nelson.io.zstd.decompress(packed, ''MaxSize'', 10)', 'Decompressed data exceeds MaxSize.', ...
  'Nelson:io:zstd:tooLarge');
asserts.isequal(numel(nelson.io.zstd.decompress(packed, 'MaxSize', numel(text))), numel(text));
truncated = packed(1:numel(packed) - 4);
asserts.checkerror('nelson.io.zstd.decompress(truncated)', ...
  'Not valid Zstandard data: truncated or empty data', 'Nelson:io:zstd:invalidData');
asserts.checkerror('nelson.io.zstd.decompress(uint8([]))', ...
  'Not valid Zstandard data: truncated or empty data', 'Nelson:io:zstd:invalidData');
damaged = packed;
damaged(end) = bitxor(damaged(end), uint8(255));
try
  nelson.io.zstd.decompress(damaged);
  failed = false;
catch e
  failed = strcmp(e.identifier, 'Nelson:io:zstd:invalidData');
end
asserts.istrue(failed);
try
  nelson.io.zstd.compress('x', 'Level', 99);
  levelError = '';
catch e
  levelError = e.identifier;
end
asserts.isequal(levelError, 'Nelson:io:zstd:invalidLevel');
asserts.checkerror('nelson.io.zstd.decompress(packed, ''MaxSize'', -1)', ...
  'MaxSize must be Inf or a nonnegative integer.', 'Nelson:io:invalidMaxSize');
asserts.checkerror('nelson.io.zstd.compress({1})', 'Data must be uint8, char or a string scalar.', ...
  'Nelson:io:invalidData');
%=============================================================================
% Files.
WORK = [tempdir(), createGUID()];
mkdir(WORK);
source = [WORK, '/data.txt'];
filewrite(source, text);
out = nelson.io.zstd.compressfile(source);
asserts.isequal(out, [source, '.zst']);
asserts.istrue(isfile(out));
delete(source);
back = nelson.io.zstd.decompressfile(out);
asserts.isequal(back, source);
asserts.isequal(fileread(back), text);
custom = nelson.io.zstd.compressfile(source, [WORK, '/custom.bin'], 'Level', 3);
asserts.isequal(custom, [WORK, '/custom.bin']);
restored = nelson.io.zstd.decompressfile(custom, [WORK, '/restored.txt']);
asserts.isequal(fileread(restored), text);
asserts.checkerror('nelson.io.zstd.decompressfile(custom)', ...
  'A destination is needed when the source does not end with .zst.', 'Nelson:io:zstd:destinationExpected');
asserts.checkerror('nelson.io.zstd.decompressfile([WORK, ''/missing.zst''])', ...
  ['Cannot read file: ', WORK, '/missing.zst'], 'Nelson:io:zstd:cannotRead');
asserts.checkerror('nelson.io.zstd.decompressfile(source, [WORK, ''/bad.txt''])', ...
  'Not valid Zstandard data: Unknown frame descriptor', 'Nelson:io:zstd:invalidData');
asserts.isfalse(isfile([WORK, '/bad.txt']));
%=============================================================================
rmdir(WORK, 's');
%=============================================================================

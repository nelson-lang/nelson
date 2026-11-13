%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Decompression bomb protections: overlapping zip entries, declared sizes against
% MaxExtractedSize and free disk space, MaxCompressionRatio.
WORK = tempname();
mkdir(WORK);
cleanup = onCleanup(@() rmdir(WORK, 's'));
%=============================================================================
% A hand-made zip with one stored entry extracts normally.
good = fullfile(WORK, 'good.zip');
writeBytes(good, craftZip({'a.txt'}, [0], 5));
out = fullfile(WORK, 'good');
names = unzip(good, out);
asserts.isequal(numel(names), 1);
asserts.isequal(fileread(fullfile(out, 'a.txt')), 'hello');
%=============================================================================
% Two entries sharing the same bytes (non-recursive zip bomb) are refused.
overlap = fullfile(WORK, 'overlap.zip');
writeBytes(overlap, craftZip({'a.txt', 'b.txt'}, [0, 0], 5));
out = fullfile(WORK, 'overlap');
checkId(@() unzip(overlap, out), 'Nelson:io:zip:overlappingEntries');
asserts.isfalse(isfolder(out));
checkId(@() nelson.io.ZipArchive(overlap), 'Nelson:io:zip:overlappingEntries');
%=============================================================================
% A declared size larger than the free disk space is refused before writing.
huge = fullfile(WORK, 'huge.zip');
writeBytes(huge, craftZip({'a.txt'}, [0], 2^60));
out = fullfile(WORK, 'huge');
checkId(@() unzip(huge, out), 'Nelson:file_archiver:notEnoughSpace');
asserts.isfalse(isfolder(out));
%=============================================================================
% MaxExtractedSize counts every file of the archive.
data = fullfile(WORK, 'data');
mkdir(data);
zeroFile = fullfile(data, 'zeros.bin');
writeBytes(zeroFile, zeros(1, 20e6, 'uint8'));
writeBytes(fullfile(data, 'small.txt'), uint8('small'));
zipfile = fullfile(WORK, 'zeros.zip');
zip(zipfile, {'zeros.bin', 'small.txt'}, data);
out = fullfile(WORK, 'limited');
SIZE_ID = 'Nelson:file_archiver:extractedSizeLimit';
checkId(@() unzip(zipfile, out, 'MaxExtractedSize', 1e6), SIZE_ID);
asserts.isfalse(isfolder(out));
names = unzip(zipfile, out, 'MaxExtractedSize', 20e6 + 5);
asserts.isequal(numel(names), 2);
%=============================================================================
% MaxCompressionRatio: 20 MB of zeros deflate about 1000 times.
out = fullfile(WORK, 'ratio');
RATIO_ID = 'Nelson:file_archiver:compressionRatioLimit';
checkId(@() unzip(zipfile, out, 'MaxCompressionRatio', 100), RATIO_ID);
asserts.isfalse(isfolder(out));
names = unzip(zipfile, out, 'MaxCompressionRatio', 5000);
asserts.isequal(numel(names), 2);
% Small entries are never refused for their ratio.
names = unzip(good, fullfile(WORK, 'smallratio'), 'MaxCompressionRatio', 1);
asserts.isequal(numel(names), 1);
%=============================================================================
% tar and tar.gz: the sizes in the headers are checked before writing.
targz = fullfile(WORK, 'zeros.tar.gz');
tar(targz, {'zeros.bin', 'small.txt'}, data);
out = fullfile(WORK, 'untar_ratio');
checkId(@() untar(targz, out, 'MaxCompressionRatio', 100), RATIO_ID);
asserts.isfalse(isfolder(out));
out = fullfile(WORK, 'untar_size');
checkId(@() untar(targz, out, 'MaxExtractedSize', 1000), 'Nelson:file_archiver:extractedSizeLimit');
asserts.isfalse(isfolder(out));
names = untar(targz, out, 'MaxExtractedSize', Inf, 'MaxCompressionRatio', Inf);
asserts.isequal(numel(names), 2);
t = nelson.io.TarArchive(targz);
out = fullfile(WORK, 'tar_extract');
checkId(@() t.extract(out, 'MaxExtractedSize', 10), 'Nelson:file_archiver:extractedSizeLimit');
names = t.extract(out, 'MaxCompressionRatio', 5000);
asserts.isequal(numel(names), 2);
t.close();
%=============================================================================
% gzip and Zstandard streams are counted while decompressing.
packed = nelson.io.gzip.compress(zeros(1, 20e6, 'uint8'));
checkId(@() nelson.io.gzip.decompress(packed, 'MaxCompressionRatio', 100), RATIO_ID);
asserts.isequal(numel(nelson.io.gzip.decompress(packed, 'MaxCompressionRatio', 5000)), 20e6);
gzfile = fullfile(WORK, 'zeros.bin.gz');
writeBytes(gzfile, packed);
target = fullfile(WORK, 'gunzipped.bin');
checkId(@() nelson.io.gzip.decompressfile(gzfile, target, 'MaxCompressionRatio', 100), RATIO_ID);
asserts.isfalse(isfile(target));
out = fullfile(WORK, 'gunzip_out');
checkId(@() gunzip(gzfile, out, 'MaxCompressionRatio', 100), RATIO_ID);
checkId(@() gunzip(gzfile, out, 'MaxExtractedSize', 1000), 'Nelson:io:gzip:tooLarge');
names = gunzip(gzfile, out, 'MaxCompressionRatio', 5000);
info = dir(names{1});
asserts.isequal(info.bytes, 20e6);
if nelson.io.zstd.isavailable()
  packed = nelson.io.zstd.compress(zeros(1, 20e6, 'uint8'));
  checkId(@() nelson.io.zstd.decompress(packed, 'MaxCompressionRatio', 100), RATIO_ID);
  checkId(@() nelson.io.zstd.decompresstext(packed, 'MaxCompressionRatio', 100), RATIO_ID);
end
%=============================================================================
% Option values.
checkId(@() unzip(zipfile, out, 'MaxCompressionRatio', 0), ...
  'Nelson:file_archiver:invalidMaxCompressionRatio');
checkId(@() unzip(zipfile, out, 'MaxExtractedSize', -1), 'Nelson:file_archiver:invalidMaxExtractedSize');
checkId(@() unzip(zipfile, out, 'MaxSize', 10), 'Nelson:io:invalidOption');
%=============================================================================
function checkId(f, id)
  % f must fail with identifier id.
  raised = '';
  try
    f();
  catch e
    raised = e.identifier;
  end
  asserts.isequal(raised, id);
end
%=============================================================================
function writeBytes(filename, bytes)
  fid = fopen(filename, 'wb');
  fwrite(fid, bytes, 'uint8');
  fclose(fid);
end
%=============================================================================
function bytes = craftZip(names, offsets, declaredSize)
  % One stored local entry 'a.txt' = 'hello' at offset 0, then central entries naming
  % it at the given offsets with declaredSize as uncompressed size (zip64 beyond 4 GiB).
  content = uint8('hello');
  crc = le32(hex2dec('3610A686'));
  local = [uint8('PK'), 3, 4, le16(20), le16(0), le16(0), le16(0), le16(33), crc, ...
    le32(5), le32(5), le16(5), le16(0), uint8('a.txt'), content];
  central = uint8([]);
  for k = 1:numel(names)
    sizeField = le32(declaredSize);
    extra = uint8([]);
    if declaredSize > 4294967294
      sizeField = le32(4294967295);
      extra = [le16(1), le16(8), typecast(uint64(declaredSize), 'uint8')];
    end
    central = [central, uint8('PK'), 1, 2, le16(20), le16(45), le16(0), le16(0), le16(0), ...
      le16(33), crc, le32(5), sizeField, le16(numel(names{k})), le16(numel(extra)), le16(0), ...
      le16(0), le16(0), le32(0), le32(offsets(k)), uint8(names{k}), extra];
  end
  eocd = [uint8('PK'), 5, 6, le16(0), le16(0), le16(numel(names)), le16(numel(names)), ...
    le32(numel(central)), le32(numel(local)), le16(0)];
  bytes = [local, central, eocd];
end
%=============================================================================
function b = le16(x)
  b = typecast(uint16(x), 'uint8');
end
%=============================================================================
function b = le32(x)
  b = typecast(uint32(x), 'uint8');
end
%=============================================================================

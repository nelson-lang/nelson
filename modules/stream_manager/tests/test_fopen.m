%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('fopen'), 4);
asserts.isequal(nargout('fopen'), 4);
%=============================================================================
d = tempname();
mkdir(d);
c = onCleanup(@() rmdir(d, 's')); %#ok<NASGU>
test_fopen_file = [d, '/test_fopen.txt'];
%=============================================================================
ID = fopen(test_fopen_file);
asserts.isequal(ID, -1);
%=============================================================================
[ID, MSG] = fopen(test_fopen_file);
asserts.isequal(ID, -1);
asserts.isequal(MSG, _('Cannot open file.'));
%=============================================================================
ID = fopen(test_fopen_file, 'w');
asserts.isequal(ID, 3);
%=============================================================================
filename = fopen(ID);
if ismac()
  asserts.istrue(endsWith(filename, '/test_fopen.txt'));
else
  asserts.isequal(filename, test_fopen_file);
end
%=============================================================================
[filename, mode] = fopen(ID);
if ismac()
  asserts.istrue(endsWith(filename, '/test_fopen.txt'));
else
  asserts.isequal(filename, test_fopen_file);
end
asserts.isequal(mode, 'wb');
%=============================================================================
[filename, mode, machine] = fopen(ID);
if ismac()
  asserts.istrue(endsWith(filename, '/test_fopen.txt'));
else
  asserts.isequal(filename, test_fopen_file);
end
asserts.isequal(mode, 'wb');
asserts.istrue(strcmp(machine, 'ieee-le') || strcmp(machine, 'ieee-be'));
%=============================================================================
[filename, mode, machine, encoding] = fopen(ID);
if ismac()
  asserts.istrue(endsWith(filename, '/test_fopen.txt'));
else
  asserts.isequal(filename, test_fopen_file);
end
asserts.isequal(mode, 'wb');
asserts.istrue(strcmp(machine, 'ieee-le') || strcmp(machine, 'ieee-be'));
asserts.isequal(encoding, 'UTF-8');
%=============================================================================
fclose(ID);
%=============================================================================
IDS = fopen('all');
asserts.isequal(IDS, [0 1 2]);
%=============================================================================
asserts.checkerror('fopen()', _('Not enough input arguments.'))
%=============================================================================
rootParent = fullfile(nelsonroot(), 'temp');
rootTemp = fullfile(rootParent, 'compatibility_stream_behaviors');
if ~isdir(rootParent)
  mkdir(nelsonroot(), 'temp');
end
if isdir(rootTemp)
  rmdir(rootTemp, 's');
end
mkdir(rootParent, 'compatibility_stream_behaviors');
textFile = fullfile(rootTemp, 'text.txt');
errorFile = fullfile(rootTemp, 'error.txt');
fid = fopen(textFile, 'wt', 'n', 'UTF-8');
asserts.istrue(fid > 2);
fprintf(fid, '%s\n%d %.1f\n', 'alpha', 42, 3.5);
asserts.isequal(fclose(fid), 0);
%=============================================================================
fid = fopen(textFile, 'rt', 'n', 'UTF-8');
asserts.isequal(ftell(fid), 0);
firstLine = fgetl(fid);
[values, valueCount] = fscanf(fid, '%d %f');
asserts.isequal(firstLine, 'alpha');
asserts.isequal(values, [42; 3.5]);
asserts.isequal(valueCount, 2);
asserts.isequal(feof(fid), 1);
[message, code] = ferror(fid);
asserts.isequal(message, '');
asserts.isequal(code, 0);
frewind(fid);
asserts.isequal(ftell(fid), 0);
asserts.isequal(fseek(fid, 0, 'eof'), 0);
asserts.isequal(ftell(fid), fsize(fid));
asserts.isequal(fclose(fid), 0);
fid = fopen(textFile, 'rt', 'n', 'UTF-8');
asserts.isequal(fgetl(fid), 'alpha');
asserts.isequal(fgetl(fid), '42 3.5');
asserts.isequal(fgetl(fid), -1);
asserts.isequal(fclose(fid), 0);
%=============================================================================
fid = fopen(textFile, 'at', 'n', 'UTF-8');
asserts.istrue(fid > 2);
asserts.istrue(fprintf(fid, '%s', 'omega') > 0);
asserts.isequal(fclose(fid), 0);
contents = fileread(textFile);
asserts.istrue(endsWith(contents, 'omega'));
%=============================================================================
fid = fopen(errorFile, 'wt', 'n', 'UTF-8');
asserts.istrue(fid > 2);
asserts.isequal(fgets(fid), -1);
[message, code] = ferror(fid);
asserts.isequal(code, -1);
asserts.istrue(ischar(message));
[message, code] = ferror(fid, 'clear');
asserts.isequal(code, -1);
[message, code] = ferror(fid);
asserts.isequal(message, '');
asserts.isequal(code, 0);
asserts.isequal(fclose(fid), 0);
%=============================================================================
fid = fopen(textFile, 'rt', 'n', 'UTF-8');
asserts.istrue(fid > 2);
asserts.isequal(fgets(fid, 5), 'alpha');
asserts.isequal(fclose(fid), 0);
asserts.isequal(SEEK_SET(), -1);
asserts.isequal(SEEK_CUR(), 0);
asserts.isequal(SEEK_END(), 1);
asserts.isequal(stdin(), 0);
asserts.isequal(stdout(), 1);
asserts.isequal(stderr(), 2);
asserts.isequal(sscanf('1 2 3', '%d'), [1; 2; 3]);
[scanValues, scanCount, scanMessage, scanNextIndex] = sscanf('3.14159 are digits', '%f');
asserts.isapprox(scanValues, 3.14159, 1e-5);
asserts.isequal(scanCount, 1);
asserts.isequal(scanMessage, _('Matching failure in format.'));
asserts.isequal(scanNextIndex, 9);
asserts.isequal(sprintf('%02d', 3), '03');
cprintfText = evalc('cprintfCount = cprintf(''red'', ''value %d'', 7);');
asserts.isequal({cprintfText, cprintfCount}, {'value 7', 7});
%=============================================================================
binaryFile = fullfile(rootTemp, 'values.bin');
fid = fopen(binaryFile, 'wb');
byteValues = uint8([0 127 255]);
asserts.isequal(fwrite(fid, byteValues, 'uint8'), 3);
asserts.isequal(fclose(fid), 0);
fid = fopen(binaryFile, 'rb');
[bytes, readCount] = fread(fid, 'uint8');
asserts.isequal(readCount, 3);
asserts.isequal(bytes, uint8([0; 127; 255]));
asserts.isequal(fseek(fid, 1, 'bof'), 0);
asserts.isequal(fread(fid, 1, 'uint8'), uint8(127));
asserts.isequal(fclose(fid), 0);
%=============================================================================
dataFile = fullfile(rootTemp, 'data.mat');
numericValues = [NaN Inf; 3 4];
emptyValues = zeros(0, 2);
textValue = 'saved';
save(dataFile, 'numericValues', 'emptyValues', 'textValue');
loaded = load(dataFile);
asserts.istrue(isequaln(loaded.numericValues, numericValues));
asserts.isequal(loaded.emptyValues, emptyValues);
asserts.isequal(loaded.textValue, textValue);
%=============================================================================
diaryFile = fullfile(rootTemp, 'diary.txt');
diary('off');
diary('set', 'DiaryFile', diaryFile);
asserts.isequal(diary('get', 'DiaryFile'), diaryFile);
diary('on');
disp('compatibility diary line');
diary('off');
asserts.isequal(diary('get', 'Diary'), 'off');
if isfile(diaryFile)
  diaryText = fileread(diaryFile);
  asserts.istrue(contains(diaryText, 'compatibility diary line'));
end
%=============================================================================
asserts.checkerror('fopen()', _('Not enough input arguments.'));
asserts.checkerror('fread()', _('Not enough input arguments.'));
asserts.checkerror('fwrite()', _('Not enough input arguments.'));
asserts.checkerror('fseek()', _('Not enough input arguments.'));
%=============================================================================
rmdir(rootTemp, 's');
%=============================================================================

%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
ncc('--help');
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
archives = {};
for variant = 1:2
  root = fullfile(directory, num2str(variant));
  mkdir(root);
  mkdir(fullfile(root, 'nested'));
  mkdir(fullfile(root, 'nested', 'empty'));
  names = {'z.txt', 'nested/b.txt', 'a.txt', 'nested/a.txt'};
  if variant == 2
    names = names(end:-1:1);
    pause(2.1);
  end
  for k = 1:numel(names)
    filewrite(fullfile(root, names{k}), ['content:', names{k}]);
  end
  archives{variant} = fullfile(directory, [num2str(variant), '.zip']);
  __nelsonc_archive__('pack', archives{variant}, {'nested', 'z.txt', 'a.txt'}, [root, '/']);
end
asserts.isequal(sha256(archives{1}, '-file'), sha256(archives{2}, '-file'));
fid = fopen(archives{1}, 'rb');
bytes = fread(fid, Inf, '*uint8');
fclose(fid);
central = find(bytes(1:end - 3) == 80 & bytes(2:end - 2) == 75 ...
  & bytes(3:end - 1) == 1 & bytes(4:end) == 2);
asserts.isequal(numel(central), 5);
storedNames = {};
for offset = central'
  % DOS time/date: midnight on 1980-01-01, with no local time-zone field.
  asserts.isequal(bytes(offset + (12:15)), uint8([0; 0; 33; 0]));
  length = double(bytes(offset + 28)) + 256 * double(bytes(offset + 29));
  storedNames{end + 1} = char(bytes(offset + (46:45 + length)))';
end
asserts.isequal(storedNames, {'a.txt', 'nested/a.txt', 'nested/b.txt', 'nested/empty/', 'z.txt'});
original = sha256(archives{1}, '-file');
assert_rejected(@() __nelsonc_archive__('pack', archives{1}, {'a.txt'}, [root, '/']));
asserts.isequal(sha256(archives{1}, '-file'), original);
assert_rejected(@() __nelsonc_archive__('pack', fullfile(directory, 'bad.zip'), {'missing.txt'}, [root, '/']));
asserts.isfalse(isfile(fullfile(directory, 'bad.zip')));
ordinary = fullfile(directory, 'ordinary.zip');
zip(ordinary, {'a.txt'}, [root, '/']);
fid = fopen(ordinary, 'rb');
bytes = fread(fid, Inf, '*uint8');
fclose(fid);
asserts.isfalse(isequal(bytes(11:14), uint8([0; 0; 33; 0])));
clear cleanup;

function assert_rejected(callback)
  rejected = false;
  try
    callback();
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
end

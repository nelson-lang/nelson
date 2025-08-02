%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
dest_path = [tempdir(), 'test_audiowrite'];
if isdir(dest_path)
  rmdir(dest_path, 's');
end
%=============================================================================
wav_file = [modulepath('audio', 'tests'), '/DynoA3.wav'];
[Y_REF, FS_REF] = audioread(wav_file);
mkdir(dest_path);
dest_file = [dest_path, '/'];
dest_file = [dest_path, '/test_audiowrite_1', '.wav'];
audiowrite(dest_file, Y_REF, FS_REF)
[y, fs] = audioread(dest_file);
assert_isapprox(y, Y_REF, 1e-4);
%=============================================================================
formats = audiosupportedformats();
unsupported_extensions = {'.htk', '.iff', '.sds', '.wve', '.xi', '.raw', '.sd2', '.m1a', '.oga'};
for f = formats(:)'
  if ~any(strcmp(f.Extension, unsupported_extensions))
    dest_file = [dest_path, '/test_audiowrite_1', f.Extension];
    audiowrite(dest_file, Y_REF, FS_REF)
    assert_istrue(isfile(dest_file));
    [y, fs] = audioread(dest_file);
    assert_isequal(size(y), size(Y_REF));
    assert_isequal(fs, FS_REF);
    assert_isapprox(y, Y_REF, 1e-4);
  end
end
%=============================================================================
rmdir(dest_path, 's');

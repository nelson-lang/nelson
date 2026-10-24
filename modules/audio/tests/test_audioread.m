%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(iswasm(), ...
  'WebAssembly audio backend validates browser-supported file decoding in portable audio tests; this desktop fixture set includes formats not enabled there.');
%=============================================================================
wav_file = [modulepath('audio', 'tests'), '/6_Channel_ID.wav'];
[y, fs] = audioread(wav_file);
asserts.isequal(size(y), [257411 6]);
asserts.isequal(class(y), 'double');
asserts.isequal(fs, 44100);
%=============================================================================
[y, fs] = audioread(wav_file, 'native');
asserts.isequal(size(y), [257411 6]);
asserts.isequal(class(y), 'int16');
asserts.isequal(fs, 44100);
%=============================================================================
wav_file = [modulepath('audio', 'tests'), '/DynoA3.wav'];
[y, fs] = audioread(wav_file);
asserts.isequal(size(y), [3113 2]);
asserts.isequal(class(y), 'double');
asserts.isequal(fs, 22001);
%=============================================================================
[y, fs] = audioread(wav_file, 'native');
asserts.isequal(size(y), [3113 2]);
asserts.isequal(class(y), 'int16');
asserts.isequal(fs, 22001);
%=============================================================================
wav_file = [modulepath('audio', 'tests'), '/MacBoing.wav'];
[y, fs] = audioread(wav_file);
asserts.isequal(size(y), [7673 1]);
asserts.isequal(class(y), 'double');
asserts.isequal(fs, 11025);
%=============================================================================
[y, fs] = audioread(wav_file, 'native');
asserts.isequal(size(y), [7673 1]);
asserts.isequal(class(y), 'uint8');
asserts.isequal(fs, 11025);
%=============================================================================
flac_file = [modulepath('audio', 'tests'), '/mana.flac'];
[y, fs] = audioread(flac_file);
asserts.isequal(size(y), [23501 1]);
asserts.isequal(class(y), 'double');
asserts.isequal(fs, 44100);
%=============================================================================
[y, fs] = audioread(flac_file, 'native');
asserts.isequal(size(y), [23501 1]);
asserts.isequal(class(y), 'int16');
asserts.isequal(fs, 44100);
%=============================================================================
wav_file = [modulepath('audio', 'tests'), '/DynoA3.wav'];
[y, fs] = audioread(wav_file, [1 10]);
y_ref = [0.0034 0.0034;
  0.0026 0.0026;
  0.0016 0.0016;
  0.0016 0.0016;
  0.0016 0.0016;
  0.0016 0.0016;
  0.0016 0.0016;
  0.0016 0.0016;
  0.0016 0.0016;
  0.0016 0.0016];
asserts.isapprox(y * 1000, y_ref * 1000, 1e-1);
%=============================================================================
ogg_file = [modulepath('audio', 'tests'), '/flac_char.ogg'];
asserts.checkerror('[y, fs] = audioread(ogg_file);', sprintf(_('Wrong value for #%d argument.'), 1));
%=============================================================================

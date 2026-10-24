%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
skip_testsuite(iswasm(), ...
  'WebAssembly audio backend validates browser-supported file metadata in portable audio tests; this desktop fixture set includes formats not enabled there.');
%=============================================================================
wav_file = [modulepath('audio', 'tests'), '/6_Channel_ID.wav'];
info = audioinfo(wav_file);
asserts.isequal(info.CompressionMethod, 'WAVX')
asserts.isequal(info.NumChannels, [6.000000])
asserts.isequal(info.SampleRate, [44100.000000])
asserts.isequal(info.TotalSamples, [257411.000000])
asserts.isapprox(info.Duration, [5.836984], 1e-5)
asserts.isequal(info.Title, [])
asserts.isequal(info.Comment, [])
asserts.isequal(info.Artist, [])
asserts.isequal(info.BitsPerSample, 16)
%=============================================================================
flac_file = [modulepath('audio', 'tests'), '/handel.flac'];
info = audioinfo(flac_file);
asserts.isequal(info.CompressionMethod, 'FLAC');
asserts.isequal(info.NumChannels, [1.000000]);
asserts.isequal(info.SampleRate, [8192.000000]);
asserts.isequal(info.TotalSamples, [73113.000000]);
asserts.isapprox(info.Duration, [8.924927], 1e-5);
isvalid = (isempty(info.Title) || strcmp(info.Title, 'handel (title)'));
if ~isvalid
  asserts.isequal(info.Title, 'handel (title)');
  asserts.isequal(info.Comment, 'some comments');
end
asserts.isequal(info.Artist, []);
asserts.isequal(info.BitsPerSample, [16.000000]);
%=============================================================================
ogg_file = [modulepath('audio', 'tests'), '/flac_char.ogg'];
asserts.checkerror('info = audioinfo(ogg_file);', sprintf(_('Wrong value for #%d argument.'), 1));
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
%=============================================================================
wav_file = [modulepath('audio', 'tests'), '/6_Channel_ID.wav'];
info = audioinfo(wav_file);
assert_isequal(info.CompressionMethod, 'WAVX')
assert_isequal(info.NumChannels, [6.000000])
assert_isequal(info.SampleRate, [44100.000000])
assert_isequal(info.TotalSamples, [257411.000000])
assert_isapprox(info.Duration, [5.836984], 1e-5)
assert_isequal(info.Title, [])
assert_isequal(info.Comment, [])
assert_isequal(info.Artist, [])
assert_isequal(info.BitsPerSample, 16)
%=============================================================================
flac_file = [modulepath('audio', 'tests'), '/handel.flac'];
info = audioinfo(flac_file);
assert_isequal(info.CompressionMethod, 'FLAC');
assert_isequal(info.NumChannels, [1.000000]);
assert_isequal(info.SampleRate, [8192.000000]);
assert_isequal(info.TotalSamples, [73113.000000]);
assert_isapprox(info.Duration, [8.924927], 1e-5);
isvalid = (isempty(info.Title) || strcmp(info.Title, 'handel (title)'));
if ~isvalid
  assert_isequal(info.Title, 'handel (title)');
  assert_isequal(info.Comment, 'some comments');
end
assert_isequal(info.Artist, []);
assert_isequal(info.BitsPerSample, [16.000000]);
%=============================================================================
ogg_file = [modulepath('audio', 'tests'), '/flac_char.ogg'];
assert_checkerror('info = audioinfo(ogg_file);', _('Wrong value for #1 argument.'));
%=============================================================================


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
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
asserts.isequal(nargin('audiometadata'), -1);
asserts.isequal(nargout('audiometadata'), 1);
%=============================================================================
wav_file = [modulepath('audio'), '/tests/6_Channel_ID.wav'];
try
  info = audiometadata(wav_file);
catch ex
  if strcmp(ex.message, _('Taglib not available.'))
    return
  else
    throw(ex);
  end
end
asserts.isequal(size(info), [1 1]);
asserts.istrue(isempty(fieldnames(info)));
asserts.istrue(iscell(fieldnames(info)));
%=============================================================================
flac_file = [modulepath('audio'), '/tests/handel.flac'];
info = audiometadata(flac_file);
asserts.isequal(info.ALBUM, 'album name')
asserts.isequal(info.DESCRIPTION, 'some comments')
asserts.isequal(info.GENRE, 'music')
asserts.isequal(info.TITLE, 'handel (title)')
asserts.isequal(info.TRACKNUMBER, '3')
%=============================================================================
wav_file = [modulepath('audio'), '/tests/DynoA3.wav'];
info = audiometadata(wav_file);
asserts.isequal(info.ALBUM, 'album test');
asserts.isequal(info.ALBUMARTIST, 'artist 3 3');
asserts.isequal(info.ARTIST, 'artist test');
asserts.isequal(info.COMMENT, 'test comments');
asserts.isequal(info.COMPOSER, 'none');
asserts.isequal(info.DATE, '1900');
asserts.isequal(info.DISCNUMBER, '44');
asserts.isequal(info.GENRE, 'Acoustic');
asserts.isequal(info.TITLE, 'title test');
asserts.isequal(info.TRACKNUMBER, '42');
%=============================================================================
ogg_file = [modulepath('audio'), '/examples/drums.ogg'];
info = audiometadata(ogg_file);
asserts.isequal(info.ENCODER, 'Lavc58.9.100 libvorbis');
asserts.isequal(info.LANGUAGE, 'fra');
asserts.isequal(info.LANGUAGE_2_LETTER, '-FR');
asserts.isequal(info.ORGANIZATION, 'Parlophone');
asserts.isequal(info.RELEASE_TYPE, 'Normal release');
asserts.isequal(info.RIP_DATE, '2014-10-08');
asserts.isequal(info.RIPPING_TOOL, 'EAC');
asserts.isequal(info.SOURCE, 'CD (LP)');
asserts.isequal(info.TITLE, 'drums');
%=============================================================================

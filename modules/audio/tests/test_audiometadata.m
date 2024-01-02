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
assert_isequal(nargin('audiometadata'), -1);
assert_isequal(nargout('audiometadata'), 1);
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
assert_isequal(size(info), [1 1]);
assert_istrue(isempty(fieldnames(info)));
assert_istrue(iscell(fieldnames(info)));
%=============================================================================
flac_file = [modulepath('audio'), '/tests/handel.flac'];
info = audiometadata(flac_file);
assert_isequal(info.ALBUM, 'album name')
assert_isequal(info.DESCRIPTION, 'some comments')
assert_isequal(info.GENRE, 'music')
assert_isequal(info.TITLE, 'handel (title)')
assert_isequal(info.TRACKNUMBER, '3')
%=============================================================================
wav_file = [modulepath('audio'), '/tests/DynoA3.wav'];
info = audiometadata(wav_file);
assert_isequal(info.ALBUM, 'album test');
assert_isequal(info.ALBUMARTIST, 'artist 3 3');
assert_isequal(info.ARTIST, 'artist test');
assert_isequal(info.COMMENT, 'test comments');
assert_isequal(info.COMPOSER, 'none');
assert_isequal(info.DATE, '1900');
assert_isequal(info.DISCNUMBER, '44');
assert_isequal(info.GENRE, 'Acoustic');
assert_isequal(info.TITLE, 'title test');
assert_isequal(info.TRACKNUMBER, '42');
%=============================================================================
ogg_file = [modulepath('audio'), '/examples/drums.ogg'];
info = audiometadata(ogg_file);
assert_isequal(info.ENCODER, 'Lavc58.9.100 libvorbis');
assert_isequal(info.LANGUAGE, 'fra');
assert_isequal(info.LANGUAGE_2_LETTER, '-FR');
assert_isequal(info.ORGANIZATION, 'Parlophone');
assert_isequal(info.RELEASE_TYPE, 'Normal release');
assert_isequal(info.RIP_DATE, '2014-10-08');
assert_isequal(info.RIPPING_TOOL, 'EAC');
assert_isequal(info.SOURCE, 'CD (LP)');
assert_isequal(info.TITLE, 'drums');
%=============================================================================

%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function webAudioPlay(y, fs)
  action = struct();
  action.type = 'audio-buffer-begin';
  action.id = 0;
  action.sampleRate = double(fs);
  action.channels = size(y, 2);
  action.frames = size(y, 1);
  fprintf('%s%s%s\n', '__NELSON_UI_ACTION_BEGIN__', jsonencode(action), '__NELSON_UI_ACTION_END__');

  chunkFrames = 8192;
  for offset = 0:chunkFrames:(action.frames - 1)
    count = min(chunkFrames, action.frames - offset);
    chunk = struct();
    chunk.type = 'audio-buffer-chunk';
    chunk.id = action.id;
    chunk.offset = offset;
    chunk.frames = count;
    samples = y((offset + 1):(offset + count), :);
    chunk.samples = samples(:).';
    fprintf('%s%s%s\n', '__NELSON_UI_ACTION_BEGIN__', jsonencode(chunk), ...
      '__NELSON_UI_ACTION_END__');
  end

  playAction = struct();
  playAction.type = 'audio-buffer-play';
  playAction.id = action.id;
  fprintf('%s%s%s\n', '__NELSON_UI_ACTION_BEGIN__', jsonencode(playAction), ...
    '__NELSON_UI_ACTION_END__');
end
%=============================================================================

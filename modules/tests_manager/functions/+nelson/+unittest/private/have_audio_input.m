%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function r = have_audio_input()
  persistent audio_input_required %#ok<NLS0005>
  if isempty(audio_input_required)
    audio_input_required = false;
    if ismodule('audio')
      if strcmp(getenv('AUDIODEV'), 'null') == true
        audio_input_required = false;
      else
        devices = audiodevinfo('default');
        audio_input_required = ~isempty(devices.input);
      end
    end
  end
  r = audio_input_required;
end

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function r = have_audio_output()
  persistent audio_output_required %#ok<NLS0005>
  if isempty(audio_output_required)
    audio_output_required = false;
    if ismodule('audio')
      if strcmp(getenv('AUDIODEV'), 'null') == true
        audio_output_required = false;
      else
        devices = audiodevinfo('default');
        audio_output_required = ~isempty(devices.output);
      end
    end
  end
  r = audio_output_required;
end

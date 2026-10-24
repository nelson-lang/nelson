%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = should_disable_audio(test_case)
  % The audio module (its file-I/O builtins audioread/audiowrite/audioinfo, and
  % the device functions) is disabled with --no-audio in every non-audio test
  % child. A test that needs the module must declare it: AUDIO INPUT/OUTPUT
  % REQUIRED (device needed) or AUDIO REQUIRED (builtins only, no device).
  audio_required = test_case.options.audio_input_required ...
    || test_case.options.audio_output_required ...
    || test_case.options.audio_required;
  tf = ~audio_required && ~endsWith(test_case.classname, '.audio');
end
%=============================================================================

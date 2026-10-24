%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--AUDIO OUTPUT REQUIRED-->
% <--AUDIO INPUT REQUIRED-->
%=============================================================================
r = audiodevinfo();
asserts.isequal(class(r), 'struct');
f = fieldnames(r);
asserts.isequal(f, {'input'; 'output'});
%=============================================================================
if (isempty(r.input))
  return
end
if (isempty(r.output))
  return
end
%=============================================================================
info = audiodevinfo();
REF_fieldnames = {'Name';
  'DriverVersion';
  'MaxChannels';
  'DefaultSampleRate';
  'DefaultLowLatency';
  'DefaultHighLatency';
  'ID'};
for in_audio = info.input'
  asserts.isequal(fieldnames(in_audio), REF_fieldnames);
  asserts.istrue(isnumeric(in_audio.ID) && isscalar(in_audio.ID));
  asserts.istrue(ischar(in_audio.Name));
end
for out_audio = info.output'
  asserts.isequal(fieldnames(out_audio), REF_fieldnames);
  asserts.istrue(isnumeric(out_audio.ID) && isscalar(out_audio.ID));
  asserts.istrue(ischar(out_audio.Name));
end
%=============================================================================
if ismac()
  return
end
%=============================================================================
OUTPUT_DEVICE = 0;
R1 = audiodevinfo(OUTPUT_DEVICE, 44100, 16, 2);
asserts.istrue(R1 ~= -1);
R2 = audiodevinfo(OUTPUT_DEVICE, R1, 44100, 16, 2);
asserts.istrue(R2);
%=============================================================================
INPUT_DEVICE = 1;
R3 = audiodevinfo(INPUT_DEVICE, 44100, 16, 2);
asserts.istrue(R3 ~= -1);
R4 = audiodevinfo(INPUT_DEVICE, R3, 44100, 16, 2);
asserts.istrue(R4);
%=============================================================================
if audiodevinfo(OUTPUT_DEVICE) > 0
  support = audiodevinfo(OUTPUT_DEVICE, audiodevinfo(OUTPUT_DEVICE), 44100, 16, 2);
  asserts.istrue(support);
end
%=============================================================================
if audiodevinfo(OUTPUT_DEVICE) > 0
  support = audiodevinfo(OUTPUT_DEVICE, audiodevinfo(OUTPUT_DEVICE), 961000, 16, 2);
  asserts.isfalse(support);
end
%=============================================================================

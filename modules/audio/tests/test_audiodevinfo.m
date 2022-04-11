%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--AUDIO OUTPUT REQUIRED-->
% <--AUDIO INPUT REQUIRED-->
%=============================================================================
OUTPUT_DEVICE = 0;
INPUT_DEVICE = 1;
%=============================================================================
r = audiodevinfo();
assert_isequal(class(r), 'struct');
f = fieldnames(r);
assert_isequal(f, {'input'; 'output'});
%=============================================================================
info = audiodevinfo();
for k = [1:audiodevinfo(OUTPUT_DEVICE)]
  assert_isequal(audiodevinfo(OUTPUT_DEVICE, info.output(k).ID), info.output(k).Name);
end
for k = [1:audiodevinfo(INPUT_DEVICE)]
  assert_isequal(audiodevinfo(INPUT_DEVICE, info.input(k).ID), info.input(k).Name);
end
%=============================================================================
info = audiodevinfo();
for k = [1:audiodevinfo(OUTPUT_DEVICE)]
  assert_isequal(audiodevinfo(OUTPUT_DEVICE,info.output(k).Name), info.output(k).ID)
end
for k = [1:audiodevinfo(INPUT_DEVICE)]
  assert_isequal(audiodevinfo(INPUT_DEVICE,info.input(k).Name), info.input(k).ID)
end
%=============================================================================
info = audiodevinfo();
for k = [1:audiodevinfo(OUTPUT_DEVICE)]
  assert_isequal(audiodevinfo(OUTPUT_DEVICE, info.output(k).ID, 'DriverVersion'), info.output(k).DriverVersion)
end
for k = [1:audiodevinfo(INPUT_DEVICE)]
  assert_isequal(audiodevinfo(INPUT_DEVICE, info.input(k).ID, 'DriverVersion'), info.input(k).DriverVersion)
end
%=============================================================================
R1 = audiodevinfo(OUTPUT_DEVICE, 44100, 16, 2);
assert_istrue(R1 ~= -1);
R2 = audiodevinfo(OUTPUT_DEVICE, R1, 44100, 16, 2);
assert_istrue(R2);
%=============================================================================
R3 = audiodevinfo(INPUT_DEVICE, 44100, 16, 2);
assert_istrue(R3 ~= -1);
R4 = audiodevinfo(INPUT_DEVICE, R3, 44100, 16, 2);
assert_istrue(R4);
%=============================================================================
if audiodevinfo(OUTPUT_DEVICE) > 0
  support = audiodevinfo(OUTPUT_DEVICE,audiodevinfo(OUTPUT_DEVICE),44100,16,2);
  assert_istrue(support);
end
%=============================================================================
if audiodevinfo(OUTPUT_DEVICE) > 0
  support = audiodevinfo(OUTPUT_DEVICE,audiodevinfo(OUTPUT_DEVICE),961000,16,2);
  assert_isfalse(support);
end
%=============================================================================

%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~iswasm()
  skip_testsuite('Portable Web Audio backend requires a WebAssembly build.');
end
info = audiodevinfo();
%=============================================================================
asserts.isequal(fieldnames(info), {'input'; 'output'});
asserts.istrue(isempty(info.input));
asserts.isequal(info.output.Name, 'Web Audio');
asserts.isequal(info.output.DriverVersion, 'Browser Web Audio');
asserts.isequal(info.output.MaxChannels, 2);
asserts.isequal(info.output.DefaultSampleRate, 44100);
asserts.isequal(info.output.ID, 0);
%=============================================================================
OUTPUT_DEVICE = 0;
INPUT_DEVICE = 1;
asserts.isequal(audiodevinfo(OUTPUT_DEVICE), 1);
asserts.isequal(audiodevinfo(INPUT_DEVICE), 0);
asserts.isequal(audiodevinfo(OUTPUT_DEVICE, 0), 'Web Audio');
asserts.isequal(audiodevinfo(OUTPUT_DEVICE, 'Web Audio'), 0);
asserts.isequal(audiodevinfo(OUTPUT_DEVICE, 0, 'DriverVersion'), 'Browser Web Audio');
asserts.isequal(audiodevinfo(OUTPUT_DEVICE, 44100, 16, 2), 0);
asserts.istrue(audiodevinfo(OUTPUT_DEVICE, 0, 44100, 16, 2));
asserts.isfalse(audiodevinfo(OUTPUT_DEVICE, 0, 44100, 16, 3));
asserts.isequal(audiodevinfo(INPUT_DEVICE, 44100, 16, 1), -1);
%=============================================================================

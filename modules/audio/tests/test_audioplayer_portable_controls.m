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
%=============================================================================
fs = 8000;
y = sin(2 * pi * 440 * (0:255)' / fs);
p = audioplayer(y, fs);
asserts.isequal(p.Running, 'off');
asserts.isequal(p.CurrentSample, 0);
asserts.isequal(p.TotalSamples, numel(y));
play(p);
asserts.isequal(p.Running, 'on');
asserts.istrue(p.CurrentSample >= 0);
pause(p);
asserts.isequal(p.Running, 'off');
resume(p);
asserts.isequal(p.Running, 'on');
stop(p);
asserts.isequal(p.Running, 'off');
asserts.isequal(p.CurrentSample, 0);
delete(p);
asserts.istrue(true);
%=============================================================================

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
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
asserts.isequal(nargin('sound'), -1)
asserts.isequal(nargout('sound'), 0)
wav_file = [modulepath('audio', 'tests'), '/MacBoing.wav'];
[y, fs] = audioread(wav_file);
%=============================================================================
try
  sound(y);
catch ex
  skip_testsuite(strcmp(ex.message, 'Device unavailable'), 'Device unavailable');
end
%=============================================================================
try
  sound(y, 2 * fs);
catch ex
  skip_testsuite(strcmp(ex.message, 'Device unavailable'), 'Device unavailable');
end
%=============================================================================
nBits = 16;
try
  sound(y, fs, nBits);
catch ex
  skip_testsuite(strcmp(ex.message, 'Device unavailable'), 'Device unavailable');
end
%=============================================================================
asserts.checkerror('sound(10, 8192, 4)', _('NBITS must be 8, 16, or 24.'));
%=============================================================================
asserts.checkerror('sound(10, 8192, [20 10])', _('NBITS must be 8, 16, or 24.'));
%=============================================================================
asserts.checkerror('sound(10, [10 20 30])', _('Sample rate FS must be a positive number.'));
%=============================================================================
asserts.checkerror('sound(10, [20 10])', _('Sample rate FS must be a positive number.'));
%=============================================================================
asserts.checkerror('sound(10, [], 20)', _('Sample rate FS must be a positive number.'));
%=============================================================================
asserts.checkerror('sound(10, -10)', _('Sample rate FS must be a positive number.'));
%=============================================================================
asserts.checkerror('sound(10, 20, 30, 40, 50)', _('Too many input arguments.'));
%=============================================================================
asserts.checkerror('sound(rand(3, 3, 3))', sprintf(_('Argument #%d: Requires 2-D values only.'), 1));
%=============================================================================






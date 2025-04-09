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
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
assert_isequal(nargin('soundsc'), -1)
assert_isequal(nargout('soundsc'), 0)
wav_file = [modulepath('audio', 'tests'), '/MacBoing.wav'];
[y, fs] = audioread(wav_file);
%=============================================================================
try
  soundsc(y);
catch ex
  skip_testsuite(strcmp(ex.message, 'Device unavailable'), 'Device unavailable');
end
%=============================================================================
try
  soundsc(y, 2 * fs);
catch ex
  skip_testsuite(strcmp(ex.message, 'Device unavailable'), 'Device unavailable');
end
%=============================================================================
nBits = 16;
try
  soundsc(y, fs, nBits);
catch ex
  skip_testsuite(strcmp(ex.message, 'Device unavailable'), 'Device unavailable');
end
%=============================================================================
nBits = 16;
yRange = [-0.7,0.7];
try
  soundsc(y, fs, nBits, yRange);
catch ex
  skip_testsuite(strcmp(ex.message, 'Device unavailable'), 'Device unavailable');
end
%=============================================================================
assert_checkerror('soundsc(10, 8192, 16, [10 20 30])', _('RANGE must be a 2-element [YMIN, YMAX] vector.'));
%=============================================================================
assert_checkerror('soundsc(10, 8192, 16, [20 10])', _('RANGE must be a 2-element [YMIN, YMAX] vector.'));
%=============================================================================
assert_checkerror('soundsc(10, 8192, [10 20 30])', _('NBITS must be 8, 16, or 24.'));
%=============================================================================
assert_checkerror('soundsc(10, 8192, [20 10])', _('NBITS must be 8, 16, or 24.'));
%=============================================================================
assert_checkerror('soundsc(10, [10 20 30])', _('Sample rate FS must be a positive number.'));
%=============================================================================
assert_checkerror('soundsc(10, [20 10])', _('Sample rate FS must be a positive number.'));
%=============================================================================
assert_checkerror('soundsc(10, [], 20)',  _('Sample rate FS must be a positive number.'));
%=============================================================================
assert_checkerror('soundsc(10, -10)',  _('Sample rate FS must be a positive number.'));
%=============================================================================
assert_checkerror('soundsc(10, 20, 30, 40, 50)', _('Wrong number of input arguments.'));
%=============================================================================







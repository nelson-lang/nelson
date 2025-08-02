%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('fftw'), 2);
assert_isequal(nargout('fftw'), 1);
%=============================================================================
m = fftw('planner');
assert_isequal(m, 'estimate');
%=============================================================================
fftw('planner', 'estimate');
m = fftw('planner');
assert_isequal(m, 'estimate');
%=============================================================================
fftw('planner', 'measure');
m = fftw('planner');
assert_isequal(m, 'measure');
%=============================================================================
fftw('planner', 'patient');
m = fftw('planner');
assert_isequal(m, 'patient');
%=============================================================================
fftw('planner', 'exhaustive');
m = fftw('planner');
assert_isequal(m, 'exhaustive');
%=============================================================================
fftw('planner', 'hybrid');
m = fftw('planner');
assert_isequal(m, 'hybrid');
%=============================================================================
fftw('planner', []);
m = fftw('planner');
assert_isequal(m, 'estimate');
%=============================================================================
wd = fftw('dwisdom');
assert_istrue(strncmp(wd, '(fftw-', 6) || length(wd) == 0);
%=============================================================================
ws= fftw('swisdom');
assert_istrue(strncmp(ws, '(fftw-', 6) || length(wd) == 0);
%=============================================================================
fftw('dwisdom', []);
fftw('swisdom', []);
%=============================================================================
ws2= fftw('swisdom');
wd2= fftw('dwisdom');
assert_isequal(wd, wd2);
assert_isequal(ws, ws2);
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('fftw'), 2);
asserts.isequal(nargout('fftw'), 1);
%=============================================================================
m = fftw('planner');
asserts.isequal(m, 'estimate');
%=============================================================================
fftw('planner', 'estimate');
m = fftw('planner');
asserts.isequal(m, 'estimate');
%=============================================================================
fftw('planner', 'measure');
m = fftw('planner');
asserts.isequal(m, 'measure');
%=============================================================================
fftw('planner', 'patient');
m = fftw('planner');
asserts.isequal(m, 'patient');
%=============================================================================
fftw('planner', 'exhaustive');
m = fftw('planner');
asserts.isequal(m, 'exhaustive');
%=============================================================================
fftw('planner', 'hybrid');
m = fftw('planner');
asserts.isequal(m, 'hybrid');
%=============================================================================
fftw('planner', []);
m = fftw('planner');
asserts.isequal(m, 'estimate');
%=============================================================================
wd = fftw('dwisdom');
asserts.istrue(strncmp(wd, '(fftw-', 6) || length(wd) == 0);
%=============================================================================
ws = fftw('swisdom');
asserts.istrue(strncmp(ws, '(fftw-', 6) || length(wd) == 0);
%=============================================================================
fftw('dwisdom', []);
fftw('swisdom', []);
%=============================================================================
ws2 = fftw('swisdom');
wd2 = fftw('dwisdom');
asserts.istrue(strncmp(wd2, '(fftw-', 6) || length(wd2) == 0);
asserts.istrue(strncmp(ws2, '(fftw-', 6) || length(ws2) == 0);
%=============================================================================
